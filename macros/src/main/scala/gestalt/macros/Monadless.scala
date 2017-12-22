import scala.gestalt._
import quasiquotes._
import tpd._

trait Monadless[Monad[_]] {

  type M[T] = Monad[T]

  /* "ghost" methods

    def apply[T](v: => T): M[T]
    def collect[T](list: List[M[T]]): M[List[T]]
    def rescue[T](m: M[T])(pf: PartialFunction[Throwable, M[T]]): M[T]
    def ensure[T](m: M[T])(f: => Unit): M[T]

  */

  def lift[T](body: T)(implicit m: WeakTypeTag[Monad[_]]): Monad[T] = meta {
    val tree = Transformer(this, body, m.tpe)

    val unliftSym = this.tpe.method("unlift").headOption.map(_.symbol)
    def isUnlift(tp: Type) = tp.denot.map(_.symbol) == unliftSym

    tree
  }

  def unlift[T](m: M[T]): T = throw new Exception("Unlift must be used within a `lift` body.")
}

object Monadless {
  def apply[M[_]](): Monadless[M] = new Monadless[M] {}
}



object Transformer {

  def apply(prefix: tpd.Tree, tree: tpd.Tree, monadType: Type): untpd.Tree = {
    val unliftSym = prefix.tpe.method("unlift").headOption.map(_.symbol)
    def isUnlift(tp: Type) = tp.denot.map(_.symbol) == unliftSym

    def rewrite(monad: tpd.Tree, name: String, tp: Type, resTp: Type, flat: Boolean, flatTp: Type = null)
               (bodyFn: Seq[tpd.RefTree] => tpd.Tree): tpd.Tree =
    {
      val fun = Function(tp :: Nil, resTp)(bodyFn)
      if (flat)
        Resolve.flatMap(monad.pos, monad).appliedToTypes(flatTp :: Nil).appliedTo(fun :: Nil)
      else
        Resolve.map(monad.pos, monad).appliedToTypes(resTp :: Nil).appliedTo(fun :: Nil)
    }

    def validate(tree: tpd.Tree): Unit = {
      tree.traverse {
        case t @ Match(_, cases) =>
          cases.foreach {
            case Case(_, Some(t @ Transform(_)), _) =>
              abort("Unlift can't be used as a case guard.", t.pos)
            case Case(_, _, body) =>
              validate(body)
          }

        case t @ Return(_) =>
          abort("Lifted expression can't contain `return` statements.", t.pos)

        // case t @ ValDef(_, _, Transform(_)) if mods.isLazy =>
        //  abort("Unlift can't be used as a lazy val initializer.", t.pos)

        case t @ ApplySeq(method, argss) if argss.size > 0 =>
          var methodTp: Type.MethodType = method.tpe.widen.asInstanceOf[Type.MethodType]
          argss.foreach { args =>
            val paramTypes = methodTp.paramInfos
            paramTypes.zip(args).map {
              case (Type.ByNameType(_), Transform(_)) =>
                abort("Unlift can't be used as by-name param.", t.pos)
              case other =>
                ()
            }
            methodTp.instantiate(args.map(_.tpe)) match {
              case tp: Type.MethodType => methodTp = tp
              case _ => methodTp = null   // last param block
            }
          }
          argss.foreach(_.foreach(validate(_)))
      }
      tree
    }

    object PureTree {
      def unapply(tree: tpd.Tree): Option[tpd.Tree] = {
        tree.exists {
          case ApplySeq(ApplyType(fun, tp :: Nil), (v :: Nil) :: Nil) if isUnlift(fun.tpe) => true
        } match {
          case true => None
          case false => Some(tree)
        }
      }
    }

    object TransformBlock {
      def unapply(trees: List[tpd.Tree])(implicit blockTp: Type): Option[tpd.Tree] =
        trees match {
          case (tree @ ValDef(sym, Transform(monad))) :: TransformBlock(body) =>
            val res = rewrite(monad, sym.name, tree.tpe.widen, body.tpe, flat = true, flatTp = blockTp) { refs =>
              body.subst(sym :: Nil, refs.head.symbol :: Nil)
            }
            Some(res)

          case (tree @ ValDef(sym, Transform(monad))) :: tail =>       // tail cannot be empty
            Some(rewrite(monad, sym.name, tree.tpe.widen, blockTp, flat = false) { refs =>
              Block(tail.init, tail.last).subst(sym :: Nil, refs.head.symbol :: Nil)
            })

          case Transform(head) :: Nil =>
            Some(head)

          case (tree @ Transform(monad)) :: TransformBlock(body) =>
            Some(rewrite(monad, fresh(), tree.tpe, body.tpe, flat = true, flatTp = blockTp) { refs =>
              body
            })

          case (tree @ Transform(monad)) :: tail =>
            Some(rewrite(monad, fresh(), tree.tpe, blockTp, flat = false) { refs =>
              Block(tail.init, tail.last)
            })

          case head :: TransformBlock(Block(stats, expr)) =>
            Some(Block(head +: stats, expr))

          case other => None
        }
    }

    object TransformIf {
      def unapply(tree: tpd.Tree): Option[tpd.Tree] = tree match {
        case tree @ If(Transform(monad), ifTrue, ifFalse) =>
          unapply(ifTrue, ifFalse) match {
            case Some(ifTrue, ifFalse) =>
              val resTp = Type.lub(ifTrue.tpe, ifFalse.tpe)
              val res = rewrite(monad, fresh(), Type.typeRef("scala.Boolean"), resTp, flat = true, flatTp = tree.tpe) { refs =>
                val ident = refs.head
                If(ident, ifTrue, ifFalse)
              }
              Some(res)
            case None =>
              val res = rewrite(monad, fresh(), Type.typeRef("scala.Boolean"), tree.tpe, flat = false) { refs =>
                val ident = refs.head
                If(ident, ifTrue, ifFalse)
              }
              Some(res)
          }

        case If(cond, ifTrue, ifFalse) =>
          unapply(ifTrue, ifFalse) match {
            case Some(ifTrue, ifFalse) =>
              Some(If(cond, ifTrue, ifFalse))
            case None =>
              Some(If(cond, ifTrue, ifFalse))
          }

        case _ => None
      }

      def unapply(ifTrue: tpd.Tree, ifFalse: tpd.Tree): Option[(tpd.Tree, tpd.Tree)] =
        (ifTrue, ifFalse) match {
          case (Transform(ifTrue), Transform(ifFalse)) =>
            Some((ifTrue, ifFalse))
          case (Transform(ifTrue), ifFalse) =>
            val elsep = Apply(Resolve.apply(tree.pos), List(ifFalse))
            Some((ifTrue, elsep))
          case (ifTrue, Transform(ifFalse)) =>
            val ifp = Apply(Resolve.apply(tree.pos), List(ifTrue))
            Some((ifp, ifFalse))
          case (ifTrue, ifFalse) =>
            None
        }
    }

    object Transform {

      def apply(tree: tpd.Tree): tpd.Tree =
        unapply(tree).getOrElse(tree)

      def unapply(tree: tpd.Tree): Option[tpd.Tree] = tree match {
        case PureTree(tree) => None

        case Ascribe(tree, _) => unapply(tree)

        case block @ Block(stats, expr) => TransformBlock.unapply(stats.toList :+ expr)(block.tpe)

        case If(cond, ifTrue, ifFalse) => TransformIf.unapply(tree)

        case ApplySeq(ApplyType(fun, tp :: Nil), (v :: Nil) :: Nil) if isUnlift(fun.tpe) => Some(v)

        case tree =>
          val unlifts = collection.mutable.ListBuffer.empty[(tpd.Tree, Symbol, Type)]
          val newTree =
            tree.transform {
              case tree @ ApplySeq(ApplyType(fun, tp :: Nil), (v :: Nil) :: Nil) if isUnlift(fun.tpe) =>
                val dummy = ValDef(tree).symbol
                unlifts += ((v, dummy, tp))
                Ident(dummy)
            }

          unlifts.toList match {
            case List() => None
            case List((tree, dummy, tpe)) =>
              val res = rewrite(tree, dummy.name, tpe, newTree.tpe, flat = false) { refs =>
                newTree.subst(dummy :: Nil, refs.head.symbol :: Nil)
              }

              Some(res)
            case unlifts =>
              val (trees, dummies, types) = unlifts.unzip3
              val list = fresh("list")
              val scalaList = Term("scala.collection.immutable.List")
              val arg = scalaList.appliedTo(trees)
              val collect = Resolve.collect(tree.pos).appliedTo(arg :: Nil)


              val tp = Type.typeRef("scala.List").appliedTo(types.head)
              val fun = Function(tp :: Nil, newTree.tpe) { refs =>
                val iter = ValDef(refs.head.select("iterator"))
                val elements = unlifts.map { case (tree, dummy, tpe) =>
                  val ident = Ident(iter.symbol)
                  val rhs = tq"$ident.next().asInstanceOf[$tpe]"
                  ValDef(rhs)
                }

                val froms   = dummies
                val tos     = elements.map(_.symbol)
                val content = newTree.subst(froms, tos)

                Block(iter +: elements, content)
              }
              Some(Resolve.map(tree.pos, collect).appliedToTypes(types.head :: Nil).appliedTo(fun :: Nil))
          }
      }
    }

    object Resolve {

      private val monadTypeName = monadType.show
      private val sourceCompatibilityMessage =
        s"""For instance, it's possible to add implicits or default parameters to the method
           |without breaking source compatibility.
           |Note: the methods defined by the `Monadless` instance have precedence over the ones
           |defined by the monad instance and its companion object.
        """.stripMargin

      def apply(pos: Position): tpd.Tree =
        companionMethod(pos, "apply").getOrElse {
          val msg =
            s"""Transformation requires the method `apply` to create a monad instance for a value.
               |${companionMethodErrorMessage(s"def apply[T](v: => T): $monadTypeName[T]")}
            """.stripMargin
          abort(msg, pos)
        }

      def collect(pos: Position): tpd.Tree =
        companionMethod(pos, "collect").getOrElse {
          val msg =
            s"""Transformation requires the method `collect` to transform List[M[T]] into M[List[T]]. The implementation
               |is free to collect the results sequentially or in parallel.
               |${companionMethodErrorMessage(s"def collect[T](list: List[$monadTypeName[T]]): $monadTypeName[List[T]]")}
            """.stripMargin
          abort(msg, pos)
        }

      private def companionMethodErrorMessage(signature: String) =
        s"""Please add the method to `${monadType}`'s companion object or to `${prefix}`.
           |It needs to be source compatible with the following signature:
           |`$signature`
           |$sourceCompatibilityMessage
        """.stripMargin

      def map(pos: Position, instance: tpd.Tree): tpd.Tree =
        instanceMethod(pos, instance, "map").getOrElse {
          val msg =
            s"""Transformation requires the method `map` to transform the result of a monad instance.
               |${instanceMethodErrorMessage("map[T, U]", s"f: T => U", s"$monadTypeName[U]")}
            """.stripMargin
          abort(msg, pos)
        }

      def flatMap(pos: Position, instance: tpd.Tree): tpd.Tree =
        instanceMethod(pos, instance, "flatMap").getOrElse {
          val msg =
            s"""Transformation requires the method `flatMap` to transform the result of a monad instance.
               |${instanceMethodErrorMessage("flatMap[T, U]", s"f: T => $monadTypeName[U]", s"$monadTypeName[U]")}
            """.stripMargin
          abort(msg, pos)
        }

      def rescue(pos: Position, instance: tpd.Tree): tpd.Tree =
        instanceMethod(pos, instance, "rescue").getOrElse {
          val msg =
            s"""Transformation requires the method `rescue` to recover from a failure (translate a `catch` clause).
               |${instanceMethodErrorMessage("collect[T]", s"pf: PartialFunction[Throwable, $monadTypeName[T]]", s"$monadTypeName[T]")}
               |$errorHandlingMonadNote
            """.stripMargin
          abort(msg, pos)
        }

      def ensure(pos: Position, instance: tpd.Tree): tpd.Tree =
        instanceMethod(pos, instance, "ensure").getOrElse {
          val msg =
            s"""Transformation requires the method `ensure` to execute code regardless of the outcome of the
               |execution (translate a `finally` clause).
               |${instanceMethodErrorMessage("rescue[T]", s"f: => Unit", s"$monadTypeName[T]")}
               |$errorHandlingMonadNote
            """.stripMargin
          abort(msg, pos)
        }

      private def instanceMethodErrorMessage(name: String, parameter: String, result: String) =
        s"""Please add the method to `${monadType}` or to `$prefix`.
           |It needs to be source compatible with the following signature:
           |As a `${monadType}` method: `def $name($parameter): $result`
           |As a `$prefix` method: `def $name[T](m: $monadTypeName[T])($parameter): $result`
           |$sourceCompatibilityMessage
        """.stripMargin

      private val errorHandlingMonadNote =
        """Note that this kind of construct (`try`/`catch`/`finally`) can't be used with monads
          |that don't represent a computation and/or don't handle exceptions (e.g. `Option`)
        """.stripMargin

      private def instanceMethod(pos: Position, instance: tpd.Tree, name: String): Option[tpd.Tree] =
        this.method(prefix, prefix.tpe, name).map(t => tq"$t($instance)")
          .orElse(this.method(instance, monadType, name))

      private def companionMethod(pos: Position, name: String) =
        method(prefix, prefix.tpe, name)
          .orElse(method(Ident(monadType.companion.get.termSymbol.get), monadType.companion.get, name))

      private def method(instance: tpd.Tree, tpe: Type, name: String): Option[tpd.Tree] =
        find(tpe, name).map(_ => Select(instance, name))

      private def find(tpe: Type, method: String): Option[Denotation] =
        tpe.method(method) match {
          case head :: tail => Some(head)
          case Nil => None
        }
    }

    validate(tree)

    tree match {
      case PureTree(tree: tpd.Tree) => untpd.Apply(Resolve.apply(tree.pos), List(tree))
      case tree => Transform(tree)
    }
  }
}


trait MonadlessOption extends Monadless[Option] {

  def collect[T](list: List[Option[T]]): Option[List[T]] =
    list.foldLeft(Option(List.empty[T])) {
      (acc, item) =>
        for {
          l <- acc
          i <- item
        } yield l :+ i
    }
}

object MonadlessOption extends MonadlessOption
