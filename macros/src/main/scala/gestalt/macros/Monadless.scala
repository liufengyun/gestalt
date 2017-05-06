import scala.gestalt._

trait Monadless[Monad[_]] {

  type M[T] = Monad[T]

  /* "ghost" methods

    def apply[T](v: => T): M[T]
    def collect[T](list: List[M[T]]): M[List[T]]
    def rescue[T](m: M[T])(pf: PartialFunction[Throwable, M[T]]): M[T]
    def ensure[T](m: M[T])(f: => Unit): M[T]

  */

  def lift[T](body: T)(implicit m: toolbox.WeakTypeTag[Monad[_]]): Monad[T] = meta {
    import toolbox._

    val tree = Transformer(toolbox)(this, body)

    val unliftSym = this.tpe.method("unlift").headOption.map(_.symbol)
    def isUnlift(tp: Type) = tp.denot.map(_.symbol) == unliftSym


    toolbox.traverse(tree) {
      case q"$fun[$tp]($v)" if fun.hasType && isUnlift(fun.tpe) =>
        error("Unsupported unlift position", tree.pos)
    }

    tree
  }

  def unlift[T](m: M[T]): T = throw new Exception("Unlift must be used within a `lift` body.")
}

object Monadless {
  def apply[M[_]](): Monadless[M] = new Monadless[M] {}
}



object Transformer {

  def apply(toolbox: Toolbox)(prefix: toolbox.TermTree, tree: toolbox.TermTree)(implicit m: toolbox.WeakTypeTag[_]): toolbox.Tree = {
    import toolbox._

    val unliftSym = prefix.tpe.method("unlift").headOption.map(_.symbol)
    def isUnlift(tp: Type) = tp.denot.map(_.symbol) == unliftSym


    def toParam(name: String) = Param(emptyMods, name, None, None)
    def wrap(tree: Tree): Splice = TypedSplice(tree)

    def validate(tree: Tree): Tree = {
      traverse(tree) {
        case t @ Match(_, cases) =>
          cases.foreach {
            case Case(_, Some(t @ Transform(_)), _) =>
              abort("Unlift can't be used as a case guard.", t.pos)
            case Case(_, _, body) =>
              validate(body)
          }

        case t @ Return(_) =>
          abort("Lifted expression can't contain `return` statements.", t.pos)

        case t @ ValDef(mods, _, _, Transform(_)) if mods.isLazy =>
          abort("Unlift can't be used as a lazy val initializer.", t.pos)

        case t @ ApplySeq(method, argss) if argss.size > 0 =>
          var methodTp: MethodType = method.tpe.widen.asInstanceOf[MethodType]
          argss.map { args =>
            val paramTypes = methodTp.paramInfos
            paramTypes.zip(args).map {
              case (ByNameType(_), Transform(_)) =>
                abort("Unlift can't be used as by-name param.", t.pos)
              case other =>
                ()
            }
            methodTp.instantiate(args.map(_.tpe)) match {
              case tp: MethodType => methodTp = tp
              case _ => methodTp = null   // last param block
            }
          }
          argss.flatten.foreach(validate(_))

        case t @ DefDef(mods, _, _, paramss, _, Transform(body)) =>
          validate(body)
      }
      tree
    }

    object PureTree {
      def unapply(tree: Tree): Option[Tree] =
        exists(tree) {
          case q"$fun[$tp]($v)" if fun.hasType && isUnlift(fun.tpe) => true
        } match {
          case true  => None
          case false => Some(tree)
        }
    }

    object TransformBlock {
      def unapply(trees: List[Tree]): Option[TermTree] =
        trees match {
          case ValDef(mods, name, _, Transform(monad)) :: tail =>
            Some(Nest(monad, name, Block(tail)))

          case Transform(head) :: Nil =>
            Some(head)

          case Transform(monad) :: tail =>
            Some(Nest(monad, "_", Block(tail)))

          case head :: TransformBlock(Block(tail)) =>
            Some(Block(head +: tail))

          case other => None
        }
    }

    object Nest {
      def apply(monad: TermTree, name: String, body: TermTree): TermTree =
        body match {
          case Transform(body) =>
            val fun = Function(toParam(name) :: Nil, body)
            q"${Resolve.flatMap(monad.pos, monad)}($fun)"
          case body: TermTree =>
            val fun = Function(toParam(name) :: Nil, body)
            q"${Resolve.map(monad.pos, monad)}($fun)"
        }
    }

    object Transform {

      def apply(tree: Tree): Tree =
        unapply(tree).getOrElse(tree)

      def unapply(tree: Tree): Option[TermTree] = tree match {
        case PureTree(tree) => None

        case Ascribe(tree, _) => unapply(tree)

        case Block(trees) if trees.size > 1 => TransformBlock.unapply(trees.toList)

        case If(Transform(monad), ifTrue, ifFalse) =>
          val name = fresh()
          val body = If(Ident(name), ifTrue, ifFalse)
          Some(Nest(monad, name, body))

        case If(cond, ifTrue, Some(ifFalse)) =>
          (ifTrue, ifFalse) match {
            case (Transform(ifTrue), Transform(ifFalse)) =>
              Some(If(cond, ifTrue, Some(ifFalse)))
            case (Transform(ifTrue), ifFalse) =>
              val elsep = Apply(Resolve.apply(tree.pos), List(ifFalse))
              Some(If(cond, ifTrue, Some(elsep)))
            case (ifTrue, Some(Transform(ifFalse))) =>
              val ifp = Apply(Resolve.apply(tree.pos), List(ifTrue))
              Some(If(cond, ifp, Some(ifFalse)))
            case (ifTrue, ifFalse) =>
              None
          }


        case q"$fun[$tp]($v)" if isUnlift(fun.tpe) => Some(v)

        case tree: Tree =>
          val unlifts = collection.mutable.ListBuffer.empty[(TermTree, String, TypeTree)]
          val newTree: TermTree =
            transform(tree) {
              case q"$fun[$tp]($v)" if isUnlift(fun.tpe) =>
                val name = fresh()
                val splice = wrap(tp)
                unlifts += ((v, name, splice))
                Ident(name)
            }.asInstanceOf[TermTree]

          unlifts.toList match {
            case List() => None
            case List((tree, name, _)) =>
              val fun = Function(toParam(name) :: Nil, newTree)
              Some(q"${Resolve.map(tree.pos, tree)}($fun)")
            case unlifts =>
              val (trees, names, types) = unlifts.unzip3
              val list = fresh("list")
              val iterator = fresh("iterator")
              val collect = q"${Resolve.collect(tree.pos)}(scala.List(..${trees.toSeq}))"

              val elements = unlifts.map {
                case (tree, name, tpe) =>
                  q"val $name = ${Ident(iterator)}.next().asInstanceOf[$tpe]"
              }

              val iteratorDef = q"val $iterator = ${Ident(list)}.iterator"
              val body = Block(iteratorDef +: elements :+ newTree)

              val fun = Function(toParam(list) :: Nil, body)
              Some(q"${Resolve.map(tree.pos, collect)}($fun)")
          }
      }
    }

    object Resolve {

      private val monadTypeName = m.tpe.show
      private val sourceCompatibilityMessage =
        s"""For instance, it's possible to add implicits or default parameters to the method
           |without breaking source compatibility.
           |Note: the methods defined by the `Monadless` instance have precedence over the ones
           |defined by the monad instance and its companion object.
        """.stripMargin

      def apply(pos: Pos): TermTree =
        companionMethod(pos, "apply").getOrElse {
          val msg =
            s"""Transformation requires the method `apply` to create a monad instance for a value.
               |${companionMethodErrorMessage(s"def apply[T](v: => T): $monadTypeName[T]")}
            """.stripMargin
          abort(msg, pos)
        }

      def collect(pos: Pos): TermTree =
        companionMethod(pos, "collect").getOrElse {
          val msg =
            s"""Transformation requires the method `collect` to transform List[M[T]] into M[List[T]]. The implementation
               |is free to collect the results sequentially or in parallel.
               |${companionMethodErrorMessage(s"def collect[T](list: List[$monadTypeName[T]]): $monadTypeName[List[T]]")}
            """.stripMargin
          abort(msg, pos)
        }

      private def companionMethodErrorMessage(signature: String) =
        s"""Please add the method to `${m.tpe}`'s companion object or to `${prefix}`.
           |It needs to be source compatible with the following signature:
           |`$signature`
           |$sourceCompatibilityMessage
        """.stripMargin

      def map(pos: Pos, instance: TermTree): TermTree =
        instanceMethod(pos, instance, "map").getOrElse {
          val msg =
            s"""Transformation requires the method `map` to transform the result of a monad instance.
               |${instanceMethodErrorMessage("map[T, U]", s"f: T => U", s"$monadTypeName[U]")}
            """.stripMargin
          abort(msg, pos)
        }

      def flatMap(pos: Pos, instance: TermTree): TermTree =
        instanceMethod(pos, instance, "flatMap").getOrElse {
          val msg =
            s"""Transformation requires the method `flatMap` to transform the result of a monad instance.
               |${instanceMethodErrorMessage("flatMap[T, U]", s"f: T => $monadTypeName[U]", s"$monadTypeName[U]")}
            """.stripMargin
          abort(msg, pos)
        }

      def rescue(pos: Pos, instance: TermTree): TermTree =
        instanceMethod(pos, instance, "rescue").getOrElse {
          val msg =
            s"""Transformation requires the method `rescue` to recover from a failure (translate a `catch` clause).
               |${instanceMethodErrorMessage("collect[T]", s"pf: PartialFunction[Throwable, $monadTypeName[T]]", s"$monadTypeName[T]")}
               |$errorHandlingMonadNote
            """.stripMargin
          abort(msg, pos)
        }

      def ensure(pos: Pos, instance: TermTree): TermTree =
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
        s"""Please add the method to `${m.tpe}` or to `$prefix`.
           |It needs to be source compatible with the following signature:
           |As a `${m.tpe}` method: `def $name($parameter): $result`
           |As a `$prefix` method: `def $name[T](m: $monadTypeName[T])($parameter): $result`
           |$sourceCompatibilityMessage
        """.stripMargin

      private val errorHandlingMonadNote =
        """Note that this kind of construct (`try`/`catch`/`finally`) can't be used with monads
          |that don't represent a computation and/or don't handle exceptions (e.g. `Option`)
        """.stripMargin

      private def instanceMethod(pos: Pos, instance: TermTree, name: String) =
        this.method(prefix, prefix.tpe, name).map(t => q"$t($instance)")
          .orElse(this.method(instance, m.tpe, name))

      private def companionMethod(pos: Pos, name: String) =
        method(prefix, prefix.tpe, name)
          .orElse(method(Ident(m.tpe.denot.get.name), m.tpe.companion.get, name))

      private def method(instance: TermTree, tpe: Type, name: String) =
        find(tpe, name).map(_ => Select(instance, name))

      private def find(tpe: Type, method: String) =
        tpe.method(method) match {
          case head :: tail => Some(head)
          case Nil => None
        }
    }

    validate(tree) match {
      case PureTree(tree: TermTree) => Apply(Resolve.apply(tree.pos), List(tree))
      case tree: Tree     => Transform(tree)
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
