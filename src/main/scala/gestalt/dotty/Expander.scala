package scala.gestalt.dotty

import dotty.tools.dotc._
import ast.Trees._
import ast.{tpd, untpd}
import ast.untpd.modsDeco
import typer.Inferencing
import core.StdNames._
import core.Contexts._
import core.Symbols._
import core.Names._
import core.Decorators._
import core.Constants._
import core.Types._
import core.TypeApplications._

import scala.gestalt._

object Expander {
  private object ExtractApply {
    def unapply(tree: tpd.Tree): Option[(tpd.Tree, List[tpd.Tree], List[List[tpd.Tree]])] = tree match {
      case tree: tpd.TypeApply =>
        Some((tree.fun, tree.args, Nil))
      case tree: tpd.Apply =>
        val Some((f, targs, argss)) = unapply(tree.fun)
        Some((f, targs, argss :+ tree.args))
      case _ =>
        Some((tree, Nil, Nil))
    }
  }

  private object MethodSelect {
    def unapply(tree: tpd.Tree): Option[(tpd.Tree, Name)] = tree match {
      case tree: tpd.Select => Some((tree.qualifier, tree.name))
      case tree: tpd.Ident => Some((null, tree.name))
      case _ => None
    }
  }

  private def javaClassName(classSymbol: Symbol)(implicit ctx: Context): String = {
    val enclosingPackage = classSymbol.enclosingPackageClass
    if (enclosingPackage.isEffectiveRoot) {
      classSymbol.flatName.toString
    } else {
      enclosingPackage.showFullName + "." + classSymbol.flatName
    }
  }

  def expandQuasiquote(tree: untpd.Tree, isTerm: Boolean)(implicit ctx: Context): untpd.Tree = {
    val (tag, parts, args) = tree match {
      case Apply(Select(Apply(Ident(nme.StringContext), parts), name), args) =>
        (name.toString, parts, args)
      case UnApply(Select(Select(Apply(Select(Ident(nme.StringContext), nme.apply), List(Typed(SeqLiteral(parts, _), _))), name), nme.unapply), _, pats) =>
        (name.toString, parts, pats)
    }
    val strs = for(Literal(Constant(v: String)) <- parts) yield v
    expand(new Toolbox(tree.pos))(tag, tree, strs, args, !isTerm)
  }

  /** Expand annotation macros */
  def expandAnnotMacro(mdef: untpd.MemberDef)(implicit ctx: Context): untpd.Tree = {
    val ann = mdef.mods.annotations.filter(macros.isAnnotMacro).headOption
    val expansion = ann.flatMap {
      case ann @ Apply(Select(New(tpt), init), _) =>
        val tpdClass = ctx.typer.typedAheadType(tpt)

        val className = tpdClass.symbol.fullName + "$inline"
        // reflect macros definition
        val moduleClass = ctx.classloader.loadClass(className)
        val impl = moduleClass.getDeclaredMethods().find(_.getName == "apply").get
        impl.setAccessible(true)

        val expandee = {
          val mods1 = mdef.mods.withAnnotations(mdef.mods.annotations.filter(_ ne ann))
          mdef.withMods(mods1)
        }
        val result = impl.invoke(null, new Toolbox(ann.pos), ann, expandee).asInstanceOf[untpd.Tree]
        Some(result)
      case _ =>
        None
    }

    expansion.getOrElse(mdef)
  }

  private def synthesizeTypeTag(f: tpd.Tree, targs: List[tpd.Tree], argss: List[List[tpd.Tree]])
                               (tb: Toolbox)(implicit ctx: Context): List[List[Object]]
  =
  {
    if (argss.size == 0 || (argss.size == 1 && argss.head.size == 0)) return argss

    var methodType = f.tpe.widen.appliedTo(targs.map(_.tpe)).asInstanceOf[MethodType]
    argss.foldRight(Nil : List[List[Object]]) { (args: List[tpd.Tree], acc) =>
      val paramTypes = methodType.paramInfos
      val args2 = paramTypes.zip(args).map {
        case (AppliedType(tp, targs), arg) if tp.isRef(defn.WeakTypeTag) =>
          Inferencing.isFullyDefined(targs(0), typer.ForceDegree.noBottom)
          new tb.WeakTypeTag[Nothing](targs(0).stripTypeVar)
        case (_, arg) => arg
      }

      methodType.instantiate(args.map(_.tpe)) match {
        case tp: MethodType => methodType = tp
        case _ => methodType = null             // last param section
      }

      acc :+ args2
    }
  }

  /** Expand def macros */
  def expandDefMacro(tree: tpd.Tree)(implicit ctx: Context): untpd.Tree = tree match {
    case ExtractApply(methodSelect @ MethodSelect(prefix, method), targs, argss) =>
      val methodOwner = methodSelect.symbol.owner
      val className = if (methodOwner.isPackageObject) {
        // if macro is defined in a package object
        // the implementation is located relative to the package not the `package$` module
        methodOwner.owner.showFullName + "$" + "$inline"
      } else {
        javaClassName(methodOwner) + "$inline"
      }
      // reflect macros definition
      val moduleClass = ctx.classloader.loadClass(className)
      val impl = moduleClass.getDeclaredMethods().find(_.getName == method.toString).get
      impl.setAccessible(true)

      val tb = new Toolbox(tree.pos)
      val argss2 = synthesizeTypeTag(methodSelect, targs, argss)(tb)
      val trees  = tb :: prefix :: targs ++ argss2.flatten
      try {
        impl.invoke(null, trees: _*).asInstanceOf[untpd.Tree]
      }
      catch {
        case e: Exception =>
          ctx.error("error occurred while expanding macro: \n" + e.getMessage, tree.pos)
          untpd.Literal(Constant(null)).withPos(tree.pos)
      }
    case _ =>
      ctx.warning(s"Unknown macro expansion: $tree")
      tree
  }
}
