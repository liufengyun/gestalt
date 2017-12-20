package scala.gestalt.dotty

import dotty.tools.dotc._
import ast.{Trees => t, tpd, untpd}
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
      fullName(enclosingPackage) + "." + classSymbol.flatName
    }
  }

  /** Expand annotation macros */
  def expandAnnotMacro(mdef: untpd.MemberDef)(implicit ctx: Context): untpd.Tree = {
    val ann = mdef.mods.annotations.filter(macros.isAnnotMacro).headOption
    val expansion = ann.flatMap {
      case ann @ t.Apply(t.Select(t.New(tpt), init), _) =>
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

        gestalt.withToolbox(new Toolbox(ann.pos)) {
          val result = impl.invoke(null, ann, expandee, ctx).asInstanceOf[untpd.Tree]
          Some(result)
        }
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
    argss.foldLeft(Nil : List[List[Object]]) { (acc, args: List[tpd.Tree]) =>
      val paramTypes = methodType.paramInfos
      val args2 = paramTypes.zip(args).map {
        case (AppliedType(tp, targs), arg) if tp.isRef(defn.WeakTypeTag) =>
          Inferencing.isFullyDefined(targs(0), typer.ForceDegree.noBottom)
          new gestalt.WeakTypeTag[Nothing](targs(0).stripTypeVar.asInstanceOf[gestalt.Type])
        case (_, arg) => arg
      }

      methodType.instantiate(args.map(_.tpe)) match {
        case tp: MethodType => methodType = tp
        case _ => methodType = null             // last param section
      }

      acc :+ args2
    }
  }

  /** Duplicate Symbol.showFullName, as it is prone to compiler args changes */
  private def fullName(sym: Symbol)(implicit ctx: Context): String = {
    if (sym.isRoot || sym == NoSymbol || sym.owner.isEffectiveRoot)
      sym.name.toString
    else
      fullName(sym.effectiveOwner.enclosingClass) + "." + sym.name.toString
  }

  /** Expand def macros */
  def expandDefMacro(tree: tpd.Tree)(implicit ctx: Context): untpd.Tree = tree match {
    case ExtractApply(methodSelect @ MethodSelect(prefix, method), targs, argss) =>
      println("expanding \n" + tree.show)
      val methodOwner = methodSelect.symbol.owner
      val className = if (methodOwner.isPackageObject) {
        // if macro is defined in a package object
        // the implementation is located relative to the package not the `package$` module
        fullName(methodOwner.owner) + "$" + "$inline"
      } else {
        javaClassName(methodOwner) + "$inline"
      }

      // reflect macros definition
      val moduleClass = ctx.classloader.loadClass(className)
      val impl = moduleClass.getDeclaredMethods().find(_.getName == method.encode.show).get
      impl.setAccessible(true)

      val tb = new Toolbox(tree.pos)
      val argss2 = synthesizeTypeTag(methodSelect, targs, argss)(tb)
      val prefix2 =
        if (prefix == null) tpd.ref(methodSelect.tpe.asInstanceOf[TermRef].prefix.asInstanceOf[NamedType])
        else prefix
      val trees  = (prefix2 :: targs ++ argss2.flatten) :+ ctx
      try {
        val res = gestalt.withToolbox(tb) {
          impl.invoke(null, trees: _*).asInstanceOf[untpd.Tree]
        }
        println(" => \n" + res.show)
        res
      }
      catch {
        case e: Exception =>
          ctx.error("error occurred while expanding macro: \n" + e.getMessage, tree.pos)
          e.printStackTrace()
          untpd.Literal(Constant(null)).withPos(tree.pos)
      }
    case _ =>
      ctx.warning(s"Unknown macro expansion: $tree")
      tree
  }
}
