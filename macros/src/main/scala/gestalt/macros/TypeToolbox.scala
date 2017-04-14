import scala.gestalt._

object TypeToolbox {
  /** are the two types equal? */
  def =:=[A, B]: Boolean = meta {
    val tpA = toolbox.typeOf(A)
    val tpB = toolbox.typeOf(B)
    val res = toolbox.=:=(tpA, tpB)
    toolbox.Lit(res)
  }

  /** is `tp1` a subtype of `tp2` */
  def <:<[A, B]: Boolean = meta {
    val tpA = toolbox.typeOf(A)
    val tpB = toolbox.typeOf(B)
    val res = toolbox.<:<(tpA, tpB)
    toolbox.Lit(res)
  }

  /** returning a type referring to a type definition */
  def typeRef[Expected](path: String): Boolean = meta {
    val toolbox.Lit(p: String) = path
    val exp = toolbox.typeOf(Expected)
    val tp = toolbox.typeRef(p)
    val res = toolbox.=:=(tp, exp)
    // println(s"typeRef: $exp   <->  $tp")
    toolbox.Lit(res)
  }

  /** returning a type referring to a value definition */
  def termRef[Expected](path: String): Boolean = meta {
    val toolbox.Lit(p: String) = path
    val exp = toolbox.typeOf(Expected)
    val tp = toolbox.termRef(p)
    // println(s"typeRef: $exp   <->  $tp")
    val res = toolbox.=:=(tp, exp)
    toolbox.Lit(res)
  }

  /** type associated with the tree */
  def typeOf[T, Expected](a: T): Boolean = meta {
    val tp = toolbox.typeOf(a)
    val res = toolbox.=:=(tp, toolbox.typeOf(Expected))
    toolbox.Lit(res)
  }

  /** does the type refer to a case class? */
  def isCaseClass[A]: Boolean = meta {
    val tp = toolbox.typeOf(A)
    val res = toolbox.isCaseClass(tp)
    toolbox.Lit(res)
  }

  /** val fields of a case class Type -- only the ones declared in primary constructor */
  def caseFields[T]: List[String] = meta {
    val tp = toolbox.typeOf(T)
    val fieldTrees = toolbox.caseFields(tp).map(m => toolbox.Lit(toolbox.name(m)))
    q"List(..$fieldTrees)"
  }

  /** type of a member with respect to a prefix */
  def asSeenFrom[Prefix, Expected](mem: String): Boolean = meta {
    val toolbox.Lit(fd: String) = mem
    val expectedTp = toolbox.typeOf(Expected)
    val prefixTp = toolbox.typeOf(Prefix)
    val fieldTp = toolbox.asSeenFrom(toolbox.field(prefixTp, fd).get, prefixTp)
    val res = toolbox.<:<(fieldTp, expectedTp)
    toolbox.Lit(res)
  }
}
