import scala.gestalt.api._

object TypeToolbox {
  /** are the two types equal? */
  def =:=[A, B]: Boolean = meta {
    val res = A.tpe =:= B.tpe
    Lit(res)
  }

  /** is `tp1` a subtype of `tp2` */
  def <:<[A, B]: Boolean = meta {
    val res = A.tpe =:= B.tpe
    Lit(A.tpe <:< B.tpe)
  }

  /** returning a type referring to a type definition */
  def typeRef[Expected](path: String): Boolean = meta {
    val Lit(p: String) = path
    val exp = Expected.tpe
    val tp = Type.typeRef(p)
    val res = tp =:= exp
    // println(s"typeRef: $exp   <->  $tp")
    Lit(res)
  }

  /** returning a type referring to a value definition */
  def termRef[Expected](path: String): Boolean = meta {
    val Lit(p: String) = path
    val exp = Expected.tpe
    val tp = Type.termRef(p)
    // println(s"typeRef: $exp   <->  $tp")
    val res = tp =:= exp
    Lit(res)
  }

  /** type associated with the tree */
  def typeOf[T, Expected](a: T): Boolean = meta {
    val tp = a.tpe
    val res = tp =:= Expected.tpe
    Lit(res)
  }

  /** does the type refer to a case class? */
  def isCaseClass[A]: Boolean = meta {
    val res = A.tpe.isCaseClass
    Lit(res)
  }

  /** val fields of a case class Type -- only the ones declared in primary constructor */
  def caseFields[T]: List[String] = meta {
    val tp = T.tpe
    val fieldTrees = tp.caseFields.map(d => Lit(d.name))
    q"List(..$fieldTrees)"
  }

  def fieldType[Pre, Expected](mem: String): Boolean = meta {
    val Lit(fd: String) = mem
    val expectedTp = Expected.tpe
    val fieldTp = Pre.tpe.fieldIn(fd).get.info
    val res = fieldTp <:< expectedTp
    Lit(res)
  }

  def fieldIn[T](mem: String): String = meta {
    val Lit(fd: String) = mem
    val tp = T.tpe
    val field = tp.fieldIn(fd)
    if (field.isEmpty) Lit("")
    else Lit(field.get.name)
  }

  def fieldsIn[T]: Seq[String] = meta {
    val fields = T.tpe.fieldsIn.map(d => Lit(d.name))

    q"List(..$fields)"
  }

  def methodIn[T](mem: String): Seq[String] = meta {
    val Lit(md: String) = mem
    val tp = T.tpe
    val methods = tp.methodIn(md).map(d => Lit(d.name))

    q"List(..$methods)"
  }

  def methodsIn[T]: Seq[String] = meta {
    val tp = T.tpe
    val methods = tp.methodsIn.map(d => Lit(d.name))

    q"List(..$methods)"
  }

  def method[T](mem: String): Seq[String] = meta {
    val Lit(md: String) = mem
    val tp = T.tpe
    val methods = tp.method(md).map(d => Lit(d.name))

    q"List(..$methods)"
  }

  def methods[T]: Seq[String] = meta {
    val tp = T.tpe
    val methods = tp.methods.map(d => Lit(d.name))

    q"List(..$methods)"
  }

  def typeTag[T](x: T)(implicit m: WeakTypeTag[T]): String = meta {
    val tp = m.tpe.show
    Lit(tp)
  }

  def companion[T1, T2]: Boolean = meta {
    Lit(T1.tpe.companion.get =:= T2.tpe)
  }

  def companionName[T1]: String = meta {
    T1.tpe.companion match {
      case Some(tp) => Lit(tp.show)
      case _ => Lit("")
    }
  }
}
