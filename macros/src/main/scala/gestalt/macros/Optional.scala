import scala.gestalt._

final class Optional[+A >: Null](val value: A) extends AnyVal {
  def get: A = value
  def isEmpty = value == null

  def getOrElse[B >: A](alt: => B): B = meta {
    val tempValDef = tpd.ValDef(prefix)
    val tempIdent = tpd.Ident(tempValDef.symbol)

    q"""
       $tempValDef
       if ($tempIdent.isEmpty) $alt else $tempIdent.value
    """
  }

  def map[B >: Null](f: A => B): Optional[B] = meta {
    val tpd.Function(param :: Nil, body) = f
    val tempValDef = tpd.ValDef(prefix)
    val tempIdent = tpd.Ident(tempValDef.symbol)

    val newBody = body.transform {
      case tpd.Ident(sym) if sym eq param =>
        tpd.Select(tempIdent, "value")
    }

    q"""
       $tempValDef
       if ($tempIdent.isEmpty) new _empty_.Optional(null)
       else new _empty_.Optional($newBody)
     """
  }

  override def toString = if (isEmpty) "<empty>" else s"$value"
}
