import scala.gestalt.api._

final class Optional[+A >: Null](val value: A) extends AnyVal {
  def get: A = value
  def isEmpty = value == null

  def getOrElse[B >: A](alt: => B): B = meta {
    val tempValDef = ValDef(prefix)
    val tempIdent = Ident(tempValDef.symbol)

    q"""
       $tempValDef
       if ($tempIdent.isEmpty) $alt else $tempIdent.value
    """
  }

  def map[B >: Null](f: A => B): Optional[B] = meta {
    val Function(param :: Nil, body) = f
    val tempValDef = ValDef(prefix)
    val tempIdent = Ident(tempValDef.symbol)

    val newBody = body.transform {
      case Ident(sym) if sym eq param =>
        Select(tempIdent, "value")
    }

    q"""
       $tempValDef
       if ($tempIdent.isEmpty) new _empty_.Optional(null)
       else new _empty_.Optional(${newBody.wrap})
     """
  }

  override def toString = if (isEmpty) "<empty>" else s"$value"
}
