import scala.gestalt.api._

import scala.annotation.unchecked.{ uncheckedVariance => uncheckVar }

final class Optional[+A >: Null](val value: A) extends AnyVal {
  def get: A = value
  def isEmpty = value == null

  def getOrElse[B >: A](alt: => B)(implicit m: WeakTypeTag[A] @uncheckVar): B = meta {
    import scala.gestalt.options.unsafe

    val temp = fresh("_temp")
    val tempIdent = Ident(temp)

    q"""
      val $temp = $prefix
      if ($tempIdent.isEmpty) $alt else $tempIdent.value
    """
  }

  def map[B >: Null](f: A => B)(implicit m: WeakTypeTag[A] @uncheckVar): Optional[B] = meta {
    val Function(param :: Nil, body) = f
    val tempValDef = ValDef(fresh("_temp"), prefix)
    val tempIdent = Ident(tempValDef.symbol)

    val newBody = body.transform {
      case id @ Ident(_) if id.symbol.get eq param =>
        Select(tempIdent, "value")
    }

    {
      import scala.gestalt.options.unsafe
      q"""
       $tempValDef
       if ($tempIdent.isEmpty) new Optional(null)
       else new Optional(${newBody.wrap})
     """
    }
  }

  override def toString = if (isEmpty) "<empty>" else s"$value"
}
