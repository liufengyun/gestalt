import scala.gestalt._

import scala.annotation.unchecked.{ uncheckedVariance => uncheckVar }

final class Optional[+A >: Null](val value: A) extends AnyVal {
  def get: A = value
  def isEmpty = value == null

  def getOrElse[B >: A](alt: => B)(implicit m: toolbox.WeakTypeTag[A] @uncheckVar): B = meta {
    import toolbox._

    val temp = toolbox.fresh("_temp")
    val tempIdent = Ident(temp)

    q"""
      val $temp = $prefix
      if ($tempIdent.isEmpty) $alt else $tempIdent.value
    """
  }

  def map[B >: Null](f: A => B)(implicit m: toolbox.WeakTypeTag[A] @uncheckVar): Optional[B] = meta {
    import toolbox._

    val Function(param :: Nil, body) = f
    val tempValDef = ValDef(toolbox.fresh("_temp"), prefix)
    val tempIdent = Ident(tempValDef.symbol)

    val newBody = transform(body) {
      case id @ Ident(_) if id.symbol.get eq param =>
        Select(tempIdent, "value")
    }

    q"""
       $tempValDef
       if ($tempIdent.isEmpty) new Optional(null)
       else new Optional($newBody)
     """
  }

  override def toString = if (isEmpty) "<empty>" else s"$value"
}