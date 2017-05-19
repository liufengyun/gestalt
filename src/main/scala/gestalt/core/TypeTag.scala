package scala.gestalt.core

trait TypeTags { this: Toolbox =>
  class WeakTypeTag[T](val tpe: Type)
}
