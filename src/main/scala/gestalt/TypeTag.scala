package scala.gestalt

trait TypeTags { this: Toolbox =>
  class WeakTypeTag[T](val tpe: Type)
}
