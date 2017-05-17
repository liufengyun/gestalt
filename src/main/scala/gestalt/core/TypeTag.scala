package scala.gestalt
package core

trait TypeTags { this: Toolbox =>
  class WeakTypeTag[T](val tpe: Type)
}
