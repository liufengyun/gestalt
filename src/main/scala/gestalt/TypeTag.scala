package scala.gestalt

trait TypeTags { this: Toolbox =>
  @annotation.implicitNotFound(msg = "No WeakTypeTag available for ${T}")
  trait WeakTypeTag[T] extends Equals with Serializable {
    /**
     * Reflective representation of type T.
     */
    def tpe: Type
  }

  @annotation.implicitNotFound(msg = "No TypeTag available for ${T}")
  trait TypeTag[T] extends WeakTypeTag[T] with Equals with Serializable {

  }

  /**
   * Shortcut for `implicitly[WeakTypeTag[T]].tpe`
   * @group TypeTags
   */
  def weakTypeOf[T](implicit attag: WeakTypeTag[T]): Type = attag.tpe

  /**
   * Shortcut for `implicitly[TypeTag[T]].tpe`
   * @group TypeTags
   */
  def typeOf[T](implicit ttag: TypeTag[T]): Type = ttag.tpe
}
