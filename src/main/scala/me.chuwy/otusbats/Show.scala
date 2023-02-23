package me.chuwy.otusbats


trait Show[A] {
  def show(a: A): String
}

object Show {

  // 1.1 Instances (`Int`, `String`, `Boolean`)
  implicit val intShow: Show[Int] = new Show[Int] {
    override def show(a: Int): String = a.toString
  }

  implicit val stringShow: Show[String] = new Show[String] {
    override def show(a: String): String = a
  }

  implicit val booleanShow: Show[Boolean] = new Show[Boolean] {
    override def show(a: Boolean): String = a.toString
  }
  // 1.2 Instances with conditional implicit

  implicit def listShow[A](implicit ev: Show[A]): Show[List[A]] = new Show[List[A]] {
    override def show(a: List[A]): String = a.map(e => ev.show(e)).toString()
  }


  // 2. Summoner (apply)
  def apply[A](implicit ev: Show[A]): Show[A] = ev

  // 3. Syntax extensions

  implicit class ShowOps[A](a: A) {
    def show(implicit ev: Show[A]): String =
      ev.show(a)

    def mkString_[B](begin: String, end: String, separator: String)(implicit S: Show[B], ev: A <:< List[B]): String = {
      // with `<:<` evidence `isInstanceOf` is safe!
      val casted: List[B] = a.asInstanceOf[List[B]]
      Show.mkString_(casted, separator, begin, end)
    }

  }

  /** Transform list of `A` into `String` with custom separator, beginning and ending.
   *  For example: "[a, b, c]" from `List("a", "b", "c")`
   *
   *  @param separator. ',' in above example
   *  @param begin. '[' in above example
   *  @param end. ']' in above example
   */
  def mkString_[A: Show](list: List[A], separator: String, begin: String, end: String): String = {
    val ev = implicitly[Show[A]]
    begin + list
      .map(e => ev.show(e))
      .fold("") { (ac, el) =>
        if (ac == "") el else ac + separator + el
      } + end
  }


  // 4. Helper constructors

  /** Just use JVM `toString` implementation, available on every object */
  def fromJvm[A]: Show[A] = new Show[A] {
    def show(a: A): String = a.toString
  }

  /** Provide a custom function to avoid `new Show { ... }` machinery */
  def fromFunction[A](f: A => String): Show[A] = new Show[A] {
    def show(a: A): String = f(a)
  }

}
