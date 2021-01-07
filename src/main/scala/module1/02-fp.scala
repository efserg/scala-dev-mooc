package module1

import scala.annotation.tailrec

/**
 * Реализуем тип Option
 */


object opt {

  /**
   *
   * Реализовать тип Option, который будет указывать на присутствие либо отсутсвие результата
   */

  sealed trait Option[+A] {
    /**
     *
     * Реализовать метод isEmpty, который будет возвращать true если Option не пуст и false в противном случае
     */
    def isEmpty: Boolean = this match {
      case Option.Some(_) => false
      case Option.None => true
    }

    /**
     *
     * Реализовать метод get, который будет возвращать значение
     */
    def get: A = this match {
      case Option.Some(v) => v
      case Option.None => throw new Exception("get on empty Option")
    }


    /**
     *
     * Реализовать метод printIfAny, который будет печатать значение, если оно есть
     */
    def printIfAny(): Unit = if (!isEmpty) println(get)

    /**
     *
     * реализовать метод orElse который будет возвращать другой Option, если данный пустой
     */
    def orElse[B >: A](default: Option[B]): Option[B] = if (isEmpty) default else this

    /**
     *
     * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
     */
    def zip[B >: A](other: Option[B]): Option[(A, B)] =
      if (isEmpty || other.isEmpty) Option.None
      else Option.Some((get, other.get))

    /**
     *
     * Реализовать метод filter, который будет возвращать не пустой Option
     * в случае если исходный не пуст и предикат от значения = true
     */
    def filter(predicate: A => Boolean): Option[A] =
      if (!isEmpty && predicate(this.get)) this
      else Option.None
  }

  object Option {

    case class Some[A](v: A) extends Option[A]

    case object None extends Option[Nothing]

  }

}

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Long = {
    var _n = 1L
    var i = 2
    while (i <= n) {
      _n *= i
      i += 1
    }
    _n
  }

  def !!(n: Int): Long = {
    if (n <= 1) 1
    else n * !!(n - 1)
  }

  def !(n: Int): Long = {
    @tailrec
    def loop(n1: Int, acc: Long): Long = {
      if (n <= 1) acc
      else loop(n1 - 1, n1 * acc)
    }

    loop(n, 1)
  }

}

object list {
  import List._

  /**
   *
   * Реализовать односвязанный имутабельный список List
   */

  sealed trait List[+A] {
    /**
     *
     * Реализовать метод конс :: который позволит добавлять элемент в голову списка
     */
    def ::[AA >: A](elem: AA): List[AA] = Cons(elem, this)


    /**
     *
     * Реализовать метод mkString который позволит красиво представить список в виде строки
     */
    def mkString: String = mkString(", ")

    def mkString(sep: String): String = {
      import List._

      def loop(l: List[A], acc: StringBuilder): StringBuilder = {
        l match {
          case Nil => acc
          case h :: Nil => acc.append(s"$h")
          case h :: t => loop(t, acc.append(s"$h$sep"))
        }
      }

      loop(this, new StringBuilder()).toString()
    }

    /**
     *
     * Реализовать метод для списка который будет применять некую ф-цию к элементам данного списка
     */
    def map[B](f: A => B): List[B] = this match {
      case Nil => Nil
      case a :: Nil =>  f(a) :: Nil
      case a :: tail => f(a) :: tail.map(f)
    }

    /**
     *
     * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
     */
    def reverse: List[A] = {
      @tailrec
      def loop(remainder: List[A], acc: List[A]): List[A] = {
        remainder match {
          case List.Nil => acc
          case head :: tail => loop(tail, head :: acc)
        }
      }
      loop(this, List())
    }
  }

  object List {

    case object Nil extends List[Nothing]

    case class ::[+A](head: A, tail: List[A]) extends List[A]

    val Cons: ::.type = ::

    /**
     *
     * Реализовать конструктор, для создания списка n элементов
     */
    def apply[T](arg: T*): List[T] = {
      var l: List[T] = List.Nil
      arg.foreach(el => l = el :: l)
      l
    }
  }

  val list = 1 :: List.Nil

  /**
   *
   * Написать функцию incList котрая будет принимать список Int и возвращать список,
   * где каждый элемент будет увеличен на 1
   */
  def incList(l: List[Int]): List[Int] = l.map(_ + 1)

  /**
   *
   * Написать функцию shoutString котрая будет принимать список String и возвращать список,
   * где к каждому элементу будет добавлен префикс в виде '!'
   */
  def shoutString(l: List[String]): List[String] = l.map("!" + _)


}