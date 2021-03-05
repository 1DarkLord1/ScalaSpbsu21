package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.MyGenericList._

import scala.annotation.tailrec

sealed trait MyGenericList[+T] {
  def head: T
  def tail: MyGenericList[T]
  def drop(n: Int): MyGenericList[T]
  def take(n: Int): MyGenericList[T]
  def map[U](f: T => U): MyGenericList[U]
  def ::[U >: T](elem: U): MyGenericList[U] = MyCons(elem, this)
}

case object MyNil extends MyGenericList[Nothing] {
  override def head: Nothing = undef
  override def tail: MyGenericList[Nothing] = undef
  override def drop(n: Int): MyGenericList[Nothing] =
    if (n == 0) {
      MyNil
    } else {
      undef
    }
  override def take(n: Int): MyGenericList[Nothing] =
    if (n == 0) {
      MyNil
    } else {
      undef
    }

  override def map[U](f: Nothing => U): MyGenericList[Nothing] = MyNil
}

case class MyCons[T](elem: T, list: MyGenericList[T]) extends MyGenericList[T] {
  override def head: T = elem
  override def tail: MyGenericList[T] = list
  override def drop(n: Int): MyGenericList[T] =
    if (n == 0) {
      this
    } else if (n > 0) {
      list.drop(n - 1)
    } else {
      undef
    }
  override def take(n: Int): MyGenericList[T] =
    if (n == 0) {
      MyNil
    } else if (n > 0) {
      MyCons(elem, list.take(n - 1))
    } else {
      undef
    }

  override def map[U](f: T => U): MyGenericList[U] = MyCons(f(elem), list.map(f))
}

object MyGenericList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")
  def fromSeq[T](seq: Seq[T]): MyGenericList[T] = seq.foldRight(MyNil : MyGenericList[T])((x, xs) => x :: xs)
  def sum(MyGenericList: MyGenericList[Int]): Int =
    MyGenericList match {
      case MyCons(_, _) => foldLeft(MyGenericList)((acc : Int, x : Int) => acc + x, 0)
      case MyNil        => undef
    }
  def size[T](MyGenericList: MyGenericList[T]): Int = foldLeft(MyGenericList)((acc : Int, _) => acc + 1, 0)

  @tailrec
  def foldLeft[U, T](MyGenericList: MyGenericList[T])(f: (U, T) => U, ini: U): U =
    MyGenericList match {
      case MyCons(x, xs) => foldLeft(xs)(f, f(ini, x))
      case MyNil         => ini
    }
}