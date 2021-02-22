package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._

import scala.annotation.tailrec

sealed trait IntList {
  def head: Int
  def tail: IntList
  def drop(n: Int): IntList
  def take(n: Int): IntList
  def map(f: Int => Int): IntList
  def ::(elem: Int): IntList = IntCons(elem, this)
}

case object IntNil extends IntList {
  override def head: Int = undef
  override def tail: IntList = undef
  override def drop(n: Int): IntList =
    if (n == 0) {
      IntNil
    } else {
      undef
    }
  override def take(n: Int): IntList =
    if (n == 0) {
      IntNil
    } else {
      undef
    }

  override def map(f: Int => Int): IntList = IntNil
}

case class IntCons(elem: Int, list: IntList) extends IntList {
  override def head: Int = elem
  override def tail: IntList = list
  override def drop(n: Int): IntList =
    if (n == 0) {
      this
    } else if (n > 0) {
        list.drop(n - 1)
    } else {
      undef
    }
  override def take(n: Int): IntList =
    if (n == 0) {
      IntNil
    } else if (n > 0) {
      IntCons(elem, list.take(n - 1))
    } else {
      undef
    }

  override def map(f: Int => Int): IntList = IntCons(f(elem), list.map(f))
}

object IntList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")
  def fromSeq(seq: Seq[Int]): IntList = seq.foldRight(IntNil.asInstanceOf[IntList])((x, xs) => x :: xs)
  def sum(intList: IntList): Int      =
    intList match {
      case IntCons(_, _) => foldLeft(intList)((acc, x) => acc + x, 0)
      case IntNil        => undef
    }
  def size(intList: IntList): Int     = foldLeft(intList)((acc, _) => acc + 1, 0)

  @tailrec
  def foldLeft(intList: IntList)(f: (Int, Int) => Int, ini: Int): Int =
    intList match {
      case IntCons(x, xs) => foldLeft(xs)(f, f(ini, x))
      case IntNil         => ini
    }
}