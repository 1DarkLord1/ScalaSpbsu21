package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._

import scala.annotation.tailrec

sealed trait IntList {
  def head: Int =
    this match {
      case IntCons(x, _) => x
      case IntNil        => IntList.undef
    }
  def tail: IntList =
    this match {
      case IntCons(_, xs) => xs
      case IntNil         => IntList.undef
    }
  def drop(n: Int): IntList =
    if (n == 0) {
      this
    } else if (n > 0) {
      this match {
        case IntCons(_, xs) => xs.drop(n - 1)
        case IntNil         => IntList.undef
      }
    } else {
      IntList.undef
    }
  def take(n: Int): IntList =
    if (n == 0) {
      IntNil
    } else if (n > 0) {
      this match {
        case IntCons(x, xs) => IntCons(x, xs.take(n - 1))
        case IntNil         => IntList.undef
      }
    } else {
      IntList.undef
    }

  def map(f: Int => Int): IntList =
    this match {
      case IntCons(x, xs) => IntCons(f(x), xs.map(f))
      case IntNil         => IntNil
    }
  def ::(elem: Int): IntList = IntCons(elem, this)
}
case object IntNil extends IntList
case class IntCons(elem: Int, list: IntList) extends IntList

object IntList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")
  def fromSeq(seq: Seq[Int]): IntList =
    seq match {
      case Seq() => IntNil
      case _     => seq.head :: fromSeq(seq.tail)
    }
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