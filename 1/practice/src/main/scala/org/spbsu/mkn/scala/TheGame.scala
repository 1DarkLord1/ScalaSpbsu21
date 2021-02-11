package org.spbsu.mkn.scala

import scala.io.StdIn.readLine
import scala.util.Random

object TheGame {

  sealed trait GuessResult
  case class Correct(numTries: Int) extends GuessResult
  case class Incorrect(bulls: Int, cows: Int) extends GuessResult

  class RepeatingDigitsException extends RuntimeException
  class WrongNumberLengthException(val expected: Int, val got: Int) extends RuntimeException


  def generateNumberString(length: Int): String = {
    val charList = ('0' to '9').toList ++ ('A' to 'Z').toList
    scala.util.Random.shuffle(charList).take(length).mkString
  }

  def cows(secret: String, userInput: String): Int =
    (secret.toList zip userInput.toList).filter(pair => pair._1 != pair._2).
      map(pair => pair._2).count(x => secret.toList.contains(x))

  def bulls(secret: String, userInput: String): Int =
    (secret.toList zip userInput.toList).count(pair => pair._1 == pair._2)

  def validate(secret: String, userInput: String, numTries: Int = 1): GuessResult = {
    if (userInput.length != secret.length)
      throw new WrongNumberLengthException(secret.length, userInput.length)
    else if (userInput.toSet.size != userInput.length)
      throw new RepeatingDigitsException

    if (secret == userInput) Correct(numTries)
    else Incorrect(bulls(secret, userInput), cows(secret, userInput))
  }

  def main(args: Array[String]): Unit = {
    print("Enter your name: ")
    val name = readLine()
    println(s"Hello, $name!")

    print("Enter the secret length: ")
    val len = readLine().toInt

    val secret = generateNumberString(len)
    println("The secret has been generated.")

    var numTries = 1
    var isCorrect = false
    do {
      print("Enter your guess: ")
      val userInput = readLine()
      try {
        val verdict = validate(secret, userInput, numTries)
        verdict match {
          case x: Correct =>
            isCorrect = true
            println("You're right! " + name + ", your score is " + x.numTries + " tries.")
          case x: Incorrect =>
            println("You have " + x.bulls + " bulls and " + x.cows + " cows. Try again.")
        }
      }
      catch {
        case e: WrongNumberLengthException =>
          println("Wrong length! Expected " + e.expected + ", but got " + e.got + ".")
        case _: RepeatingDigitsException =>
          println("Repeating digits!")
      }
      numTries += 1
    } while(!isCorrect)

  }
}
