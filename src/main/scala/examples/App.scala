package examples

import scala.annotation.tailrec

object App {
  //test
  private def sum1(numbers: List[Int]): Long = {
    numbers match {
      case Nil => 0
      case x :: xs => x + sum1(xs)
    }
  }

  private def sumTailRec(numbers: List[Int]): Long = {
    @tailrec def sumTailRec(numbers: List[Int], result: Long): Long = numbers match {
      case Nil => result
      case x :: xs => sumTailRec(xs, x + result)
    }
    sumTailRec(numbers, 0)
  }

  private def sumOptional(numbers: List[Int], which: String): Long = {

    def sumMap(numbers: List[Int]): Long = {
      val sum = 0;
      numbers.map(sum + _).
      sum
    }

    def sumFoldMap(numbers: List[Int]): Long = {
      numbers.foldLeft(0)(_ + _)
    }

    def sumSimple(numbers: List[Int]): Long = {
      var sum = 0
      numbers.foreach(sum += _)
      sum
    }

    def sumReduceLeft(numbers: List[Int]): Long = {
      numbers.reduceLeft(_ + _)
    }

    def sumSum(numbers: List[Int]): Long = {
      numbers.sum
    }

    which match {
      case "map" => sumMap(numbers)
      case "foldMap" => sumFoldMap(numbers)
      case "sumSimple" => sumSimple(numbers)
      case "reduceLeft" => sumReduceLeft(numbers)
      case _ => sumSum(numbers)
    }
  }

  def main(args: Array[String]): Unit = {
    val numbers = (1 to 5000).toList
    println("Without tailrec: " + sum1(numbers))
    println("With tailrec: " + sumTailRec(numbers))
    println("With map: " + sumOptional(numbers, "map"))
    println("With foldMap: " + sumOptional(numbers, "foldMap"))
    println("With sumSimple: " + sumOptional(numbers, "sumSimple"))
    println("With reduceLeft: " + sumOptional(numbers, "reduceLeft"))
    println("With sumSum: " + sumOptional(numbers, "sum"))
  }

}
