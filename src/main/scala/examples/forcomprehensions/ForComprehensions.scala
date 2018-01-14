package examples.forcomprehensions

object ForComprehensions {

  object Student {

    val students = List(
      new Student("Oleg", 3),
      new Student("Serhii", 5),
      new Student("Petro", 2)
    )

    class Student(val name: String, val mark: Int)

    def satisfactoryStudents(xs: List[Student]): List[String] = {
      for (s <- xs if s.mark >= 3) yield s.name
    }
  }

  object Numeric {

    def divisors(n: Int): List[Int] =
      for (i <- List.range(1, n+1) if n % i == 0) yield i

    def isPrime(n: Int) = divisors(n).length == 2

    def sum(xs: List[Double]): Double =
      xs.foldLeft(0.0) { (x, y) => x + y }

    def zipSum(xs: List[Double], ys: List[Double]) =
      sum(for((x, y) <- xs zip ys) yield x * y);

    def removeDuplicates[A](xs: List[A]): List[A] =
      if (xs.isEmpty)
        xs
      else
        xs.head :: removeDuplicates(for (x <- xs.tail if x != xs.head) yield x)
  }

  def main(args: Array[String]) {

    import Student._

    print("Satisfactory students: ")
    satisfactoryStudents(students) foreach { x => print(x + " ") }
    println

    import Numeric._

    println("divisors(34) = " + divisors(34))

    println("Is 7 prime number: " + isPrime(7))

    val xs = List(3.5, 5.0, 4.5)
    println("average(" + xs + ") = " + sum(xs) / xs.length)

    val ys = List(2.0, 1.0, 3.0)
    println("zipSum(" + xs + ", " + ys +") = " + zipSum(xs, ys))

    val listWithDuplicates = List(1, 2, 3, 4, 4, 1, 6, 7, 7)
    println("List without duplicates: " + removeDuplicates(listWithDuplicates))
  }
}

