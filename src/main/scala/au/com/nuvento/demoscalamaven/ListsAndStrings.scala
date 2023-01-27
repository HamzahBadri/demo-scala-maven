package au.com.nuvento.demoscalamaven

object ListsAndStrings {

  //Question 1
  def findLargest(list: List[Int]): Int = {
    def check(largest: Int, miniList: List[Int]): Int = {
      if (miniList.size <= 0) largest
      else if (miniList.head > largest) check(miniList.head, miniList.tail)
      else check(largest, miniList.tail)
    }
    check(list.head, list.tail)
  }

  //Question 2
  def reverseList(list: List[Int]): List[Int] = {
    if (list.size <= 1) list
    else reverseList(list.tail) :+ list.head
  }

  //Question 3
  def checkForOccurence(list: List[Int], element: Int): Boolean = {
    if (list.head == element) true
    else if (list.tail.size <= 0) false
    else checkForOccurence(list.tail, element)
  }

  //Question 4
  def oddPostiions(list: List[Int]): List[Int] = {
    list match {
      case head :: _ :: rest => (head :: oddPostiions(rest))
      case List(position) => List(position)
      case _ => List()
    }
  }

  //Question 5
  def palindrome(string: String): Boolean = {
    if (string.length <= 1) true
    else if (string.charAt(0) != string.charAt(string.length - 1)) false
    else palindrome(string.substring(1, string.length - 1))
  }

  //Question 6
  class computeSum (list: List[Int]){
    def forSum(): Int = {
      var sum = 0
      var reduceList = list
      for (_ <- 1 to list.size){
        sum = sum + reduceList.head
        reduceList = reduceList.tail
      }
      sum
    }

    def whileSum(): Int = {
      var sum = 0
      var reduceList = list
      while (reduceList.size > 0) {
        sum = sum + reduceList.head
        reduceList = reduceList.tail
      }
      sum
    }

    def recursiveSum(): Int = {
      if (list.size < 1) 0
      else list.head + computeSum(list.tail).recursiveSum()
    }

  }

  //Question 7
  def concatenate(list1: List[Int], list2: List[Int]): List[Int] = {
    if (list2.size < 1) list1
    else concatenate(list1 :+ list2.head, list2.tail)
  }

  //Question 8
  def fibonacci(): List[Int] = {
    def nextFib(list: List[Int]): List[Int] = {
      if (list.size >= 100) list
      else if (list.size < 2) nextFib(List(1, 1))
      else nextFib(list.head + list.tail.head :: list)
    }
    reverseList(nextFib(List()))
  }

  //Question 9
  def digitList(int: Int): List[Int] = {
    def nextInt(digits: List[Int], exp: Int): List[Int] = {
      if (int / scala.math.pow(10, exp) < 1) digits
      else {
        val removeOldDigits: Int = (int / scala.math.pow(10, exp)).toInt
        val oneDigit = removeOldDigits % 10
        nextInt(oneDigit :: digits, exp + 1)
      }
    }
    nextInt(List(), 0)
  }

  def main(args: Array[String]) = {
    println("Question 1:")
    println(findLargest(List(1, 2, 3, 4, 5))) //5
    println(findLargest(List(2, 3, 5, 5, 6, 1, 6))) //6
    println(findLargest(List(1))) //1

    println("Question 2:")
    println(reverseList(List(1,2,3,4,5))) //List(5, 4, 3, 2, 1)
    println(reverseList(List())) //List()
    println(reverseList(reverseList(List(3, 4, 5, 4, 9)))) //List(3, 4, 5, 4, 9)

    println("Question 3:")
    println(checkForOccurence(List(1,2,3,4,5), 3)) //true
    println(checkForOccurence(List(1,2,3,4,5), 6)) //false
    println(checkForOccurence(List(1,2,3,4,5), 5)) //true

    println("Question 4:")
    println(oddPostiions(List(1, 2, 3, 4, 5))) //List(1, 3, 5)
    println(oddPostiions(List(9))) //List(9)
    println(oddPostiions(List(1, 2))) //List(1)

    println("Question 5:")
    println(palindrome("madam")) //true
    println(palindrome("sinnis")) //true
    println(palindrome("")) //true
    println(palindrome("race car")) //false

    println("Question 6:")
    val testList = new computeSum(List(1,2,3,4,5))
    println(testList.forSum()) //15
    println(testList.whileSum()) //15
    println(testList.recursiveSum()) //15

    println("Question 7:")
    println(concatenate(List(1, 2, 3), List(4, 4, 5))) //List(1, 2, 3, 4, 4, 5)
    println(concatenate(List(1, 2, 3), List())) //List(1, 2, 3)
    println(concatenate(List(), List(4, 4, 5))) //List(4, 4, 5)

    println("Question 8:")
    val fib = fibonacci()
    println(fibonacci())
    println(s"Size of sequence: ${fib.size}") //100

    println("Question 9:")
    println(digitList(2342)) //List(2, 3, 4, 2)
    println(digitList(0)) //List()
    println(digitList(30007)) //List(3, 0, 0, 0, 7)
  }

}
