package au.com.nuvento.demoscalamaven

object BuildingBlocks {

  // Queustion 1
  def hello() = {
    println("Hello Data Academy")
  }

  // Queustion 2
  def greet() = {
    println("Please enter your name:")
    val name = scala.io.StdIn.readLine()
    println(s"Hello $name")
  }

  // Queustion 3
  def greetAliceBob() = {
    println("Please enter your name:")
    val name = scala.io.StdIn.readLine()
    if (name == "Alice" || name == "Bob"){
      println(s"Hello $name")
    }
  }

  // Queustion 4
  def sumToN() = {
    def adding(n: Int): Int =
      if (n <= 0) 0
      else n + adding(n - 1)
    println("Please enter a number:")
    val n = scala.io.StdIn.readInt()
    println(adding(n))
  }

  // Queustion 5
  def sumToNThreeFive() = {
    def adding(n: Int): Int =
      if (n <= 2) 0
      else if (n % 3 == 0 || n % 5 == 0) n + adding(n - 1)
      else adding(n - 1)

    println("Please enter a number:")
    val n = scala.io.StdIn.readInt()
    println(adding(n))
  }

  // Queustion 6
  def sumOrProduct() = {
    def sum(n: Int): Int =
      if (n <= 0) 0
      else n + sum(n - 1)

    def product(n: Int): Int =
      if (n <= 1) 1
      else n * product(n - 1)

    print("Please enter a number: ")
    val n = scala.io.StdIn.readInt()
    print("Please choose your operation, either 'add' of 'multiply': ")
    val operation = scala.io.StdIn.readLine()
    if (operation == "add") println(sum(n))
    else if (operation == "multiply") println(product(n))
    else println("Operation not specified")
  }

  // Queustion 7
  def multiplyTable() = {
    def printforalign(num: Int) = {
      if (num < 10) print(" ")
      if (num < 100) print(" ")
      print(s"$num")
    }
    def table(rownum: Int): Unit = {
      if (rownum != 1) table(rownum - 1)
      printforalign(rownum)
      print(" | ")
      cell(rownum, 12)
      println("")
    }
    def cell(rownum: Int, colnum: Int): Unit = {
      val celnum = rownum * colnum
      if (colnum == 1) {
        printforalign(celnum)
      }
      else {
        cell(rownum, colnum - 1)
        printforalign(celnum)
      }
      print(" ")
    }
    print("  * | ")
    cell(1, 12)
    println("")
    println("-----------------------------------------------------")
    table(12)
  }

  // Question 8
  def allprimes(finish: Int): Unit = {
    def checkIfPrime(number: Int, factor: Int): Boolean = {
      if (number % factor == 0) false
      else if (factor >= scala.math.sqrt(number)) true
      else checkIfPrime(number, factor+1)
    }
    if (finish == 2) println(finish)
    else if (finish > 2) {
      allprimes(finish - 1)
      if (checkIfPrime(finish, 2)) println(finish)
    }
  }

  // Question 9
  def randomNumber(): Unit = {
    var luckynum = scala.util.Random.nextInt(100) + 1

    def makeAGuess(guesses: List[Int]): Unit = {
      val guess = scala.io.StdIn.readInt()
      if (guesses.size > 0 && guesses.head == guess) {
        print("You just guessed that. Guess a different number: ")
        makeAGuess(guesses)
      } else if (guess <= 0 || guess > 100) {
        print("Your guess was not in the range of 1 to 100. Guess again: ")
        makeAGuess(guesses)
      } else if (luckynum < guess) {
        print("Your guess was too high. Guess again: ")
        makeAGuess(guess :: guesses)
      } else if (luckynum > guess) {
        print("Your guess was too low. Guess again: ")
        makeAGuess(guess :: guesses)
      } else {
        println(s"Correct! The number was $luckynum!")
        println(s"It took you ${guesses.size + 1} guesses.")
      }

    }

    print("Guess a number from 1 to 100 (inclusive): ")
    makeAGuess(List())
  }

  // Question 10
  def leapYear() = {
    def checkYear(year: Int, leapsSoFar: Int): Unit = {
      if ((year % 4 == 0 && year % 100 != 0) || year % 400 == 0) {
        println(year)
        if (leapsSoFar != 1) checkYear(year+1, leapsSoFar-1)
      }
      else checkYear(year+1, leapsSoFar)
    }
    checkYear(2023, 20)
  }

  def main(args: Array[String]) = {
    hello()
    //greet()
    //greetAliceBob()
    //sumToN()
    //sumToNThreeFive()
    //sumOrProduct()
    //multiplyTable()
    //allprimes(30)
    randomNumber()
    //leapYear()
  }

}
