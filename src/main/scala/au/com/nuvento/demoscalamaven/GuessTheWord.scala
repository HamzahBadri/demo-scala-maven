package au.com.nuvento.demoscalamaven

object GuessTheWord {

  val wordBank = List("david",
  "ability",
  "sustain",
  "progress",
  "articulate",
  "scientist",
  "complete",
  "hysteria",
  "advanced",
  "spectrum",
  "detective",
  "husband",
  "microwave",
  "graduate",
  "surprise",
  "kangaroo",
  "tactical",
  "cucumber",
  "wrestler",
  "fragment",
  "health",
  "question",
  "antique",
  "thirsty",
  "psychic")

  def displayWord(word: String, guesses: Set[Char]): Boolean = {
    var solved = true
    for (index <- 0 to word.length - 1) {
      if (guesses.contains(word.charAt(index))) print(word.charAt(index))
      else {
        print("_")
        solved = false
      }
    }
    println("")
    solved
  }

  def guessALetter(word: String, guesses: Set[Char]): Boolean = {
    print("Guess a letter: ")
    val letter = scala.io.StdIn.readChar()
    val newGuesses = guesses + letter
    val wordGuessed = displayWord(word, newGuesses)
    println(s"You have guessed ${newGuesses.size} letters.")
    println(s"Guessed letters: $newGuesses")
    if (wordGuessed) true
    else if (newGuesses.size < 12) guessALetter(word, newGuesses)
    else false
  }

  def main(args: Array[String]) = {
    print("Ready to play? Please enter your name: ")
    val name = scala.io.StdIn.readLine()
    println("You must guess letters up to 12 times. If you guess all the letters in the word, you win!")
    println(s"Good luck, $name!")
    val theWord = wordBank(scala.util.Random.nextInt(wordBank.size))
    displayWord(theWord, Set())
    val result = guessALetter(theWord, Set())
    if (result) println(s"You did it! The word was --$theWord--. Well done, $name!")
    else println(s"Sorry, you couldn't guess that --$theWord-- was the word. Better luck next time, $name!")
  }

}
