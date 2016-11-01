package edu.towson.cosc.cosc455.kkhati1.project1

/**
  * Created by kkhati1 on 10/11/2016.
  */
class LexAnalyzer extends LexicalAnalyzer{
  private var position: Int = -1
  var current : Char = ' '
  var currentString : String = " "
  val lexems: List[String] = List("\\BEGIN","\\END", "\\TITLE[","]","]", "#", "\\PARB",
                                  "\\PARE", "**","**","*", "*", "+", "\\", "[", "(", ")", "![",
                                  "\\DEF[", "=", "\\USE[")
  var lexToken = new scala.collection.mutable.Queue[Char]
  override def addChar(): Unit = {
      lexToken.enqueue(current)
      currentString = lexToken.mkString
  }

  override def getChar(): Char = {
    position+=1
    Compiler.fileContents.charAt(position)
  }

  override def getNextToken(): Unit = {
    current = getChar()
    if (current.equals(' ')) {
      while ((current.equals(' '))) {
        current = getChar();
      }
    }else if(current.equals('\\') || current.equals(('#')) || current.equals('*')|| current.equals('[')|| current.equals('!')|| current.equals('+')) {
      addChar();
      current = getChar();
      while(!(current.equals('\\') || current.equals(('#')) || current.equals('*')|| current.equals('[')|| current.equals('!')|| current.equals('+')
             || current.equals('\n')||current.equals('\r'))){
        addChar();
        current = getChar();
      }
      if(lookup()){
        Compiler.currentToken = currentString
        println(lookup())
      }else{
        println("Lexical Error- Token does not exist!")
        System.exit(1)
      }
    }
  }

  def lookup(): Boolean = {
    var flag = false
    if(lexems.contains(currentString))
      flag = true
    flag
  }

}
