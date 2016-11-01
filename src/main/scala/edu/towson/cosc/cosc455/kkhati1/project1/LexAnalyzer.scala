package edu.towson.cosc.cosc455.kkhati1.project1

/**
  * Created by kkhati1 on 10/11/2016.
  */
class LexAnalyzer extends LexicalAnalyzer{
  private var position: Int = -1
  var current : Char = ' '
  var currentString : String = " "
  val lexems: List[String] = List("\\BEGIN\r\n","\\END\r\n", "\\TITLE[","]\r\n","]", "#", "\\PARB\r\n",
                                  "\\PARE\r\n", "**\r\n","**","*", "*\r\n", "+", "\\\r\n", "[", "(", ")\r\n", "![",
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
      while(!(current.equals('\\') || current.equals(('#')) || current.equals('*')|| current.equals('[')|| current.equals('!')|| current.equals('+'))){
        addChar();
        current = getChar();
      }
    }
    Compiler.currentToken = currentString
    print(Compiler.currentToken)
    println(lookup())
  }

  def lookup(): Boolean = {
    var flag = false
    if(lexems.contains(Compiler.currentToken))
      flag = true

    flag
  }

}
