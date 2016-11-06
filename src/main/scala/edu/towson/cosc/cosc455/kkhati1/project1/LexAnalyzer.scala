package edu.towson.cosc.cosc455.kkhati1.project1

import scala.collection.immutable.Queue

/**
  * Created by kkhati1 on 10/11/2016.
  */
class LexAnalyzer extends LexicalAnalyzer{

  var current : Char = ' '
  var currentString : String = " "
  var reqText : Boolean = false
  val lexems: List[String] = List("\\BEGIN","\\END", "\\TITLE[","]","]", "#", "\\PARB",
                                  "\\PARE", "**","**","*", "*", "+", "\\", "[", "(", ")", "![",
                                  "\\DEF[", "=", "\\USE[")
  var lexToken = new scala.collection.mutable.Queue[Char]
  override def addChar(): Unit = {
      lexToken.enqueue(current)
      currentString = lexToken.mkString
  }

  override def getChar(): Char = {
    Compiler.position+=1
    Compiler.fileContents.charAt(Compiler.position)
  }

  override def getNextToken() : Unit = {
    current = getChar()
    if (current.equals(' ') || current.equals('\n')) {
      while (current.equals(' ') ) {
        current = getChar()
      }
    }else if(current.equals('\\') || current.equals(('#')) || current.equals('*') || current.equals('+')) {
      addChar()
      current = getChar()
      while(!(current.equals('\n')||current.equals('\r') || current.equals('[') || current.equals('('))) {
        addChar()
        current = getChar()
      }
      if(lookup()){
        Compiler.currentToken = currentString
        while(!lexToken.isEmpty) {
          lexToken.dequeue()
        }
      }else{
        println("Lexical Error- Token does not exist!")
        System.exit(1)
      }
    }
    }

  def text():Boolean={
    for(character <- 'A' to 'Z'){
      if(current.equals(character))
          return true
    }
    for(number <- '0' to '9'){
      if(current.equals(number))
        return true
    }
    for(character <- 'a' to 'z'){
      if(current.equals(character))
        return true
    }
    if(current.equals(',') || current.equals('.')|| current.equals('.')|| current.equals('.')
      || current.equals('?')|| current.equals('_')|| current.equals('/'))
        return true
    return false
  }
  def lookup(): Boolean = {
    var flag = false
    if(lexems.contains(currentString))
      flag = true
    flag
  }

}
