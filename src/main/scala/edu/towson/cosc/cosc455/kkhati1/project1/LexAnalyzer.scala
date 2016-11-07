package edu.towson.cosc.cosc455.kkhati1.project1

import scala.collection.immutable.Queue
import scala.collection.mutable

/**
  * Created by kkhati1 on 10/11/2016.
  */
class LexAnalyzer extends LexicalAnalyzer{

  var current : Char = ' '
  var currentString : String = " "
  var Text : Boolean = false
  val lexems: List[String] = List("\\BEGIN","\\END", "\\TITLE[","]", "#", "\\PARB",
                                  "\\PARE", "**","**","*", "*", "+", "\\", "[", "(", ")", "![",
                                  "\\DEF[", "=", "\\USE[")
  var tokenString = ""
  override def addChar(): Unit = {
    tokenString = tokenString + current
  }

  override def getChar(): Char = {
    Compiler.position+=1
    Compiler.fileContents.charAt(Compiler.position)
  }

  override def getNextToken(): Unit = {
    current = getChar()
    if ((current.equals(' ') || current.equals('\r') || current.equals('\n'))) {
         current = getChar()
    }
    if (current.equals('\\') || current.equals('#') || current.equals('*') || current.equals('+')) {
      addChar()
      current=getChar()
      while (!((current.equals('[') || current.equals('(') || current.equals('\r') || current.equals('\n') ||current.equals(']')))) {
          addChar()
          current = getChar()
        }
      if(current.equals('['))
        addChar()

      if(lookup()){
        Compiler.currentToken = tokenString
        tokenString =""
      }else{
        var Tree2 = new mutable.Stack[String]()
        Tree2 = Compiler.Parser.retTree
        while(!Tree2.isEmpty)
          println(Tree2.pop())
        println("Lexical Token error: Token does not exist")
        System.exit(1)
      }
    }
    if(text()){
      while(text()){
        addChar()
        current = getChar()
      }
      Compiler.currentToken = tokenString
      tokenString = ""
    }
    if(current.equals(CONSTANTS.BRACKETE))
      Compiler.currentToken = CONSTANTS.BRACKETE
  }
  def text() : Boolean = {
    Text = false
    for(character <- 'A' to 'Z'){
      if(current.equals(character)) {
        Text = true
      }
    }
    for(number <- '0' to '9'){
      if(current.equals(number))
        Text = true
    }
    for(character <- 'a' to 'z'){
      if(current.equals(character))
        Text = true
    }
    if(current.equals(',') || current.equals('.')|| current.equals('\"')|| current.equals('.')
      || current.equals('?')|| current.equals('_')|| current.equals('/') || current.equals(' '))
        Text = true

    Text
  }
  def lookup(): Boolean = {
    var flag = false
    if(lexems.contains(tokenString))
      flag = true
    flag
  }

}
