package edu.towson.cosc.cosc455.kkhati1.project1

import scala.collection.immutable.Queue
import scala.collection.mutable

/**
  * Created by kkhati1 on 10/11/2016.
  */
class LexAnalyzer extends LexicalAnalyzer{

  var current : Char = ' '
  var Text : Boolean = false
  val lexems: List[String] = List("\\BEGIN","\\END", "\\TITLE[","]", "#", "\\PARB",
                                  "\\PARE", "*", "+", "\\\\", "[", "(", ")", "![",
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
    if(current.equals(' ')|| current.equals('\n')){
      Compiler.Parser.TextBool = false
      isSpace()
    }
    if (isSpecial()){
      Compiler.Parser.TextBool = false
      addChar()
      current = getChar()
      while(!endChar()) {
        addChar()
        current = getChar()
      }
      if (current.equals('[') || current.equals('\\')
        || current.equals(']') || current.equals('*') || current.equals(')') || current.equals('(')) {
        addChar()
      }
      if (lookup()) {
        Compiler.currentToken = tokenString
        tokenString =""
      } else {
        println("Lexical Error")
      }
    }
    if(text()) {
      Compiler.Parser.TextBool = true
      addChar()
      while (text()) {
        current = getChar()
        addChar()

      }
      Compiler.currentToken = tokenString
      tokenString = ""
    } else if( current.equals(')') || current.equals('(')){
      Compiler.Parser.TextBool = false
      addChar()
      current = getChar()
      if(lookup()){
        Compiler.currentToken = tokenString
        tokenString = ""
      }
    }
  }

  def isSpace(): Unit = {
    while(current.equals('\r') || current.equals('\n') || current.equals(' ')){
      current = getChar()
    }
  }
  def isSpecial(): Boolean = {
    current match{
      case '\\' | '*' | '#' | '+' | '[' | '!' => true
      case _ => false
    }
  }
  def endChar(): Boolean = {
    current match {
      case '\r'| '\n' | '[' | '\\' | ']' | '*' | ')' | '('=> true
      case _ => false
      }
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
      flag = true;
    flag
  }

}
