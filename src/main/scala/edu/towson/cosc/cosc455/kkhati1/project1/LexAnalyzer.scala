package edu.towson.cosc.cosc455.kkhati1.project1

import scala.collection.immutable.Queue
import scala.collection.mutable

/**
  * Created by kkhati1 on 10/11/2016.
  */
class LexAnalyzer extends LexicalAnalyzer{

  var current : Char = ' '
  var Text : Boolean = false
  var BoolCheck : Boolean = false
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
    Compiler.Parser.TextBool = false
    if (isSpace()) {
      current = getChar()
      while (isSpace()) {
        current = getChar()
      }
    }
    if(lexems.contains(current.toString)){
      Compiler.currentToken = current.toString
      if(current.equals('=') || current.equals(']'))
        current = getChar()
    } else if (text()) {
      Compiler.Parser.TextBool = true;
      do {
        addChar()
        current = getChar()
      } while (text())
      Compiler.currentToken = tokenString
      tokenString = ""
    } else if (isSpecial()) {
      do {
        addChar();
        current = getChar()
      } while (!endChar())
      if (current.equals('[') || current.equals('\\') || current.equals('*') || current.equals(')') || current.equals('(')) {
        addChar()
        current = getChar()
      }
      if (lookup()) {
        Compiler.currentToken = tokenString
        tokenString = ""
      } else {
        println("Lexical Error: Lexical Token Not Found")
        System.exit(1)
      }
    }
    else {
      println("Lexical Error: Lexical Token Not Found")
      System.exit(1)
    }
  }

  def isSpace(): Boolean = {
    if(current.equals('\r') || current.equals('\n') || current.equals(' ')){
      true
    }else
      false
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