package edu.towson.cosc.cosc455.kkhati1.project1

/**
  * Created by kkhati1 on 10/11/2016.
  */
class SynAnalyzer extends SyntaxAnalyzer {
  var Tree = new scala.collection.mutable.Stack[String]

  override def gittex() : Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) {
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      //variableDefine()
      title()
      //body()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
        Tree.push(Compiler.currentToken)
      }
    } else {
      println("SYNTAX ERROR : good message here, fool gittex ")
      System.exit(1)
    }
  }

  override def title() : Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)){
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if(!(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE))){
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }else{
      println("Syntax Error: title")
      System.exit(1)
    }
  }

  override def body(): Unit = ???

  override def paragraph(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
      Tree.push(Compiler.currentToken)
      variableDefine()
      innerText()
      Compiler.Scanner.getNextToken()

      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)) {
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      } else {
        println("SYNTAX ERROR : good message here, fool body")
        System.exit(1)
      }
    } else {
      println("SYNTAX ERROR : good message here, fool body")
    }
  }

  override def innerText(): Unit = ???

  override def heading(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      //if(Compiler)
    }
  }

  override def variableDefine(): Unit = {

  }

  override def variableUse(): Unit = ???

  override def bold(): Unit = ???

  override def italics(): Unit = ???

  override def listItem(): Unit = ???

  override def innerItem(): Unit = ???

  override def link(): Unit = ???

  override def image(): Unit = ???

  override def newline(): Unit = ???

  def retTree: scala.collection.mutable.Stack[String]={
    return Tree;
  }


}