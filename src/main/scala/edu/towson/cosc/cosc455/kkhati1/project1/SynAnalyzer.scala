package edu.towson.cosc.cosc455.kkhati1.project1

/**
  * Created by kkhati1 on 10/11/2016.
  */
class SynAnalyzer extends SyntaxAnalyzer{
  var Tree = new scala.collection.mutable.Stack[String]
  override def gittex(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) {
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      variableDefine()
      title()
      body()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)){
        Tree.push(Compiler.currentToken)
      }
    } else {
      println("SYNTAX ERROR : good message here, fool gittex ")
      System.exit(1)
    }
  }
  override def title(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)){
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      requiredText()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
        Tree.push(Compiler.currentToken)
      }
    }else{
      println("Syntax Error")
      System.exit(1)
    }
  }

  override def body(): Unit = ???

  override def paragraph(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)){
      Tree.push(Compiler.currentToken)
      variableDefine()
      innerText()
      Compiler.Scanner.getNextToken()

      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)){
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }else{
        println("SYNTAX ERROR : good message here, fool body")
        System.exit(1)
      }
    }else{
      println("SYNTAX ERROR : good message here, fool body")
    }
  }

  override def innerText(): Unit = ???

  override def heading(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)){
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

  def text(): Unit = {
    letters()
    numbers()
    specialChar()
  }

  def requiredText():Unit = {
    requiredLetters()
    requiredNumbers()
    requiredSpecials()
  }

  def letters(): Unit = {
    for(letters <- 'A' to 'Z'){
      if(Compiler.currentToken.equals(letters)){
        val let = letters.toString
        Tree.push(let)
      }
    }
  }

  def numbers():Unit = {
    for(numbers <- '0' to '9'){
      if(Compiler.currentToken.equals(numbers)){
        val num = numbers.toString
        Tree.push(num)
      }
    }
  }

  def specialChar():Unit = {
    val aList:List[String]=List(",",".",":","?","_","\"")
    for(kVal<-0 until aList.length-1)
      if(Compiler.currentToken.equals(aList{kVal})){
        Tree.push(aList{kVal})
      }
  }

  def requiredLetters():Unit = {
    for(letter <- 'A' to 'Z'){
      if(Compiler.currentToken.equals(letter)){
        val char = letter.toString
        Tree.push(char)
      }else{
        println("Syntax Error: You fool!")
        System.exit(1)
      }
    }
  }

  def requiredNumbers(): Unit = {
    for(numb <- '0' to '9'){
      if(Compiler.currentToken.equals(numbers)){
        val num = numb.toString
        Tree.push(num)
      }else{
        println("Syntax Error")
        System.exit(1)
      }
    }
  }

  def requiredSpecials():Unit = {
    val aList:List[String]=List(",",".",":","?","_","\"")
    for(kVal<-0 until aList.length-1)
      if(Compiler.currentToken.equals(aList{kVal})){
        Tree.push(aList{kVal})
      }else{
        println("Syntax Error")
        System.exit(1)
      }
  }
}
