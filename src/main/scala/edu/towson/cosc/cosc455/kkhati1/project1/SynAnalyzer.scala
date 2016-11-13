package edu.towson.cosc.cosc455.kkhati1.project1

/**
  * Created by kkhati1 on 10/11/2016.
  */
class SynAnalyzer extends SyntaxAnalyzer {
  var Tree = new scala.collection.mutable.Stack[String]
  var TextBool = false
  var innerItemFound = false;
  override def gittex() : Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) {
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      title()
      variableDefine()
      body()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
        Tree.push(Compiler.currentToken)
      }
    } else {
      println("SYNTAX ERROR : Beginning HTML Tag not found ")
      System.exit(1)
    }
  }

  override def title() : Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)){
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if(TextBool) {
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }else {
        println("Syntax Error: Text Required")
        System.exit(1)
      }
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }else{
        println("Ending Bracket not found")
        System.exit(1)
      }
    }
  }

  override def body(): Unit = {
    paragraph()
    innerText()
    newline()
    while(!Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
      Compiler.Scanner.getNextToken()
      body()
    }
  }

  override def paragraph(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARB)) {
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      variableDefine()
      innerText()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARE)) {
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      } else {
        while(!Tree.isEmpty)
          println(Tree.pop())
        println("SYNTAX ERROR : Need an ending token for paragraph")
        System.exit(1)
      }
    }
  }

  override def innerText(): Unit = {
    Compiler.currentToken match{
      case CONSTANTS.DEFB => variableDefine(); innerText()
      case CONSTANTS.USEB => variableUse(); innerText()
      case CONSTANTS.HEADING => heading(); innerText()
      case CONSTANTS.BOLD => bold(); innerText()
      case CONSTANTS.ITALICS =>italics(); innerText()
      case CONSTANTS.LISTITEM =>listItem(); innerText()
      case CONSTANTS.IMAGEB =>image(); innerText()
      case CONSTANTS.LINKB => link(); innerText()
      case CONSTANTS.NEWLINE =>newline(); innerText()
      case _ =>{
        if(TextBool){
          text();
          innerText()
        }

      }
    }

  }

  override def heading(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if(TextBool) {
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }else {
        println("Syntax Error: Text Required")
        System.exit(1)
      }

    }
  }

  override def variableDefine(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)){
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if(TextBool) {
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }else {
        println("Syntax Error: Text Required")
        System.exit(1)
      }
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.EQSIGN)){
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }else{
        println("Syntax Error: Variable Define '=' not found")
        System.exit(1)}
      if(TextBool) {
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }else {
        println("Syntax Error: Text Required")
        System.exit(1)
      }
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        variableDefine()
      }else{
        println("Ending Bracket not found")
        System.exit(1)
      }
    }
  }

  override def variableUse(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)){
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if(TextBool){
        text()
      }else{
        println("Syntax Error: Variable Use Text Required")
        System.exit(1)
      }
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }else{
        println("Syntax Error: Variable Use Ending Tag Required")
        System.exit(1)
      }
    }
  }

  override def bold(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if(TextBool){
        text()
      }
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }else{
        println("Syntax Error: Bold Ending Tag Required")
        System.exit(1)
      }
    }
  }

  override def italics(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ITALICS)){
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if(TextBool){
        text()
      }
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ITALICS)){
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }else{
        println("Syntax Error: Italics Ending Tag Required")
        System.exit(1)
      }
    }
  }

  override def listItem(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)){
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      innerItem()
      listItem()
    }
  }

  override def innerItem(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)){
      variableUse()
      innerItem()
    }else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
      bold()
      innerItem()
    }else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ITALICS)){
      italics()
      innerItem()
    }else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)){
      link()
      innerItem()
    }else{
      if(TextBool) {
        text()
      }else{println("Syntax Error: Text Required"); System.exit(1)}
    }
  }

  override def link(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)){
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if(TextBool) {
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }else {
        println("Syntax Error: Text Required")
        System.exit(1)
      }
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }else{
        println("Syntax Error: Ending Bracket Required for Link")
        System.exit(1)
      }
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)){
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }else{
        println("Syntax Error: Address required after the description ")
        System.exit(1)
      }
      if(TextBool) {
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }else {
        println("Syntax Error: Text Required")
        System.exit(1)
      }
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)){
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        if(TextBool) {
          Tree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
        }else {
          println("Syntax Error: Text Required")
          System.exit(1)
        }
        if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)){
          Tree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
        }
      }else{
        println("Syntax Error: Ending Link Symbol required")
        System.exit(1)
      }

    }
  }

  override def image(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)){
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if(TextBool){
        text()
      }else{println("Syntax Error: Image Text Required");System.exit(1)}
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }else{println("Syntax Error: Image Ending Bracket Before Link Required");System.exit(1)}
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)){
        Tree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        if(TextBool){
          text()
        }else{println("Syntax Error: Image Address Required");System.exit(1)}
        if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)){
          Tree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
        }else{
          println("Syntax Error: Image Ending Address Token Required")
        }
      }else{println("Syntax Error: Image Address Begin Bracket Required")}
    }
  }

  override def newline(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)){
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
  }

  def text():Unit ={
    while(TextBool){
      Tree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
  }
  def retTree: scala.collection.mutable.Stack[String]={
    return Tree;
  }


}