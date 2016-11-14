package edu.towson.cosc.cosc455.kkhati1.project1

import java.awt.Desktop
import java.io.{File, IOException, PrintWriter}

import scala.collection.mutable

/**
  * Created by Karoon on 11/13/2016.
  */
class SemAnalyzer {
  val Tree = Compiler.Parser.Tree
  var varStack = new mutable.Stack[String]
  var printTree : List[String] = Nil
  var varOn = ""
/*   def varScope(): Unit ={
      var previous, current, next = ""
      if(!Tree.isEmpty){
        previous = Tree.pop()
        current = Tree.pop()
        next = Tree.pop()
        while(!Tree.isEmpty){
          if(current.equalsIgnoreCase(CONSTANTS.EQSIGN)){
            varStack.push(previous)
            varStack.push(next)
          }
          previous = current
          current = next
          next = Tree.pop()
        }
      }else{
        println("Semantics Error! Tree is Empty!")
      }
    }*/
  def convertCode(): Unit = {
    var resTree = Tree.clone().reverse
    var resVarStack = varStack.clone()
    //varScope()
    var current = resTree.pop()
    while(!resTree.isEmpty){
      current match {
        case CONSTANTS.DOCB => printTree = "<!DOCTYPE html>\n<html>\n<head>\n" ::printTree; current = resTree.pop();
        case CONSTANTS.DOCE => printTree = "</body>\n</html>"::printTree;  current = resTree.pop();
        case CONSTANTS.TITLEB =>{
          current = resTree.pop()
          var Text = current
          current = resTree.pop()
          printTree = "<body>\n"::"</head>\n"::"</title>\n"::Text::"<title>"::printTree
          current = resTree.pop()
        }
        case CONSTANTS.HEADING => {
          printTree = "<h1>"::printTree
          current = resTree.pop()
          printTree = current::printTree
          printTree = "</h1>\n"::printTree
          current = resTree.pop()
        }
        case CONSTANTS.PARB => printTree = "<p>\n"::printTree; current = resTree.pop();
        case CONSTANTS.PARE => printTree = "</p>\n"::printTree; current = resTree.pop();
        case CONSTANTS.BOLD => {
          printTree = "<b>"::printTree;
          current = resTree.pop();
          while(!current.equalsIgnoreCase(CONSTANTS.BOLD)){
            printTree = current::printTree
            current = resTree.pop()
          }
          printTree = "</b>\n"::printTree
          current = resTree.pop()
        }
        case CONSTANTS.ITALICS => {
          printTree = "<i>"::printTree
          current = resTree.pop()
          while(!current.equalsIgnoreCase(CONSTANTS.ITALICS)){
            printTree = current::printTree
            current = resTree.pop()
          }
          printTree = "</i>\n"::printTree
          current = resTree.pop()
        }
        case CONSTANTS.LISTITEM => {
          printTree = "<li>"::printTree
          current = resTree.pop()
          printTree = current::printTree
          current = resTree.pop()
        }
        case CONSTANTS.NEWLINE => printTree = "<br>\n"::printTree; current = resTree.pop();
        case CONSTANTS.LINKB => {
          current = resTree.pop()
          var Text = current
          current = resTree.pop()
          current = resTree.pop()
          current = resTree.pop()
          var Link = current
          printTree = "</a>\n"::Text::"\">"::Link::"<a href=\""::printTree
          current = resTree.pop()
          current = resTree.pop()
        }
        case CONSTANTS.IMAGEB => {
          current = resTree.pop()
          var Text = current
          current = resTree.pop()
          current = resTree.pop()
          current = resTree.pop()
          var Link = current
          printTree = "\">"::Text::" alt=\""::"\""::Link::"<img src=\""::printTree
          current = resTree.pop()
          current = resTree.pop()

        }
        case CONSTANTS.DEFB =>{
          current = resTree.pop()
          varStack.push(current)
          current = resTree.pop()
          current = resTree.pop()
          varStack.push(current)
          current = resTree.pop()
          current = resTree.pop()
        }
        case CONSTANTS.USEB =>{
          var resVarStack = varStack.clone()
          var foundBool = false
          var varInfo = resVarStack.pop()
          var varName = resVarStack.pop()
          current = resTree.pop()
          if(current.trim().equalsIgnoreCase(varName.trim())){
            foundBool = true
            printTree = " "::varInfo::printTree

          }
          if(!foundBool){
            println(current)
            println(varName)
            println("SEMANTIC ERROR: Variable Not Found!");
            System.exit(1)
          }
          current = resTree.pop()
          current = resTree.pop()
        }
        case _ => printTree = current::printTree; current = resTree.pop()
      }
    }
    if(current.equalsIgnoreCase(CONSTANTS.DOCE)) {
      printTree = "\n</body>\n</html>"::printTree;
    }
    printTree = printTree.reverse
    writeHTML(printTree.mkString)
    println(printTree.mkString)
    openHTMLFileInBrowser("Output.html")

  }
  def writeHTML(s : String): Unit = {
    val writer = new PrintWriter(new File("Output.html"))
    writer.write(s)
    writer.close()
  }

  /* * Hack Scala/Java function to take a String filename and open in default web browser. */
  def openHTMLFileInBrowser(htmlFileStr : String) = {
    val file : File = new File(htmlFileStr.trim)
    println(file.getAbsolutePath)
    if (!file.exists())
      sys.error("File " + htmlFileStr + " does not exist.")

    try {
      Desktop.getDesktop.browse(file.toURI)
    }
    catch {
      case ioe: IOException => sys.error("Failed to open file:  " + htmlFileStr)
      case e: Exception => sys.error("He's dead, Jim!")
    }
  }
}
