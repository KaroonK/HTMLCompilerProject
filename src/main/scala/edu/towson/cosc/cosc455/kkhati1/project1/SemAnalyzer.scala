package edu.towson.cosc.cosc455.kkhati1.project1

import java.awt.Desktop
import java.io.{File, IOException, PrintWriter}

import scala.collection.mutable

/**
  * Created by Karoon on 11/13/2016.
  */
class SemAnalyzer {
  val Tree = Compiler.Parser.Tree
  val resTree = Tree.clone()
  var varStack = new mutable.Stack[String]
  var resVarStack = new mutable.Stack[String]
  var printTree : List[String] = Nil
  var varOn = ""
  def varScope(): Unit ={
      var previous, current, next = ""
    println(resTree.length)
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
    }
  def convertCode(): Unit = {
    varScope()
    println(resTree.length)
    while(!resTree.isEmpty){
      println(resTree.pop())
    }
    //var current = resTree.pop()
    /*while(!resTree.isEmpty){
      current match {
        case CONSTANTS.DOCB => printTree = "<DOCTYPE html>\n<html>\n<head>\n" ::printTree; current = resTree.pop();
        case CONSTANTS.DOCE => printTree = "</body>\n</html>"::printTree;  current = resTree.pop();
        case CONSTANTS.TITLEB => printTree = "<title>"::printTree; varOn = "title";  current = resTree.pop();
        case CONSTANTS.BRACKETE => {
          if(varOn.equalsIgnoreCase("title")){printTree = "</title>"::printTree}
          else if(varOn.equalsIgnoreCase("links")){printTree = "</a>"::printTree}
          current = resTree.pop();
        }case CONSTANTS.HEADING => printTree = "<h1>"::printTree; current = resTree.pop();
        case CONSTANTS.PARB => printTree = "<p>\n"::printTree; current = resTree.pop();
        case CONSTANTS.PARE => printTree = "</p>\n"::printTree; current = resTree.pop();
        case CONSTANTS.BOLD =>
      }
    }*/
  }
}
