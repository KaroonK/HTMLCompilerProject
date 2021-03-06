package edu.towson.cosc.cosc455.kkhati1.project1
import scala.collection.mutable
import scala.io.BufferedSource
import scala.io.Source._
/**
  * Created by kkhati1 on 10/11/2016.
  */
object  Compiler {
  var fileContents : String = ""
  var currentToken : String = ""
  val Scanner = new LexAnalyzer
  val Parser = new SynAnalyzer
  val Syman = new SemAnalyzer
  var position: Int = -1
  def main(args: Array[String]) = {

    checkFile(args)
    readFile(args(0))

    Scanner.getNextToken()
    Parser.gittex()
    //while(!Parser.Tree.isEmpty)
      //println(Parser.Tree.pop())
    Syman.convertCode()
    //Semantic Analyzer do your thing

  }
  def readFile(file : String) = {
    var source = scala.io.Source.fromFile(file)
    fileContents = try source.mkString finally source.close()
  }

  def checkFile(args: Array[String]) = {
    if (args.length != 1 ){
      println("Usage Error: wrong number of args fool!")
      System.exit(1)
    }
    else if(!args(0).endsWith((".mkd"))){
      println("Usage Error: wrong extension fool!")
      System.exit(1)
    }
  }
}

