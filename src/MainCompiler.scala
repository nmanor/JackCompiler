import java.io.{BufferedWriter, File, FileWriter}

import Exercise0.Main.getListOfFiles
import Exercise1.AInstructions._
import Exercise2.MInstructions._

import scala.io.{Source, StdIn}

object MainCompiler {
  def main(args: Array[String]): Unit = {
    print("Enter the path of the directory: ")
    val path = StdIn.readLine()
    for (file <- getListOfFiles(path)) {
      compileToASM(file)
    }
  }

  def compileToASM(file: File): Unit = {
    var asmCode = ""
    var lineNum = 0
    val fileContent = Source.fromFile(file)

    // foreach line in the file do:
    for (line <- fileContent.getLines()) {
      // update the line number
      lineNum += 1

      // split the line into separated words
      val words = line.split(" ")

      // call to the right function and add it to the asm code
      words(0) match {
        case "add" => asmCode += add()
        case "sub" => asmCode += sub()
        case "neg" => asmCode += neg()
        case "eq" => asmCode += eql(lineNum)
        case "gt" => asmCode += gt(lineNum)
        case "lt" => asmCode += lt(lineNum)
        case "and" => asmCode += and()
        case "or" => asmCode += or()
        case "not" => asmCode += not()
        case "pop" => asmCode += pop(words(1), words(2).toInt, file.getName, lineNum)
        case "push" => asmCode += push(words(1), words(2).toInt, file.getName, lineNum)
        case "call" => asmCode += call(words(1), words(2).toInt)
        case "function" => asmCode += function(words(1), words(2).toInt, lineNum)
        case "label" => asmCode += label(file,words(1))
        case "goto" => asmCode += label(file,words(1))
        case "if-goto" => asmCode += label(file,words(1))
        case "//" | "" =>
        case _ => throw new Exception("Unknown VM instruction at line " + lineNum + " in " + file.getName)
      }
    }

    // close the open file
    fileContent.close()

    // create the ASM file and open it for writing
    val asmFile = new File(file.getPath.replace(".vm", ".asm"))
    val writer = new BufferedWriter(new FileWriter(asmFile))

    // write the content
    writer.write(asmCode)
    writer.close()
  }

}
