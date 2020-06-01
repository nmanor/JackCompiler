import java.io.{BufferedWriter, File, FileWriter}

import Exercise0.Main.getListOfFiles
import Exercise1.AInstructions._
import Exercise2.MInstructions._
import Exercise4.Tokenizing._

import scala.io.{Source, StdIn}

object MainCompiler {
  def main(args: Array[String]): Unit = {
    print("Enter the path of the directory: ")
    val path = StdIn.readLine()
    var listOfFiles = getListOfFiles(path, "vm")
    var asmCode = ""
    if (listOfFiles.length > 1) {
      asmCode = bootstrap()
    }

    for (file <- listOfFiles) {
      asmCode += compileToASM(file)
    }

    // create the ASM file and open it for writing
    val asmFile = new File(path + "\\" + new File(path).getName + ".asm")
    val writer = new BufferedWriter(new FileWriter(asmFile))

    // write the content
    writer.write(asmCode)
    writer.close()

    listOfFiles = getListOfFiles(path, "jack")
    for (file <- listOfFiles) {
      tokenizer(file, path)
    }
  }

  def compileToASM(file: File): String = {
    var asmCode = ""
    var lineNum = 0
    val fileContent = Source.fromFile(file)

    // foreach line in the file do:
    for (line <- fileContent.getLines()) {
      // update the line number
      lineNum += 1

      // split the line into separated words
      val words = line.split("\\s+")

      // call to the right function and add it to the asm code
      words(0) match {
        case "add" => asmCode += "// " + line + "\n" + add()
        case "sub" => asmCode += "// " + line + "\n" + sub()
        case "neg" => asmCode += "// " + line + "\n" + neg()
        case "eq" => asmCode += "// " + line + "\n" + eql(lineNum)
        case "gt" => asmCode += "// " + line + "\n" + gt(lineNum)
        case "lt" => asmCode += "// " + line + "\n" + lt(lineNum)
        case "and" => asmCode += "// " + line + "\n" + and()
        case "or" => asmCode += "// " + line + "\n" + or()
        case "not" => asmCode += "// " + line + "\n" + not()
        case "pop" => asmCode += "// " + line + "\n" + pop(words(1), words(2).toInt, file.getName, lineNum)
        case "push" => asmCode += "// " + line + "\n" + push(words(1), words(2).toInt, file.getName, lineNum)
        case "call" => asmCode += "// " + line + "\n" + call(words(1), words(2).toInt)
        case "function" => asmCode += "// " + line + "\n" + function(words(1), words(2).toInt)
        case "return" => asmCode += "// " + line + "\n" + retrn()
        case "label" => asmCode += "// " + line + "\n" + label(file.getName, words(1))
        case "goto" => asmCode += "// " + line + "\n" + goto(file.getName, words(1))
        case "if-goto" => asmCode += "// " + line + "\n" + ifgoto(file.getName, words(1))
        case "//" | "" =>
        case _ => throw new Exception("Unknown VM instruction at line " + lineNum + " in " + file.getName)
      }
    }

    // close the open file
    fileContent.close()

    // return the asm code of the current file
    asmCode
  }

}
