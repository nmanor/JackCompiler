import java.io.{BufferedWriter, File, FileWriter}

import Exercise0.Main.getListOfFiles
import Exercise1.AInstructions._

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
    var asmCode =
      """@20
        |D=A
        |@0
        |M=D
        |""".stripMargin
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
        case "lt" => asmCode += lt(lineNum)
        case "and" => asmCode += and()
        case "or" => asmCode += or()
        case "not" => asmCode += not()
        case "push" => asmCode += push(words(1), words(2).toInt, file.getName)
        case _ => throw new Exception("Unknown VM instruction at line " + lineNum)
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
