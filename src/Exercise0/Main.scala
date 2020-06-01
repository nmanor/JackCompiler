package Exercise0

import java.io.{BufferedWriter, File, FileWriter}

import scala.io.{Source, StdIn}

/**
 * Concepts of Programming Languages
 * Exercise 0
 * Michael Goldmeier (315132027) and Natan Manor (207676511)
 */

object Main {
  def main(args: Array[String]): Unit = {
    print("Enter the path of the directory: ")
    val path = StdIn.readLine()
    var i = 1
    for (file <- getListOfFiles(path, "vm")) {
      println(file.getName)
      if (file.getName == "hello.vm")
        moveVMtoASM(file)
      else
        addNumberToFile(file, i)
      i += 1
    }
  }

  /**
   * @param path : The path of the directory with all the VM files
   * @return List of File that ends with .vm
   */
  def getListOfFiles(path: String, kind: String): List[File] = {
    // open the path as File
    val dir = new File(path)

    // check if the path is exists and its a directory
    if (!dir.exists() || !dir.isDirectory)
      throw new Exception("The path does not match")

    // get list of Files in the path
    val allFiles = dir.listFiles.filter(_.isFile).toList

    // return only the files that ends with ".vm"
    allFiles.filter(file => file.getName.endsWith("." + kind))
  }


  /**
   * The function adding num the the file content (at the end)
   *
   * @param file : The file to add the num for
   * @param num  : The number to add
   */
  def addNumberToFile(file: File, num: Int): Unit = {
    // read the content of the file
    val source = Source.fromFile(file)
    val content = try source.mkString finally source.close()

    // open the file for writing
    val writer = new BufferedWriter(new FileWriter(file))

    // write the content and add the num to it
    writer.write(content + " " + num)
    writer.close()
  }

  /**
   * The function copying the hell.vm to hello.asm and print every line starts with "you" or "are"
   * @param file: The original hello.vm file
   */
  def moveVMtoASM(file: File): Unit ={
    // set the content to be empty string
    var content = ""

    // run over every line in the file:
    for(line <- Source.fromFile(file).getLines())
      {
        // add the line to the content
        content = content + "\n" + line

        // if the line starts with "you" or "are", print it
        if(line.toLowerCase.startsWith("you") || line.toLowerCase.startsWith("are"))
          println(line)
      }

    // create the ASM file and open it for writing
    val ASMfile = new File(file.getPath.replace(".vm", ".asm"))
    val writer = new BufferedWriter(new FileWriter(ASMfile))

    // write the content
    writer.write(content.slice(1, content.length))
    writer.close()
  }

}
