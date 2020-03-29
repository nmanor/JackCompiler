import Exercise0.Main.getListOfFiles

import scala.io.StdIn

object MainCompiler {
  def main(args: Array[String]): Unit = {
    print("Enter the path of the directory: ")
    val path = StdIn.readLine()
    var i = 1

    for (file <- getListOfFiles(path)) {
      // TODO: convert the file from VM file to ASM file
      println("File")
    }
  }
}
