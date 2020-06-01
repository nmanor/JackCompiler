package Exercise4

import java.io._

import scala.io.Source
import scala.util.matching.Regex

object Tokenizing {
  val keyword: Regex = new Regex("^class|^constructor|^function|^method|^field|^static|^var|^int|^char" +
    "|^boolean|^void|^true|^false|^null|^this|^let|^do|^if|^else|^while|^return")
  val symbol: Regex = new Regex("^\\{|^\\}|^\\(|^\\)|^\\[|^\\]|^\\.|^\\,|^\\;|^\\+|^\\-|^\\*|^\\/|^\\&|^\\||^\\<|^\\>|^\\=|^\\~")
  val integerConstant: Regex = new Regex("^(3276[0-7]|327[0-5][0-9]|32[0-6][0-9]{2}|3[01][0-9]{3}|[12][0-9]{4}|999[0-9]|99[0-8][0-9]|9[0-8][0-9]{2}|[1-8][0-9]{3}|99[0-9]|9[0-8][0-9]|[1-8][0-9]{2}|9[0-9]|[1-8][0-9]|[0-9])")
  val stringConstant: Regex = new Regex("^\"[^\"]*\"")
  val identifier: Regex = new Regex("^[\\w_][\\w\\d_]*")
  val whiteSpaces: Regex = new Regex("\\s")


  // The main Tokenizing function: get the path to the directory and plot the xxxT.xml file
  def tokenizer(file: File, path: String): Unit = {
    val fileContent = Source.fromFile(file)
    var code = fileContent.mkString
    fileContent.close()

    code = removeComments(code)
    code = code.replaceAll("[\n\r]", "")
    val tokens = findTokens(code)
    tokensListToXML(tokens, file, path)
  }


  // Function that remove all kind of comments from the code
  def removeComments(code: String): String = {
    val regex = new Regex("//.*")
    var newCode = regex.replaceAllIn(code, "")

    while (newCode.indexOf("/*") != -1) {
      val startIndex = newCode.indexOf("/*")
      val endIndex = newCode.indexOf("*/") + 2
      val comment = newCode.substring(startIndex, endIndex)
      newCode = newCode.replace(comment, "")
    }

    newCode
  }


  // Function that breaks down the code into Tokens and returns a list of (Token name, Token type)
  def findTokens(code: String): List[(String, String)] = {
    var newCode = code
    var result: List[(String, String)] = List()

    while (newCode.length > 0) {
      if (keyword.findFirstIn(newCode).toList.nonEmpty) {
        val token = keyword.findFirstIn(newCode).toList.head
        result = result :+ ((token, "keyword"))
        newCode = newCode.substring(token.length)
      }
      else if (symbol.findFirstIn(newCode).toList.nonEmpty) {
        val token = symbol.findFirstIn(newCode).toList.head
        result = result :+ ((token, "symbol"))
        newCode = newCode.substring(token.length)
      }
      else if (integerConstant.findFirstIn(newCode).toList.nonEmpty) {
        val token = integerConstant.findFirstIn(newCode).toList.head
        result = result :+ ((token, "integerConstant"))
        newCode = newCode.substring(token.length)
      }
      else if (stringConstant.findFirstIn(newCode).toList.nonEmpty) {
        val token = stringConstant.findFirstIn(newCode).toList.head
        result = result :+ ((token.substring(1, token.length - 1), "stringConstant"))
        newCode = newCode.substring(token.length)
      }
      else if (identifier.findFirstIn(newCode).toList.nonEmpty) {
        val token = identifier.findFirstIn(newCode).toList.head
        result = result :+ ((token, "identifier"))
        newCode = newCode.substring(token.length)
      }
      else if (whiteSpaces.findFirstIn(newCode).toList.nonEmpty) {
        val token = whiteSpaces.findFirstIn(newCode).toList.head
        newCode = newCode.substring(token.length)
      }
    }

    result
  }


  // Function that accepts tokens list and exports them to XML
  def tokensListToXML(tokens: List[(String, String)], file: File, path: String): Unit = {
    val xml = "<tokens>\n" + {
      tokens.map(token => "<" + token._2 + "> " + token._1
        .replaceAll("&", "&amp;")
        .replaceAll("<", "&lt;")
        .replaceAll(">", "&gt;")
        .replaceAll("\"", "&quet;")
        + " </" + token._2 + ">\n").mkString
    } + "</tokens>\n"
    val xmlfile = new File(path + "\\" + file.getName.split('.').head + "_examT.xml")
    val bw = new BufferedWriter(new FileWriter(xmlfile))
    bw.write(xml)
    bw.close()
  }

}
