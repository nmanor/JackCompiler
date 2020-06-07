package Exercise4

import java.io.{BufferedWriter, File, FileWriter}

import Exercise4.TreeComponents.{NonTerminal, Rule, Terminal}

object Parsing {

  var currentToken: Int = 0
  var tokensList: List[(String, String)] = List()


  def parse(tokensList: List[(String, String)], file: File, path: String): Rule = {
    this.tokensList = tokensList
    this.currentToken = 0
    val tree = classRule()
    syntaxTreeToXml(tree, file, path)
    tree
  }


  // Function that returns the next token in the list, or null if the list is over
  // Promotes the pointer
  def popToken(): (String, String) = {
    if (currentToken == tokensList.length)
      return null
    currentToken += 1
    tokensList(currentToken - 1)
  }


  // Function that returns the next token in the list, or null if the list is over
  // Does not advance the pointer
  def topToken(): (String, String) = {
    if (currentToken == tokensList.length)
      return null
    tokensList(currentToken)
  }

  // Function that returns the previous token in the list, or null if the list is over
  // Rewound the pointer
  def pushToken(): (String, String) = {
    if (currentToken == 0)
      return null
    currentToken -= 1
    tokensList(currentToken + 1)
  }


  // Function that returns if the token matches one of the received tokens
  def matchToken(token: (String, String), expectedTokens: String*): Boolean = {
    for (expectedToken <- expectedTokens)
      if (token._1 == expectedToken)
        return true
    false
  }


  // Function that accepts syntax tree and exports it to XML
  def syntaxTreeToXml(syntaxTree: Rule, file: File, path: String): Unit = {
    val prettyPrinter = new scala.xml.PrettyPrinter(80, 2)
    // val xml = prettyPrinter.format(scala.xml.XML.loadString(rule.getXmlContent))
    val xmlFile = new File(path + "\\" + file.getName.split('.').head + "_.xml")
    val bw = new BufferedWriter(new FileWriter(xmlFile))
    // bw.write(xml)
    bw.write(syntaxTree.getXmlContent)
    bw.close()
  }


  //--
  def classRule(): Rule = {
    val root = new NonTerminal("class")

    // Add the token for "class"
    var token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    // Add the token for the class name
    root.addSubRule(classNameRule())

    // Add the token for the { sign
    token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    // Add the fields of the class
    while (matchToken(topToken(), "static", "field")) {
      root.addSubRule(classVarDecRule())
    }

    // Add the subroutines of the class
    while (matchToken(topToken(), "constructor", "function", "method")) {
      root.addSubRule(subroutineDecRule())
    }

    // Add the token for the } sign
    token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    root
  }

  //--
  def classVarDecRule(): Rule = {
    val root = new NonTerminal("classVarDec")

    // Add the token for "static" or "field"
    var token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    // Add the token for the var type
    root.addSubRule(typeRule())

    // Add the token for the var name
    root.addSubRule(varNameRule())

    // As long as there is a , add more vars
    token = popToken()
    while (matchToken(token, ",")) {
      root.addSubRule(new Terminal(token._2, token._1))
      root.addSubRule(varNameRule())
      token = popToken()
    }

    // Add the ; sign
    root.addSubRule(new Terminal(token._2, token._1))

    root
  }

  //--
  def typeRule(): Rule = {
    if (matchToken(topToken(), "int", "char", "boolean")) {
      val token = popToken()
      return new Terminal(token._2, token._1)
    }
    classNameRule()
  }

  //--
  def subroutineDecRule(): Rule = {
    val root = new NonTerminal("subroutineDec")

    // Add the token for "constructor", "function" or "method"
    var token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    // Add the void / type token
    if (matchToken(topToken(), "void")) {
      token = popToken()
      root.addSubRule(new Terminal(token._2, token._1))
    } else {
      root.addSubRule(typeRule())
    }

    // Add the subroutineName token
    root.addSubRule(subroutineNameRule())

    // Add the token for the ( sign
    token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    // Add the parameterList
    root.addSubRule(parameterList())

    // Add the token for the ) sign
    token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    // Add the subroutineBody
    root.addSubRule(subroutineBodyRule())

    root
  }

  //--
  def subroutineBodyRule(): Rule = {
    val root = new NonTerminal("subroutineBody")

    // Add the { sign
    var token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    while (matchToken(topToken(), "var")) {
      // Add the varDec
      root.addSubRule(varDecRule())
    }

    // Add the statements
    root.addSubRule(statementsRule())

    // Add the } sign
    token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    root
  }

  //--
  def parameterList(): Rule = {
    val root = new NonTerminal("parameterList")
    var token = topToken()

    // Add the list of parameters if their is parameter
    if (matchToken(token, "int", "char", "boolean") || token._2 == "identifier") {

      // Add the type token
      root.addSubRule(typeRule())

      // Add the varName token
      root.addSubRule(varNameRule())

      // As long as there is a , add more vars
      while (matchToken(topToken(), ",")) {
        token = popToken()
        root.addSubRule(new Terminal(token._2, token._1))
        root.addSubRule(typeRule())
        root.addSubRule(varNameRule())
      }
    }

    root
  }

  //--
  def varDecRule(): Rule = {
    val root = new NonTerminal("varDec")

    // Add the token for "var"
    var token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    // Add the token for type
    root.addSubRule(typeRule())

    // Add the token for the first var name
    root.addSubRule(varNameRule())

    // As long as there is a , add more vars
    while (matchToken(topToken(), ",")) {
      token = popToken()
      root.addSubRule(new Terminal(token._2, token._1))
      root.addSubRule(varNameRule())
    }

    // Add the ; sign
    token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    root
  }

  //--
  def classNameRule(): Rule = {
    val token = popToken()
    new Terminal(token._2, token._1)
  }

  //--
  def subroutineNameRule(): Rule = {
    val token = popToken()
    new Terminal(token._2, token._1)
  }

  //--
  def varNameRule(): Rule = {
    val token = popToken()
    new Terminal(token._2, token._1)
  }


  //--
  def statementsRule(): Rule = {
    val root = new NonTerminal("statements")

    // Add zero or more statements
    while (matchToken(topToken(), "let", "if", "while", "do", "return"))
      root.addSubRule(statementRule())

    root
  }

  //--
  def statementRule(): Rule = {
    // Add the right rule
    val token = topToken()
    if (matchToken(token, "let"))
      letStatementRule()
    else if (matchToken(token, "if"))
      ifStatementRule()
    else if (matchToken(token, "while"))
      whileStatementRule()
    else if (matchToken(token, "do"))
      doStatementRule()
    else
      returnStatementRule()
  }

  //--
  def letStatementRule(): Rule = {
    val root = new NonTerminal("letStatement")

    // Add the "let"
    var token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    // Add the varName
    root.addSubRule(varNameRule())

    // Add [expression] if needed
    if (matchToken(topToken(), "[")) {
      // Add the [ sign
      token = popToken()
      root.addSubRule(new Terminal(token._2, token._1))

      // Add the expression
      root.addSubRule(expressionRule())

      // Add the ] sign
      token = popToken()
      root.addSubRule(new Terminal(token._2, token._1))
    }

    // Add the = sign
    token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    // Add the expression
    root.addSubRule(expressionRule())

    // Add the ; sign
    token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    root
  }

  //--
  def ifStatementRule(): Rule = {
    val root = new NonTerminal("ifStatement")

    // Add the "if"
    var token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    // Add the ( sign
    token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    // add the expression
    root.addSubRule(expressionRule())

    // Add the ) sign
    token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    // Add the { sign
    token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    // add the expression
    root.addSubRule(statementsRule())

    // Add the } sign
    token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    // If else is needed, add it
    if (matchToken(topToken(), "else")) {
      // Add the "else"
      token = popToken()
      root.addSubRule(new Terminal(token._2, token._1))

      // Add the { sign
      token = popToken()
      root.addSubRule(new Terminal(token._2, token._1))

      // add the expression
      root.addSubRule(statementsRule())

      // Add the } sign
      token = popToken()
      root.addSubRule(new Terminal(token._2, token._1))
    }

    root
  }

  //--
  def whileStatementRule(): Rule = {
    val root = new NonTerminal("whileStatement")

    // Add the "while"
    var token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    // Add the ( sign
    token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    // Add the expression
    root.addSubRule(expressionRule())

    // Add the ) sign
    token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    // Add the { sign
    token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    // Add the expression
    root.addSubRule(statementsRule())

    // Add the } sign
    token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    root
  }

  //--
  def doStatementRule(): Rule = {
    val root = new NonTerminal("doStatement")

    // Add the "do"
    var token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    // Add subroutineCall
    root.addSubRule(subroutineCallRule())

    // Add the ; sign
    token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    root
  }

  //--
  def returnStatementRule(): Rule = {
    val root = new NonTerminal("returnStatement")

    // Add the "return"
    var token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    // Add expressionRule, if any
    if (!matchToken(topToken(), ";"))
      root.addSubRule(expressionRule())

    // Add the ; sign
    token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    root
  }


  //--
  def expressionRule(): Rule = {
    val root = new NonTerminal("expression")

    // Add the first term
    root.addSubRule(termRule())

    while (matchToken(topToken(), "+", "-", "*", "/", "&", "|", "<", ">", "=")) {
      // Add the op
      root.addSubRule(opRule())

      // Add the term
      root.addSubRule(termRule())
    }

    root
  }

  //--
  def termRule(): Rule = {
    val root = new NonTerminal("term")
    var token = topToken()

    // Add integerConstant or stringConstant
    if (token._2 == "integerConstant" || token._2 == "stringConstant") {
      token = popToken()
      root.addSubRule(new Terminal(token._2, token._1))
    }

    // Add keywordConstant
    else if (matchToken(token, "true", "false", "null", "this")) {
      root.addSubRule(keywordConstantRule())
    }

    // Add varName or subroutineCall
    else if (token._2 == "identifier") {
      // Look ahead
      popToken()

      // Check if need to call to subroutineCall
      // x = y();  or  x = y.z();  or  x = y[i];  or  x = y;
      if (matchToken(topToken(), "(", ".")) {
        pushToken()
        root.addSubRule(subroutineCallRule())
      }

      // Check if need to call to varName
      else {
        pushToken()
        root.addSubRule(varNameRule())

        if (matchToken(topToken(), "[")) {
          // Add the [ sign
          token = popToken()
          root.addSubRule(new Terminal(token._2, token._1))

          // Add the expression
          root.addSubRule(expressionRule())

          // Add the ] sign
          token = popToken()
          root.addSubRule(new Terminal(token._2, token._1))
        }
      }
    }

    // Add (expression)
    else if (matchToken(token, "(")) {
      // Add the ( sign
      token = popToken()
      root.addSubRule(new Terminal(token._2, token._1))

      // Add the expression
      root.addSubRule(expressionRule())

      // Add the ) sign
      token = popToken()
      root.addSubRule(new Terminal(token._2, token._1))
    }

    // Add unaryOp term
    else if (matchToken(token, "-", "~")) {
      root.addSubRule(unaryOpRule())
      root.addSubRule(termRule())
    }

    root
  }

  //--
  def subroutineCallRule(): Rule = {
    val root = new NonTerminal("subroutineCall", true)

    // Add the name of the subroutine / class / var name
    var token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    if (matchToken(topToken(), ".")) {
      // Add the . sign
      token = popToken()
      root.addSubRule(new Terminal(token._2, token._1))

      // Add the subroutineName
      root.addSubRule(subroutineNameRule())
    }

    // Add the ( sign
    token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    // Add the expressionList
    root.addSubRule(expressionListRule())

    // Add the ) sign
    token = popToken()
    root.addSubRule(new Terminal(token._2, token._1))

    root
  }

  //--
  def expressionListRule(): Rule = {
    val root = new NonTerminal("expressionList")
    if (!matchToken(topToken(), ")")) {
      // Add the first expression
      root.addSubRule(expressionRule())

      // Look for more expression
      while (matchToken(topToken(), ",")) {
        // Add the , sign
        val token = popToken()
        root.addSubRule(new Terminal(token._2, token._1))

        // Add the expression
        root.addSubRule(expressionRule())
      }
    }

    root
  }

  //--
  def opRule(): Rule = {
    // Create new terminal with the keyword
    val token = popToken()
    new Terminal(token._2, token._1)
  }

  //--
  def unaryOpRule(): Rule = {
    // Create new terminal with the keyword
    val token = popToken()
    new Terminal(token._2, token._1)
  }

  //--
  def keywordConstantRule(): Rule = {
    // Create new terminal with the keyword
    val token = popToken()
    new Terminal(token._2, token._1)
  }

}