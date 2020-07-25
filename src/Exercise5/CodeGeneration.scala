package Exercise5

import java.io.{BufferedWriter, File, FileWriter}

import Exercise4.TreeComponents.{Rule, Terminal}

object CodeGeneration {

  var vmCode: String = _
  var className: String = _
  var subroutineName: String = _
  var subroutineKind: String = _
  var labelsCounter: Int = _
  var classTable: SymbolTable = _
  var methodTable: SymbolTable = _

  def generateCode(syntaxTree: Rule, file: File, path: String): Unit = {
    vmCode = ""
    classTable = SymbolTable()

    generateClass(syntaxTree)
    exportToVMFile(file, path)
  }

  // Function that exports the VM code to VM file
  def exportToVMFile(file: File, path: String): Unit = {
    val xmlFile = new File(path + "\\" + file.getName.split('.').head + ".vm")
    val bw = new BufferedWriter(new FileWriter(xmlFile))
    bw.write(vmCode)
    bw.close()
  }

  //--
  def generateClass(rule: Rule): Unit = {
    var index = 1

    // Save the name of the class
    className = rule(index).asInstanceOf[Terminal].terminal
    index += 2

    // Generate the code for all the class vars
    while (rule(index).ruleType == "classVarDec") {
      generateClassVarDec(rule(index))
      index += 1
    }

    // Generate the code for all the subroutine
    while (rule(index).ruleType == "subroutineDec") {
      generateSubroutineDec(rule(index))
      index += 1
    }
  }

  //--
  def generateClassVarDec(rule: Rule): Unit = {
    val varKind = rule(0).asInstanceOf[Terminal].terminal
    val varType = rule(1).asInstanceOf[Terminal].terminal
    var varName = rule(2).asInstanceOf[Terminal].terminal

    // Add the var to the symbol table
    classTable += (varName, varType, varKind)

    // As long as there is a , add more vars
    var index = 3
    while (rule(index).asInstanceOf[Terminal].terminal == ",") {
      index += 1
      varName = rule(index).asInstanceOf[Terminal].terminal
      index += 1
      classTable += (varName, varType, varKind)
    }
  }

  //--
  def generateSubroutineDec(rule: Rule): Unit = {
    // Initialize the symbol table and the subroutine name
    methodTable = SymbolTable(classTable)
    subroutineKind = rule(0).asInstanceOf[Terminal].terminal
    subroutineName = rule(2).asInstanceOf[Terminal].terminal

    // Add the pointer to this if needed
    if (subroutineKind == "method")
      methodTable += ("this", className, "argument")

    // Add the argument to the table
    generateParameterList(rule(4))

    // Generate the rest of the code
    generateSubroutineBody(rule(6))
  }

  //--
  def generateParameterList(rule: Rule): Unit = {
    var index = 0
    // Add the var to the symbol table
    while (rule(index) != null) {
      val varType = rule(index).asInstanceOf[Terminal].terminal
      index += 1
      val varName = rule(index).asInstanceOf[Terminal].terminal
      index += 2
      methodTable += (varName, varType, "argument")
    }
  }

  //--
  def generateSubroutineBody(rule: Rule): Unit = {
    var index = 1

    // Add the vars of the subroutine to the table
    while (rule(index).ruleType == "varDec") {
      generateVarDec(rule(index))
      index += 1
    }

    // Add the signature of the function in VM
    vmCode += "function " + className + "." + subroutineName + " " + methodTable.varCount("var") + "\n"

    // Add the constructor allocation
    if (subroutineKind == "constructor") {
      vmCode += "push constant " + classTable.varCount("field") + "\n"
      vmCode += "call Memory.alloc 1\n"
      vmCode += "pop pointer 0\n"
    }

    // Add the method allocation
    if (subroutineKind == "method") {
      vmCode += "push " + methodTable("this") + "\n"
      vmCode += "pop pointer 0\n"
    }

    // Add the statements of the subroutine
    generateStatements(rule(index))
  }

  //--
  def generateVarDec(rule: Rule): Unit = {
    val varType = rule(1).asInstanceOf[Terminal].terminal
    var varName = rule(2).asInstanceOf[Terminal].terminal

    // Add the var to the symbol table
    methodTable += (varName, varType, "var")

    // As long as there is a , add more vars
    var index = 3
    while (rule(index).asInstanceOf[Terminal].terminal == ",") {
      index += 1
      varName = rule(index).asInstanceOf[Terminal].terminal
      index += 1
      methodTable += (varName, varType, "var")
    }
  }

  //--
  def generateStatements(rule: Rule): Unit = {
    // Generate code for each of the statements
    var index = 0
    while (rule(index) != null) {
      generateStatement(rule(index))
      index += 1
    }
  }

  //--
  def generateStatement(rule: Rule): Unit = {
    rule.ruleType match {
      case "letStatement" => generateLetStatement(rule)
      case "ifStatement" => generateIfStatement(rule)
      case "whileStatement" => generateWhileStatement(rule)
      case "doStatement" => generateDoStatement(rule)
      case "returnStatement" => generateReturnStatement(rule)
    }
  }


  //--
  def generateIfStatement(rule: Rule): Unit = {
    // Generate the code for the expression
    generateExpression(rule(2))

    // Set the labels
    val falseLabel = "IF_FALSE_" + labelsCounter
    labelsCounter += 1
    val endLabel = "IF_END_" + labelsCounter
    labelsCounter += 1

    // If the condition doesn't hold, go to the label FALSE
    vmCode += "not\n"
    vmCode += "if-goto " + falseLabel + "\n"

    // If the condition does exist, compile the inner statements
    generateStatements(rule(5))
    vmCode += "goto " + endLabel + "\n"

    // the statements in case the condition doesn hold
    vmCode += "label " + falseLabel + "\n"
    if (rule(7) != null)
      generateStatements(rule(9))

    // Add the end label
    vmCode += "label " + endLabel + "\n"
  }

  //--
  def generateLetStatement(rule: Rule): Unit = {
    // Get the name of the var to the assignment
    val varName = rule(1).asInstanceOf[Terminal].terminal

    var index = 2

    // If the var is array:
    if (rule(index).asInstanceOf[Terminal].terminal == "[") {
      // Generate the indexer expression
      index += 1
      generateExpression(rule(index))

      // Calculate the required value address
      vmCode += "push " + methodTable(varName) + "\n"
      vmCode += "add\n"

      // Generate the expression to the assignment
      index += 3
      generateExpression(rule(index))

      // Store the result
      vmCode += "pop temp 0\n" // Store assigned value in temp
      vmCode += "pop pointer 1\n" // Restore destination
      vmCode += "push temp 0\n" // Restore assigned value
      vmCode += "pop that 0\n" // Store in target
    }

    // Var is not array:
    else {
      // Generate the expression to the assignment
      index += 1
      generateExpression(rule(index))

      // Store the result
      vmCode += "pop " + methodTable(varName) + "\n"
    }
  }

  //--
  def generateWhileStatement(rule: Rule): Unit = {
    // Set the labels
    val whileLabel = "WHILE_" + labelsCounter
    labelsCounter += 1
    val falseLabel = "FALSE_" + labelsCounter
    labelsCounter += 1

    // Generate the label of the while
    vmCode += "label " + whileLabel + "\n"

    // Generate the code of the condition
    generateExpression(rule(2))

    // If the condition doesn't hold, go to the label FALSE
    vmCode += "not\n"
    vmCode += "if-goto " + falseLabel + "\n"

    // Else, do the statements and go back to the condition
    generateStatements(rule(5))
    vmCode += "goto " + whileLabel + "\n"

    // Finally, generate the false label
    vmCode += "label " + falseLabel + "\n"
  }

  //--
  def generateDoStatement(rule: Rule): Unit = {
    // Call generateSubroutineCall and then pop temp 0
    generateSubroutineCall(rule(1))
    vmCode += "pop temp 0\n"
  }

  //--
  def generateReturnStatement(rule: Rule): Unit = {
    // If the return statement have expression
    if (rule(1).ruleType == "expression")
      generateExpression(rule(1))

    // Else, return 0
    else
      vmCode += "push constant 0\n"

    vmCode += "return\n"
  }

  //--
  def generateExpression(rule: Rule): Unit = {
    generateTerm(rule(0))

    // Generate terms and then add the operator between them
    var index = 1
    while (rule(index) != null) {
      val op = rule(index).asInstanceOf[Terminal].terminal
      index += 1
      generateTerm(rule(index))
      index += 1
      op match {
        case "+" => vmCode += "add\n"
        case "-" => vmCode += "sub\n"
        case "*" => vmCode += "call Math.multiply 2\n"
        case "/" => vmCode += "call Math.divide 2\n"
        case "&" => vmCode += "and\n"
        case "|" => vmCode += "or\n"
        case "<" => vmCode += "lt\n"
        case ">" => vmCode += "gt\n"
        case "=" => vmCode += "eq\n"
      }
    }
  }

  //--
  def generateTerm(rule: Rule): Unit = {
    rule(0) match {
      case terminal: Terminal =>
        // Unary operator, compile the term after the operator
        if (terminal.terminal == "~" || terminal.terminal == "-") {
          // Generate the code of the term
          generateTerm(rule(1))

          // Generate the code of the unary op
          if (terminal.terminal == "~")
            vmCode += "not\n"
          else
            vmCode += "neg\n"
        }

        // If the rule is another expression surrounded by ()
        else if (terminal.terminal == "(")
          generateExpression(rule(1))

        // If the rule is integerConstant, push it to the stack
        else if (rule(0).ruleType == "integerConstant")
          vmCode += "push constant " + terminal.terminal + "\n"

        // If the rule is stringConstant, push it to the stack char by char
        else if (rule(0).ruleType == "stringConstant") {
          val string = terminal.terminal

          // Call the constructor of String with the length of the string
          vmCode += "push constant " + string.length + "\n"
          vmCode += "call String.new 1\n"

          // Append all the decimal values of the chars in the string
          for (char <- string) {
            vmCode += "push constant " + char.toInt + "\n"
            vmCode += "call String.appendChar 2\n"
          }
        }

        // If the word is keyword
        else if (rule(0).ruleType == "keyword") {
          // If the word is 'this', push 'this'
          if (terminal.terminal == "this")
            vmCode += "push pointer 0\n"

          else {
            // Push false / null
            vmCode += "push constant " + 0 + "\n"
            // Change to true of needed
            if (terminal.terminal == "true")
              vmCode += "not\n"
          }
        }

        // In case of a variable name
        else if (rule(0).ruleType == "identifier") {
          var index = 0

          // Save the name of the var
          val varName = rule(index).asInstanceOf[Terminal].terminal
          index += 1

          // If the var is array
          if (rule(index) != null && rule(index).asInstanceOf[Terminal].terminal == "[") {
            index += 1
            // Add the expression of the indexer
            generateExpression(rule(index))
            index += 1
            vmCode += "push " + methodTable(varName) + "\n"
            vmCode += "add\n"

            // Rebase 'that' to point to var+index
            vmCode += "pop pointer 1\n"
            vmCode += "push that 0\n"
          }
          else {
            vmCode += "push " + methodTable(varName) + "\n"
          }
        }
      case _ =>
        generateSubroutineCall(rule(0))
    }
  }

  //--
  def generateSubroutineCall(rule: Rule): Unit = {
    // By default, the func name is belong to 'this' class
    var funcName = rule(0).asInstanceOf[Terminal].terminal
    var funcClass = className
    var defaultCall = true
    var argsCount = 0
    var index = 1

    // If the funcName its actually a var name
    if (rule(index).asInstanceOf[Terminal].terminal == ".") {
      index += 1

      // Don't call the function as function of 'this' class
      defaultCall = false

      // We need to know if we working with variable of the class or with another class
      val varName = funcName
      funcName = rule(index).asInstanceOf[Terminal].terminal
      index += 2

      // If its really a var
      if (methodTable.contains(varName)) {
        // Get the class of the varName
        funcClass = methodTable.getType(varName)

        // Add 'this' to the args
        argsCount = 1

        // Push "this"
        vmCode += "push " + methodTable(varName) + "\n"
      }

      // Else, the varName its a class name
      else {
        funcClass = varName
      }
    }
    else index += 1

    // Default call is a method one, push this
    if (defaultCall) {
      argsCount = 1
      vmCode += "push pointer 0\n"
    }

    argsCount += generateExpressionList(rule(index))
    vmCode += "call " + funcClass + "." + funcName + " " + argsCount + "\n"
  }

  //--
  def generateExpressionList(rule: Rule): Int = {
    // Count expressions
    var count = 0
    var index = 0

    // While there is expressions, add them
    while (rule(index) != null) {
      count += 1
      generateExpression(rule(index))
      index += 2
    }

    count
  }
}
