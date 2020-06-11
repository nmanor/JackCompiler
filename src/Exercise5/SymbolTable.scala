package Exercise5

case class SymbolTable(classTable: SymbolTable = null) {
  // Static, field, argument and var counters
  var varKinds: Map[String, Int] = Map("static" -> 0, "field" -> 0, "argument" -> 0, "var" -> 0)

  // The actual values of the table: [Name, (Type, Kind, #)]
  var table: Map[String, (String, String, Int)] = Map()

  // Operator for adding symbol to the SymbolTable
  def +=(varName: String, varType: String, varKind: String): Unit = {
    table += (varName -> (varType, varKind, varKinds(varKind)))
    varKinds = varKinds.updated(varKind, varKinds(varKind) + 1)
  }

  // Returns the string that represent the variable in VM, or null if not exist
  def apply(varName: String): String = {
    if (table.contains(varName))
      table(varName)._2
        .replace("field", "this")
        .replace("var", "local") +
        " " + table(varName)._3
    else
      classTable(varName)
  }

  // Returns the amount of the variables of the given kind
  def varCount(kind: String*): Int = {
    kind.map(kind_ => table.values.count(_._2 == kind_)).sum
  }

  // Returns if varName is in the table or in the class symbol table (if exist)
  def contains(varName: String): Boolean = {
    if (table.contains(varName))
      true
    else if (classTable != null)
      classTable.contains(varName)
    else false
  }

  // Returns the type of the varName
  def getType(varName: String): String = {
    if (table.contains(varName))
      table(varName)._1
    else
      classTable.getType(varName)
  }
}
