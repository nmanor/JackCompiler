package Exercise4.TreeComponents

// A class that represents a final terminal
class Terminal extends Rule {
  // The contents of the terminal
  var terminal: String = ""

  // Constructor
  def this(ruleType: String, terminal: String) {
    this()
    this.terminal = terminal
    this.ruleType = ruleType
  }

  // The override of the function that returns the rule as XML
  override def getXmlContent: String = "<" + ruleType + "> " + terminal + " </" + ruleType + ">\n"
}
