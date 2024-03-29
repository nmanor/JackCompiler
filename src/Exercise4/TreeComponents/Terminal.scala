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
  override def getXmlContent: String = "<" + ruleType + "> " + terminal
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll("\"", "&quet;") +
    " </" + ruleType + ">\n"

  // The indexer return the this, no matter what the rule is
  override def apply(index: Int): Rule = this
}
