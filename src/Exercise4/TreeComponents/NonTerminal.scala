package Exercise4.TreeComponents

// A class that represents a non-terminal
class NonTerminal extends Rule {
  // List all the sub rules of the rule
  var subRules: List[Rule] = List()

  // Parameter that determines whether to export the xml with the rule itself or only its sub rules
  var transparent: Boolean = false

  // Constructor
  def this(ruleType: String, transparent: Boolean = false) {
    this()
    this.ruleType = ruleType
    this.transparent = transparent
  }

  // Function that adds a new rule to the subRules list
  def addSubRule(rule: Rule): Unit = {
    if (rule != null)
      subRules = this.subRules :+ rule
  }

  // Overrides the function that returns the content in XML so that it returns the XML for all sons of the rule
  override def getXmlContent: String =
    if (transparent) subRules.map(rule => "  " + rule.getXmlContent).mkString
    else "<" + ruleType + ">\n" + subRules.map(rule => "  " + rule.getXmlContent).mkString + "</" + ruleType + ">\n"

  // Return the sub rule at the index 'index'
  override def apply(index: Int): Rule = if (subRules.isEmpty || index >= subRules.length) null else subRules(index)
}
