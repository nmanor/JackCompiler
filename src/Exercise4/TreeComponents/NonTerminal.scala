package Exercise4.TreeComponents

// A class that represents a non-terminal
class NonTerminal extends Rule {
  // List all the sub rules of the rule
  var subRules: List[Rule] = List()

  // Constructor
  def this(ruleType: String) {
    this()
    this.ruleType = ruleType
  }

  // Function that adds a new rule to the subRules list
  def addSubRule(rule: Rule): Unit = {
    if (rule != null)
      subRules = this.subRules :+ rule
  }

  // Function that adds a list of rules to the subRules list
  def addSubRule(rules: List[Rule]): Unit = {
    for (rule <- rules)
      if (rule != null)
        subRules = this.subRules :+ rule
  }

  // Overrides the function that returns the content in XML so that it returns the XML for all sons of the rule
  override def getXmlContent: String = "<" + ruleType + ">\n" +
    subRules.map(rule => "  " + rule.getXmlContent).mkString +
    "</" + ruleType + ">\n"
}
