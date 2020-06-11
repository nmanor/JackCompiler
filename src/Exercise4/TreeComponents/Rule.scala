package Exercise4.TreeComponents

// An abstract class that represents a rule
abstract class Rule {
  // The type of rule
  var ruleType: String = ""

  // An abstract function that returns the contents of the entire rule as XML
  def getXmlContent: String

  // Indexer
  def apply(index: Int): Rule
}