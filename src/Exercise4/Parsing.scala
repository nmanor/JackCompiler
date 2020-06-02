package Exercise4

import Exercise4.TreeComponents.{NonTerminal, Terminal}

object Parsing {

  def parse(): Unit = {
    val terminal = new NonTerminal("Root")
    terminal.addSubRule(new Terminal("Int", "23"))
    terminal.addSubRule(new Terminal("String", "Hello world"))
    val temp = new NonTerminal("NonTerminal")
    temp.addSubRule(new Terminal("Char", "A"))
    temp.addSubRule(new Terminal("id", "class1"))
    val temp2 = new NonTerminal("temp2")
    temp2.addSubRule(new Terminal("id", "classB"))
    temp.addSubRule(temp2)
    terminal.addSubRule(temp)
    print(terminal.getXmlContent)
  }

}