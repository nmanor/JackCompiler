package Exercise1

class AInstructions {

  // פונקציות של נתן

  def lt(instruction: Int): String = {
    var lt = "@SP" // get the location of the first empty place in the stack (SP)
    lt += "A=M-1" // get the location of the 1st value in the stack
    lt += "D=M" // get the value of the 1st
    lt += "A=A-1" // get the location of the 2nd
    lt += "D=D-M" // subtract the 2nd from the 1st
    lt += "@LT_" + instruction // load the location of the code if LT is true
    lt += "D;JLT" // jump to the label if X is really LT then Y
    lt += "D=0" // load FALSE into D
    lt += "@SP" // load the location of SP into A
    lt += "A=M-1" // load the address of the 1st value in the stack
    lt += "A=A-1" // get the location of the 2nd
    lt += "M=D" // put the result in the 2nd place
    lt += "@NOT_LT_" + instruction // load the location of the code if LT is false
    lt += "0;JMP" // unconditional jump
    lt += "(LT_" + instruction + ")" // set the label
    lt += "D-1" // load TRUE into D
    lt += "@SP" // load the location of SP into A
    lt += "A=M-1" // load the address of the 1st value in the stack
    lt += "A=A-1" // get the location of the 2nd
    lt += "M=D" // put the result in the 2nd place
    lt += "(NOT_LT_" + instruction + ")" // set the label
    lt += "@SP" // load the location of SP into A
    lt += "M=M-1" // save the new SP

    // return the result to the main program
    lt
  }

  def and(): Unit = {

  }

  def or(): Unit = {

  }

  def not(): Unit = {

  }

  def push(): Unit = {

  }

}
