package Exercise1

object AInstructions {

  def lt(instruction: Int): String = {
    var lt = "@SP\n" // get the location of the first empty place in the stack (SP)
    lt += "A=M-1\n" // get the location of the 1st value in the stack
    lt += "D=M\n" // get the value of the 1st
    lt += "A=A-1\n" // get the location of the 2nd
    lt += "D=D-M\n" // subtract the 2nd from the 1st
    lt += "@LT_" + instruction + "\n" // load the location of the code if LT is true
    lt += "D;JLT\n" // jump to the label if X is really LT then Y
    lt += "D=0\n" // load FALSE into D
    lt += "@SP\n" // load the location of SP into A
    lt += "A=M-1\n" // load the address of the 1st value in the stack
    lt += "A=A-1\n" // get the location of the 2nd
    lt += "M=D\n" // put the result in the 2nd place
    lt += "@NOT_LT_" + instruction + "\n" // load the location of the code if LT is false
    lt += "0;JMP\n" // unconditional jump
    lt += "(LT_" + instruction + ")\n" // set the label
    lt += "D=-1\n" // load TRUE into D
    lt += "@SP\n" // load the location of SP into A
    lt += "A=M-1\n" // load the address of the 1st value in the stack
    lt += "A=A-1\n" // get the location of the 2nd
    lt += "M=D\n" // put the result in the 2nd place
    lt += "(NOT_LT_" + instruction + ")\n" // set the label
    lt += "@SP\n" // load the location of SP into A
    lt += "M=M-1\n" // save the new SP

    // return the result to the main program
    lt
  }

  def and(): String = {
    var and = "@SP\n" // load the SP
    and += "M=M-1\n" // put the 1st pointer in SP
    and += "@SP\n" // load the pointer to the 1st into A
    and += "A=M\n" // load the pointer of the 1st
    and += "D=M\n" // move the value of the 1st to D
    and += "@SP\n" // load the SP
    and += "M=M-1\n" // put the 2nd pointer in SP
    and += "@SP\n" // load the SP into A
    and += "A=M\n" // load the pointer of the 2nd into A
    and += "A=M\n" // load the value of the 2nd into A
    and += "D=D&A\n" // calculate the value of D && A
    and += "@SP\n" // load the SP into A
    and += "A=M\n" // load the pointer of the 2nd into A
    and += "M=D\n" // put the result D in M[2nd]
    and += "@SP\n" // load the SP into A
    and += "M=M+1\n" // move SP to SP+1

    // return the result to the main program
    and
  }

  def or(): String = {
    var or = "@SP\n" // load the SP
    or += "M=M-1\n" // put the 1st pointer in SP
    or += "@SP\n" // load the pointer to the 1st into A
    or += "A=M\n" // load the pointer of the 1st
    or += "D=M\n" // move the value of the 1st to D
    or += "@SP\n" // load the SP
    or += "M=M-1\n" // put the 2nd pointer in SP
    or += "@SP\n" // load the SP into A
    or += "A=M\n" // load the pointer of the 2nd into A
    or += "A=M\n" // load the value of the 2nd into A
    or += "D=D|A\n" // calculate the value of D||A
    or += "@SP\n" // load the SP into A
    or += "A=M\n" // load the pointer of the 2nd into A
    or += "M=D\n" // put the result D in M[2nd]
    or += "@SP\n" // load the SP into A
    or += "M=M+1\n" // move SP to SP+1

    // return the result to the main program
    or
  }

  def not(): String = {
    var not = "@SP\n" // load the SP
    not += "A=M-1\n" // load the pointer to the 1st into A
    not += "D=M\n" // load the data of the 1st into D
    not += "M=!D\n" // save the !1st into the pointer of the 1st

    // return the result to the main program
    not
  }

  def push(segment: String, offset: Int, fileName: String): String = {
    // function for function-level segments: local / argument / this / that
    def functionLevel(segment: String, offset: Int): String = {
      var push = "@" + offset + "\n" // load the offset into A
      push += "D=A\n" // move the offset into D
      push += "@" + segment + "\n" // load the address of segment into A
      push += "A=M+D\n" // calculate the segment + offset into A
      push += "D=M\n" // load the data of segment + offset into D
      push += "@SP\n" // load the SP into A
      push += "A=M\n" // load the pointer of the next free room
      push += "M=D\n" // save the data of segment + offset into the free room
      push += "@SP\n" // load the SP into A
      push += "M=M+1\n" // move the pointer to the next free room
      push // return the result to the main program
    }

    // function for pointer / temp segments
    def pointerTempLevel(segment: String, offset: Int): String = {
      var push = "@" + offset + "\n" // load the offset into A
      push += "D=A\n" // store the offset in D
      if (segment.equals("pointer"))
        push += "@3\n" // load the pointer value (3) into A
      else
      push += "@5\n" // load the temp value (5) into A
      push += "A=A+D\n" // calculate the value of segment + offset
      push += "D=M\n" // load the data of segment + offset into D
      push += "@SP\n" // load SP into A
      push += "A=M\n" // get the next free room pointer into A
      push += "M=D\n" // store the data of segment + offset from D into the free room
      push += "@SP\n" // load the SP into A
      push += "M=M+1\n" // move the pointer to the next free room
      push // return the result to the main program
    }

    // function for static segments
    def staticLevel(fileName: String, offset: Int): String = {
      var push = "@" + fileName + "." + offset + "\n" // load the pointer to fileName.offset into A
      push += "D=M\n" // store the value of fileName.offset into D
      push += "@SP\n" // load SP into A
      push += "A=M\n" // get the next free room pointer into A
      push += "M=D\n" // store the data of fileName.offset from D into the free room
      push += "@SP\n" // load the SP into A
      push += "M=M+1\n" // move the pointer to the next free room
      push // return the result to the main program
    }

    def constantLevel(value: Int): String = {
      var push = "@" + value + "\n" // load the value into A
      push += "D=A\n" // put the value from A into D
      push += "@SP\n" // get the SP into A
      push += "A=M\n" // load the next free room into A
      push += "M=D\n" // store value from D into the free room
      push += "@SP\n" // get the SP into A
      push += "M=M+1\n" // move the pointer to the next free room
      push // return the result to the main program
    }

    // return the result of the right segment type
    if (segment.equals("local") || segment.equals("argument") || segment.equals("this") || segment.equals("that"))
      return functionLevel(segment, offset)
    if (segment.equals("pointer") || segment.equals("temp"))
      return pointerTempLevel(segment, offset)
    if (segment.equals("static"))
      return staticLevel(fileName, offset)
    constantLevel(offset)
  }

}
