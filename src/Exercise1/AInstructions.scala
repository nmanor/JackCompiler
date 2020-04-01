package Exercise1

import scala.collection.immutable.HashMap

object AInstructions {
  val asmNameOf = HashMap("local" -> "LCL", "argument" -> "ARG", "this" -> "THIS", "that" -> "THAT", "temp" -> "TMP")

  def pop(segment: String, offset: Int, fileName: String, lineNum: Int): String = {
    // function for function-level segments: local / argument / this / that
    def functionLevel(segment: String, offset: Int): String = {
      var pop = "@" + offset + "\n" // load the offset into A
      pop += "D=A\n" // move the offset into D
      pop += "@" + asmNameOf(segment) + "\n" // load the address of segment into A
      pop += "D=M+D\n" // calculate the segment[offset] into D
      pop += "@R13\n" // load the temporary into A
      pop += "M=D\n" // save the data of segment[offset] into the temp room
      pop += "@SP\n" // load the SP into A
      pop += "M=M-1\n" // decrease the SP by 1
      pop += "A=M\n" // load the pointer of the next free room
      pop += "D=M\n" // save the data of segment[offset] into the free room
      pop += "@13\n" // load the SP into A
      pop += "A=M\n" // load the pointer of the next free room
      pop += "M=D\n" // save the data of segment[offset] into the free room

      // return the result to the main program
      pop
    }

    // function for pointer / temp segments
    def tempLevel(offset: Int): String = {
      var pop = "@" + offset + "\n" // load the offset into A
      pop += "D=A\n" // store the offset in D
      pop += "@5\n" // load the temp into A
      pop += "D=A+D\n" // calculate the value of segment[offset]
      pop += "@R13\n" // load SP into A
      pop += "M=D\n" // get the next free room pointer into A
      pop += "@SP\n" // load the SP into A
      pop += "M=M-1\n" // move the pointer to the next free room
      pop += "A=M\n" // get the next free room pointer into A
      pop += "D=M\n" // load the SP into A
      pop += "@R13\n" // load SP into A
      pop += "A=M\n" // get the next free room pointer into A
      pop += "M=D\n" // get the next free room pointer into A

      // return the result to the main program
      pop
    }

    def pointerLevel(offset: Int): String = {
      var pop = "@SP\n" // load the SP into A
      pop += "A=M-1\n" // get the pointer to the 1st place
      pop += "D=M\n" // get the value of the 1st place
      if (offset == 0)
        pop += "@THIS\n" // load the pointer of THIS
      else
      pop += "@THAT\n" // load the pointer of THAT
      pop += "M=D\n" // save the value of D in the pointer
      pop += "@SP\n" // load the SP into A
      pop += "M=M-1\n" // change SP

      // return the result to the main program
      pop
    }

    // function for static segments
    def staticLevel(fileName: String, offset: Int): String = {
      var pop = "@SP\n" // load SP into A
      pop += "M=M-1\n" // get the next free room pointer into A
      pop += "A=M\n" // store the data of fileName.offset from D into the free room
      pop += "D=M\n" // store the data of fileName.offset from D into the free room
      pop += "@" + fileName + "." + offset + "\n" // load the pointer to fileName.offset into A
      pop += "M=D\n" // store the value of fileName.offset into D

      // return the result to the main program
      pop
    }

    def constantLevel(value: Int): String = {
      var pop = "@SP\n" // load the value into A
      pop += "M=M-1\n" // put the value from A into D

      // return the result to the main program
      pop
    }

    // return the result of the right segment type
    segment match {
      case "local" | "argument" | "this" | "that" => functionLevel(segment, offset)
      case "temp" => tempLevel(offset)
      case "pointer" => pointerLevel(offset)
      case "static" => staticLevel(fileName, offset)
      case _ => throw new Exception("Unknown VM segment \"" + segment + "\" at line " + lineNum + " in " + fileName)
    }
  }

  def push(segment: String, offset: Int, fileName: String, lineNum: Int): String = {
    // function for function-level segments: local / argument / this / that
    def functionLevel(segment: String, offset: Int): String = {
      var push = "@" + offset + "\n" // load the offset into A
      push += "D=A\n" // move the offset into D
      push += "@" + asmNameOf(segment) + "\n" // load the address of segment into A
      push += "A=M+D\n" // calculate the segment[offset] into A
      push += "D=M\n" // load the data of segment[offset] into D
      push += "@SP\n" // load the SP into A
      push += "A=M\n" // load the pointer of the next free room
      push += "M=D\n" // save the data of segment[offset] into the free room
      push += "@SP\n" // load the SP into A
      push += "M=M+1\n" // move the pointer to the next free room

      // return the result to the main program
      push
    }

    // function for pointer / temp segments
    def tempLevel(offset: Int): String = {
      var push = "@" + offset + "\n" // load the offset into A
      push += "D=A\n" // store the offset in D
      push += "@5\n" // load the temp value (5) into A
      push += "A=A+D\n" // calculate the value of segment[offset]
      push += "D=M\n" // load the data of segment[offset] into D
      push += "@SP\n" // load SP into A
      push += "A=M\n" // get the next free room pointer into A
      push += "M=D\n" // store the data of segment[offset] from D into the free room
      push += "@SP\n" // load the SP into A
      push += "M=M+1\n" // move the pointer to the next free room

      // return the result to the main program
      push
    }

    def pointerLevel(offset: Int): String = {
      var push = "@" // load the segment pointer
      if (offset == 0)
        push += "THIS\n"
      else
        push += "THAT\n"
      push += "D=M\n" // load the value of the pointer into D
      push += "@SP\n" // load SP into A
      push += "A=M\n" // get the pointer to the next free space
      push += "M=D\n" // place the value from D into the free room
      push += "@SP\n" // load SP into A
      push += "M=M+1\n" // move the pointer to the next free space

      // return the result to the main program
      push
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

      // return the result to the main program
      push
    }

    def constantLevel(value: Int): String = {
      var push = "@" + value + "\n" // load the value into A
      push += "D=A\n" // put the value from A into D
      push += "@SP\n" // get the SP into A
      push += "A=M\n" // load the next free room into A
      push += "M=D\n" // store value from D into the free room
      push += "@SP\n" // get the SP into A
      push += "M=M+1\n" // move the pointer to the next free room

      // return the result to the main program
      push
    }

    // return the result of the right segment type
    segment match {
      case "local" | "argument" | "this" | "that" => functionLevel(segment, offset)
      case "temp" => tempLevel(offset)
      case "pointer" => pointerLevel(offset)
      case "static" => staticLevel(fileName, offset)
      case "constant" => constantLevel(offset)
      case _ => throw new Exception("Unknown VM segment \"" + segment + "\" at line " + lineNum)
    }
  }

  def gt(instruction: Int): String = {
    var gt = "@SP\n" // get the location of the first empty place in the stack (SP)
    gt += "A=M-1\n" // get the location of the 1st value in the stack
    gt += "D=M\n" // get the value of the 1st
    gt += "A=A-1\n" // get the location of the 2nd
    gt += "D=D-M\n" // subtract the 2nd from the 1st
    gt += "@GT_" + instruction + "\n" // load the location of the code if LT is true
    gt += "D;JLE\n" // jump to the label if X is really LT then Y
    gt += "D=0\n" // load FALSE into D
    gt += "@SP\n" // load the location of SP into A
    gt += "A=M-1\n" // load the address of the 1st value in the stack
    gt += "A=A-1\n" // get the location of the 2nd
    gt += "M=D\n" // put the result in the 2nd place
    gt += "@NOT_GT_" + instruction + "\n" // load the location of the code if LT is false
    gt += "0;JMP\n" // unconditional jump
    gt += "(GT_" + instruction + ")\n" // set the label
    gt += "D=-1\n" // load TRUE into D
    gt += "@SP\n" // load the location of SP into A
    gt += "A=M-1\n" // load the address of the 1st value in the stack
    gt += "A=A-1\n" // get the location of the 2nd
    gt += "M=D\n" // put the result in the 2nd place
    gt += "(NOT_GT_" + instruction + ")\n" // set the label
    gt += "@SP\n" // load the location of SP into A
    gt += "M=M-1\n" // save the new SP

    // return the result to the main program
    gt
  }

  def eql(instruction: Int): String = {
    var eq = "@SP\n" // get the location of the first empty place in the stack (SP)
    eq += "A=M-1\n" // get the location of the 1st value in the stack
    eq += "D=M\n" // get the value of the 1st
    eq += "A=A-1\n" // get the location of the 2nd
    eq += "D=D-M\n" // subtract the 2nd from the 1st
    eq += "@EQ_" + instruction + "\n" // load the location of the code if EQ is true
    eq += "D;JEQ\n" // jump to the label if X is really LT then Y
    eq += "D=0\n" // load FALSE into D
    eq += "@SP\n" // load the location of SP into A
    eq += "A=M-1\n" // load the address of the 1st value in the stack
    eq += "A=A-1\n" // get the location of the 2nd
    eq += "M=D\n" // put the result in the 2nd place
    eq += "@NOT_EQ_" + instruction + "\n" // load the location of the code if EQ is false
    eq += "0;JMP\n" // unconditional jump
    eq += "(EQ_" + instruction + ")\n" // set the label
    eq += "D=-1\n" // load TRUE into D
    eq += "@SP\n" // load the location of SP into A
    eq += "A=M-1\n" // load the address of the 1st value in the stack
    eq += "A=A-1\n" // get the location of the 2nd
    eq += "M=D\n" // put the result in the 2nd place
    eq += "(NOT_EQ_" + instruction + ")\n" // set the label
    eq += "@SP\n" // load the location of SP into A
    eq += "M=M-1\n" // save the new SP

    // return the result to the main program
    eq
  }

  def neg(): String = {
    var neg = "@SP\n" //A=0 the location of the stack pointer
    neg += "A=M-1\n" //dump the loction the SP is pointing at to A
    neg += "D=M\n" //get the value into D
    neg += "M=-D\n" //dump into the same location the value of -V

    // return the result to the main program
    neg
  }

  def sub(): String = {
    var sub = "@SP\n" //A=0 the location of the stack pointer
    sub += "M=M-1\n" //decrease the stack pointer by 1 and thats where it needs to be at the end
    sub += "A=M\n" //dump the loction the SP is pointing at to A
    sub += "D=M\n" //get the second value into D
    sub += "A=A-1\n" //get the loction of the first value
    sub += "M=M-D\n" //dump into the same location the value of V1-V2

    // return the result to the main program
    sub
  }

  def add(): String = {
    var add = "@SP\n" //A=0 the location of the stack pointer
    add += "M=M-1\n" //decrease the stack pointer by 1 and thats where it needs to be at the end
    add += "A=M\n" //dump the loction the SP is pointing at to A
    add += "D=M\n" //get the first value into D
    add += "A=A-1\n" //get the loction of the second value
    add += "M=D+M\n" //dump into the same location the value of V1+V2

    // return the result to the main program
    add
  }

  def lt(instruction: Int): String = {
    var lt = "@SP\n" // get the location of the first empty place in the stack (SP)
    lt += "A=M-1\n" // get the location of the 1st value in the stack
    lt += "D=M\n" // get the value of the 1st
    lt += "A=A-1\n" // get the location of the 2nd
    lt += "D=D-M\n" // subtract the 2nd from the 1st
    lt += "@LT_" + instruction + "\n" // load the location of the code if LT is true
    lt += "D;JGE\n" // jump to the label if X is really LT then Y
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
}
