package Exercise2

import scala.sys.process.processInternal._

object MInstructions {

  def call(g: String, n: Int): String = {
    // simple inner push function that get value and push it into the next free space in the memory
    def innerPush(value: String): String = {
      var push = "@" + value + "\n" // load the value address into A
      push += "D=A\n" // place the value address in D
      push += "@SP\n" // load SP into A
      push += "A=M\n" // get the next free room pointer into A
      push += "M=D\n" // store the return address from D into the free room
      push += "@SP\n" // load the SP into A
      push += "M=M+1\n" // move the pointer to the next free room

      // return the result
      push
    }

    var call = innerPush(g + ".ReturnAddress") // push the return-address
    call += innerPush("LCL") // push the LCL (local) address
    call += innerPush("ARG") // push the ARG (argument) address
    call += innerPush("THIS") // push the THIS (this) address
    call += innerPush("THAT") // push the THAT (that) address

    // set the current ARG to be SP-n-5
    call += "@SP\n" // load the next free room address
    call += "D=M\n" // save the next free room address into D
    call += "@" + (n - 5) + "\n" // compute the new ARG into A
    call += "D=D-A\n" // compute SP-n-5 into D
    call += "@ARG\n" // load the address of ARG into A
    call += "M=D\n" // save SP-n-5 into ARG

    // set LCL = SP
    call += "@SP\n" // load SP into A
    call += "D=M\n" // save SP into D
    call += "@LCL\n" // load LCL into A
    call += "M=D\n" // save SP in LCL

    // goto g
    call += "@g\n" // load the address of g into A
    call += "0; JMP\n" // unconditional jump

    // set the label of the return address
    call += "(" + g + ".ReturnAddress)\n" // set label

    // return the result to the main program
    call
  }


  def function(g: String, k: Int, lineNum: Int): String = {
    // set the label of function g
    var function = "(" + g + ")\n"

    // initialize the local variables to be 0
    function += "@" + k + "\n" // load k into a
    function += "D=A\n" // load k from A do D
    function += "@" + lineNum + ".End\n" // load the address of the end of the loop into A
    function += "D; JEQ\n" // jump to the end of the loop if k == 0. else, do the following:
    function += "(" + lineNum + ".Loop)\n" // set label for the loop itself
    function += "@SP\n" // load SP into A
    function += "A=M\n" // load the next free room into a
    function += "M=0\n" // put 0 in the next free room
    function += "@SP\n" // load SP into A
    function += "M=M+1\n" // move the next free room foreword
    function += "@" + lineNum + ".Loop\n" // load the address of the begging of the loop into A
    function += "D=D-1; JNE\n" // if D != 0, jump to the begging of the loop
    function += "(" + lineNum + ".End)\n" // set label for the end of the loop

    // return the result to the main program
    function
  }


  def retrn(): String = {
    // creates a logical time variable (called FRAME for convenience) whose value is equal to LCL
    var retrn = "@LCL\n" // load LCL into A
    retrn += "D=M\n" // load the address of LCL into A

    // restore the return address form FRAME-5 into RAM[13]
    retrn += "@5\n" // load 5 into A
    retrn += "A=D-A\n" // calculate FRAME-5 into A
    retrn += "D=M\n" // put the value of the return address into D
    retrn += "@13\n" // load 13 into A
    retrn += "M=D\n" // save the return address in RAM[13]

    // put in ARG the TODO: להמשיך לכתוב את הפונקציה הזאת
    retrn += "@SP\n" // load SP into A
    retrn += "M=M-1\n" // get the next element address at the RAM
    retrn += "A=M\n" // put the value of the element in A
    retrn += "D=M\n" // put in D the value of RAM[element] into D
    retrn += "@ARG\n" // load ARG into A
    retrn += "A=M\n" // load the address of ARG into A
    retrn += "M=D\n" // save the new address into ARG

    // return the result to the main program
    retrn
  }

  def label(file: File,string: String): String = {
    var label = "("+ file.getName()+"."+string+")\n" // connect file name to label name

    // return the result to the main program
    label
  }
  def goto(file: File,string: String): String = {
    var goto = "@"+ file.getName()+"."+string+"\n" // connect file name to label name and load address
    goto += "0;JMP\n"                               //jump to label
    // return the result to the main program
    goto
  }

    def ifgoto(file: File,string: String): String = {
    var ifgoto ="@SP\n" // load sp
      ifgoto += "M=M-1\n"// put the 1st pointer in SP
      ifgoto += "A=M\n"
      ifgoto += "D=M\n"
      ifgoto += "@"+ file.getName()+"."+string+"\n" // load the label address
      ifgoto += "D;JNE\n" // jump on condition

    // return the result to the main program
      ifgoto
  }

}
