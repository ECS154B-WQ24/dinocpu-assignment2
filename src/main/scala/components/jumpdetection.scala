// Logic to detect if jump is taken

package dinocpu.components

import chisel3._

/**
 * JumpDetection Unit.
 * This component takes care of deciding the PC of the next cycle upon a jump instruction (jump/branch-type).
 *
 * Input: jumpop        Specifying the type of jump instruction (J-type/B-type)
 *                                          . 0 for none of the below
 *                                          . 1 for jal
 *                                          . 2 for jalr
 *                                          . 3 for branch instructions (B-type)
 * Input: operand1                 First input
 * Input: operand2                 Second input
 * Input: funct3                   The funct3 from the instruction
 *
 * Output: pc_plus_offset          True if the next pc is the current pc plus the offset (imm)
 * Output: op1_plus_offset         True if the first operand is the first operand plus the offset (imm)
 * Output: taken                   True if, either the instruction is a branch instruction and it is taken, or it is a jump instruction
 *
 */
class JumpDetectionUnit extends Module {
  val io = IO(new Bundle {
    val jumpop            = Input(UInt(2.W))
    val operand1          = Input(UInt(64.W))
    val operand2          = Input(UInt(64.W))
    val funct3            = Input(UInt(3.W))
  
    val pc_plus_offset    = Output(Bool())
    val op1_plus_offset   = Output(Bool())
    val taken             = Output(Bool())
  })

  // default case, i.e., not a jump instruction
  io.pc_plus_offset := false.B
  io.op1_plus_offset := false.B
  io.taken := false.B

  // Your code goes here
  /*
  switch(io.jumpop) {
    is(1.U) { // jal
      io.pc_plus_offset := true.B
      io.taken := true.B
    }
    is(2.U) { // jalr
      io.op1_plus_offset := true.B
      io.taken := true.B
    }
    is(3.U) { // branch instructions
      // Determine if the branch is taken based on funct3 and operands
      io.taken := MuxCase(false.B, Array(
        (io.funct3 === 0.U) -> (io.operand1 === io.operand2),  // BEQ: Branch if Equal
        (io.funct3 === 1.U) -> (io.operand1 =/= io.operand2),  // BNE: Branch if Not Equal
        (io.funct3 === 4.U) -> (io.operand1.asSInt < io.operand2.asSInt), // BLT: Branch if Less Than (signed)
        (io.funct3 === 5.U) -> (io.operand1.asSInt >= io.operand2.asSInt), // BGE: Branch if Greater Than or Equal (signed)
        (io.funct3 === 6.U) -> (io.operand1 < io.operand2),  // BLTU: Branch if Less Than (unsigned)
        (io.funct3 === 7.U) -> (io.operand1 >= io.operand2)  // BGEU: Branch if Greater Than or Equal (unsigned)
      ))
    }
  }
  */
}

