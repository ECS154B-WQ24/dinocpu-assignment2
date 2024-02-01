// This file is where all of the CPU components are assembled into the whole CPU

package dinocpu

import chisel3._
import chisel3.util._
import dinocpu.components._

/**
 * The main CPU definition that hooks up all of the other components.
 *
 * For more information, see section 4.4 of Patterson and Hennessy
 * This follows figure 4.21
 */
class SingleCycleCPU(implicit val conf: CPUConfig) extends BaseCPU {
  // All of the structures required
  val pc              = dontTouch(RegInit(0.U(64.W)))
  val control         = Module(new Control())
  val registers       = Module(new RegisterFile())
  val aluControl      = Module(new ALUControl())
  val alu             = Module(new ALU())
  val immGen          = Module(new ImmediateGenerator())
  val jumpDetection   = Module(new JumpDetectionUnit())
  val jumpPcGen       = Module(new JumpPcGeneratorUnit())
  val (cycleCount, _) = Counter(true.B, 1 << 30)

  control.io := DontCare
  registers.io := DontCare
  aluControl.io := DontCare
  alu.io := DontCare
  immGen.io := DontCare
  jumpDetection.io := DontCare
  jumpPcGen.io := DontCare
  io.dmem <> DontCare

  //FETCH
  io.imem.address := pc
  io.imem.valid := true.B

  val instruction = Wire(UInt(32.W))
  when ((pc % 8.U) === 4.U) {
    instruction := io.imem.instruction(63, 32)
  } .otherwise {
    instruction := io.imem.instruction(31, 0)
  }

  // Your code goes here

  val funct3 = Wire(UInt(3.W))
  val funct7 = Wire(UInt(7.W))

  funct3 := instruction(14, 12)
  funct7 := instruction(31, 25)


  immGen.io.instruction := instruction

  control.io.opcode := instruction(6, 0)

  registers.io.readreg1 := instruction(19, 15)
  registers.io.readreg2 := instruction(24, 20)
  registers.io.writereg := instruction(11, 7)
  registers.io.writedata := MuxCase(
    0.U, // default value
    Array(
    (control.io.writeback_src===1.U) -> alu.io.result, 
    (control.io.writeback_src===2.U) -> immGen.io.sextImm,
    (control.io.writeback_src===3.U) ->io.dmem.readdata))
  registers.io.wen := (control.io.writeback_src =/= 0.U & registers.io.writereg =/= 0.U)

  aluControl.io.aluop := control.io.aluop
  aluControl.io.arth_type := control.io.arth_type
  aluControl.io.int_length := control.io.int_length
  aluControl.io.funct3 := funct3
  aluControl.io.funct7 := funct7

  alu.io.operation := aluControl.io.operation
  alu.io.operand1 := Mux(control.io.op1_src===0.U, registers.io.readdata1, pc)
  alu.io.operand2 := MuxCase(
    0.U, // default value
    Array(
    (control.io.op2_src===0.U) -> registers.io.readdata2, 
    (control.io.op2_src===1.U) -> immGen.io.sextImm,
    (control.io.op2_src===2.U) -> 4.U
    )
  )

  io.dmem.address := alu.io.result
  io.dmem.writedata := registers.io.readdata2
  io.dmem.memread := control.io.memop === 1.U
  io.dmem.memwrite := control.io.memop === 2.U
  io.dmem.valid := control.io.memop =/= 0.U
  io.dmem.sext := ~funct3(2)
  io.dmem.maskmode := funct3(1, 0)

  jumpDetection.io.jumpop := control.io.jumpop
  jumpDetection.io.operand1 := registers.io.readdata1
  jumpDetection.io.operand2 := registers.io.readdata2
  jumpDetection.io.funct3 := funct3

  jumpPcGen.io.pc := pc
  jumpPcGen.io.pc_plus_offset := jumpDetection.io.pc_plus_offset
  jumpPcGen.io.op1_plus_offset := jumpDetection.io.op1_plus_offset
  jumpPcGen.io.offset := immGen.io.sextImm
  jumpPcGen.io.op1 := registers.io.readdata1

  pc := Mux(jumpDetection.io.taken,
    jumpPcGen.io.jumppc,
    pc + 4.U
  )

}

/*
 * Object to make it easier to print information about the CPU
 */
object SingleCycleCPUInfo {
  def getModules(): List[String] = {
    List(
      "dmem",
      "imem",
      "control",
      "registers",
      "csr",
      "aluControl",
      "alu",
      "immGen",
      "jumpDetection",
      "jumpPcGen"
    )
  }
}
