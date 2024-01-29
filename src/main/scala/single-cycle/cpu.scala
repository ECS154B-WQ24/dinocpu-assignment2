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

  pc := pc + 4.U

  control.io.opcode := instruction(6, 0) // control inputs

  immGen.io.instruction := instruction // immgen input

  aluControl.io.aluop := control.io.aluop // alu control inputs
  aluControl.io.arth_type := control.io.arth_type
  aluControl.io.int_length := control.io.int_length
  aluControl.io.funct7 := instruction(31, 25)
  aluControl.io.funct3 := instruction(14, 12)
  
  alu.io.operation := aluControl.io.operation // alu inputs
  switch(control.io.op1_src){ 
    is(0.U){
      alu.io.operand1 := registers.io.readdata1
    }
    is(1.U){
      alu.io.operand1 := pc
    }
  }
  switch(control.io.op2_src){
    is(0.U){
      alu.io.operand2 := registers.io.readdata2
    }
    is(1.U){
      alu.io.operand2 := immGen.io.sextImm 
    }
    is(2.U){
      alu.io.operand2 := 4.U
    }
  }

  
  io.dmem.address := alu.io.result // data memory inputs new
  io.dmem.memread := control.io.memop === 1.U
  io.dmem.memwrite := control.io.memop === 2.U
  io.dmem.valid := control.io.memop > 0.U
  io.dmem.maskmode := instruction(14, 12)
  io.dmem.sext := instruction(14, 12) < 2.U
  io.dmem.writedata := registers.io.readdata2
  
  
  
  registers.io.readreg2 := instruction(24, 20) // register inputs
  registers.io.readreg1 := instruction(19, 15)
  registers.io.writereg := instruction(11, 7)
  registers.io.wen := (control.io.writeback_src =/= 0.U) && (registers.io.writereg =/= 0.U)
  switch(control.io.writeback_src){ //new
    is(1.U){
      registers.io.writedata := alu.io.result
    }
    is(2.U){
      registers.io.writedata := immGen.io.sextImm
    }
    is(3.U){
      registers.io.writedata := io.dmem.readdata
    }
  }
  

  

  
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
    )
  }
}
