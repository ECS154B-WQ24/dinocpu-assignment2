// This file contains ALU control logic.

package dinocpu.components

import chisel3._
import chisel3.util._

/**
 * The ALU control unit
 *
 * Input:  aluop        Specifying the type of instruction using ALU
 *                          . 0 for none of the below
 *                          . 1 for arithmetic instruction types (R-type or I-type)
 *                          . 2 for non-arithmetic instruction types that uses ALU (auipc/jal/jarl/Load/Store)
 * Input:  arth_type    The type of instruction (0 for R-type, 1 for I-type)
 * Input:  int_length   The integer length (0 for 64-bit, 1 for 32-bit)
 * Input:  funct7       The most significant bits of the instruction.
 * Input:  funct3       The middle three bits of the instruction (12-14).
 *
 * Output: operation    What we want the ALU to do.
 *
 * For more information, see Section 4.4 and A.5 of Patterson and Hennessy.
 * This is loosely based on figure 4.12
 */
class ALUControl extends Module {
  val io = IO(new Bundle {
    val aluop       = Input(UInt(2.W))
    val arth_type   = Input(UInt(1.W))
    val int_length    = Input(UInt(1.W))
    val funct7      = Input(UInt(7.W))
    val funct3      = Input(UInt(3.W))

    val operation   = Output(UInt(5.W))
  })

  io.operation := "b11111".U // Invalid

  // Your code goes here

  switch(io.int_length){

    is(0.U){

      when(io.aluop === 1.U && io.arth_type === 0.U){ //64-bit R-type

        when(io.funct7 === "b0000000".U && io.funct3 === "b000".U){ //add
          io.operation := "b00000".U
        }
          
        .elsewhen(io.funct7 === "b0100000".U && io.funct3 === "b000".U){ //sub
          io.operation := "b00001".U
        }
          
        .elsewhen(io.funct7 === "b0000000".U && io.funct3 === "b001".U){ //sll
          io.operation := "b01010".U
        }  
        
        .elsewhen(io.funct7 === "b0000000".U && io.funct3 === "b010".U){ //slt
          io.operation := "b01100".U
        }  
        .elsewhen(io.funct7 === "b0000000".U && io.funct3 === "b011".U){ //sltu
          io.operation := "b01111".U
        }  

        .elsewhen(io.funct7 === "b0000000".U && io.funct3 === "b100".U){ //xor
          io.operation := "b01000".U
        }  

        .elsewhen(io.funct7 === "b0000000".U && io.funct3 === "b101".U){ //srl
          io.operation := "b01011".U
        }  

        .elsewhen(io.funct7 === "b0100000".U && io.funct3 === "b101".U){ //sra
          io.operation := "b01001".U
        }  

        .elsewhen(io.funct7 === "b0000000".U && io.funct3 === "b110".U){ //or
          io.operation := "b00111".U
        }  

        .elsewhen(io.funct7 === "b0000000".U && io.funct3 === "b111".U){ //and
          io.operation := "b00101".U
        }  

        .elsewhen(io.funct7 === "b0000001".U && io.funct3 === "b000".U){ //mul
          io.operation := "b00010".U
        }  

        .elsewhen(io.funct7 === "b0000001".U && io.funct3 === "b001".U){ //mulh
          io.operation := "b10101".U
        }  

        .elsewhen(io.funct7 === "b0000001".U && io.funct3 === "b010".U){ //mulhsu
          io.operation := "b11000".U
        }  

        .elsewhen(io.funct7 === "b0000001".U && io.funct3 === "b011".U){ //mulhu
          io.operation := "b10111".U
        }  

        .elsewhen(io.funct7 === "b0000001".U && io.funct3 === "b100".U){ //div
          io.operation := "b00011".U
        }  

        .elsewhen(io.funct7 === "b0000001".U && io.funct3 === "b101".U){ //divu
          io.operation := "b01101".U
        }  

        .elsewhen(io.funct7 === "b0000001".U && io.funct3 === "b110".U){ //rem
          io.operation := "b00100".U
        }  

        .elsewhen(io.funct7 === "b0000001".U && io.funct3 === "b111".U){ //remu
          io.operation := "b01110".U
        }  
      }
      .elsewhen(io.aluop === 1.U && io.arth_type === 1.U){ //64-bit I-type

        when(io.funct3 === "b000".U){ //addi
            io.operation := "b00000".U
          }

        .elsewhen(io.funct3 === "b010".U){ //slti
            io.operation := "b01100".U
        }

        .elsewhen(io.funct3 === "b011".U){ //sltiu
          io.operation := "b01111".U
        }

        .elsewhen(io.funct3 === "b100".U){ //xori
          io.operation := "b01000".U
        }

        .elsewhen(io.funct3 === "b110".U){ //ori
          io.operation := "b00111".U
        }

        .elsewhen(io.funct3 === "b111".U){ //andi
          io.operation := "b00101".U
        }

        .elsewhen(io.funct7(6, 1) === "b000000".U && io.funct3 === "b001".U){ //slli
           io.operation := "b01010".U
        }

        .elsewhen(io.funct7(6, 1) === "b000000".U && io.funct3 === "b101".U){ //srli
          io.operation := "b01011".U
        }

        .elsewhen(io.funct7(6, 1) === "b010000".U && io.funct3 === "b101".U){ //srai
          io.operation := "b01001".U
        }
      }
      .elsewhen(io.aluop === 2.U && io.arth_type === 0.U){//ld inst
        when(io.funct3 === "b011".U){ 
          io.operation := "b00000".U
        }
        .otherwise{
          io.operation := "b00000".U
        }
      }

    }
  
  
    is(1.U){

      when(io.aluop === 1.U && io.arth_type === 0.U){ //32-bit R-type

        when(io.funct7 === "b0000000".U && io.funct3 === "b000".U){ //addw
          io.operation := "b10000".U
        }  
        
        .elsewhen(io.funct7 === "b0100000".U && io.funct3 === "b000".U){ //subw
          io.operation := "b10001".U
        }  

        .elsewhen(io.funct7 === "b0000000".U && io.funct3 === "b001".U){ //sllw
          io.operation := "b11010".U
        }  

        .elsewhen(io.funct7 === "b0000000".U && io.funct3 === "b101".U){ //srlw
          io.operation := "b11011".U
        }  

        .elsewhen(io.funct7 === "b0100000".U && io.funct3 === "b101".U){ //sraw
          io.operation := "b11001".U
        }  

        .elsewhen(io.funct7 === "b0000001".U && io.funct3 === "b000".U){ //mulw
          io.operation := "b10010".U
        }  

        .elsewhen(io.funct7 === "b0000001".U && io.funct3 === "b100".U){ //divw
          io.operation := "b10011".U
        }  

        .elsewhen(io.funct7 === "b0000001".U && io.funct3 === "b101".U){ //divuw
          io.operation := "b11101".U
        }  

        .elsewhen(io.funct7 === "b0000001".U && io.funct3 === "b110".U){ //remw
          io.operation := "b10100".U
        }  

        .elsewhen(io.funct7 === "b0000001".U && io.funct3 === "b111".U){ //remuw
          io.operation := "b11110".U
        }  
      }

      .elsewhen(io.aluop === 1.U && io.arth_type === 1.U){ //32-bit I-type

        when(io.funct3 === "b000".U){ //addiw
          io.operation := "b10000".U
        }  
        
        .elsewhen(io.funct7 === "b0000000".U && io.funct3 === "b001".U){ //slliw
          io.operation := "b11010".U
        }  

        .elsewhen(io.funct7 === "b0000000".U && io.funct3 === "b101".U){ //srliw
          io.operation := "b11011".U
        }  

        .elsewhen(io.funct7 === "b0100000".U && io.funct3 === "b101".U){ //sraiw
          io.operation := "b11001".U
        }  
      }

      
      
    }
  }
}
