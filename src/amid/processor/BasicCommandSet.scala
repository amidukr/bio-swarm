package amid.processor;

import amid.processor.Implicits._
import amid.utils.Utils._

class BasicCommandSet extends CommandSet{
  
    val DELAY_COMMAND = registerCommand{_=>};
    val DEBUG_COMMAND = registerCommand{e => println("Debug: " + e.pointer + " => [" + e.word1 + "]" + e.value1)};
    val ADD_COMMAND = registerCommand{e => e.value3 = e.value1 + e.value2};
    
    object LetOperationType extends Enumeration {
     type LetOperationType = Value;
     def operation(v:Int) = Value(v);
    }
    
    type LetOperationType = LetOperationType.LetOperationType;
    val FROM_NEAR_TO_NEAR = LetOperationType.operation(0);
    val FROM_NEAR_TO_FAR  = LetOperationType.operation(1);
    val FROM_FAR_TO_NEAR  = LetOperationType.operation(2);
    val FROM_FAR_TO_FAR   = LetOperationType.operation(3);
    
    
    val LET_COMMAND = registerCommand{e =>
      var value = if((e.word3 & 0x2) == 0){
        e.value1;
      }else{
        e.memory.get(e.value1.value);
      }
      
      if((e.word3 & 0x1) == 0){
         e.value2 = value;
      }else{
        e.memory.set(e.value2.value, value);
      }
    };
    
    val SUB_COMMAND = registerCommand{v => v.value3 = v.value1 - v.value2};
    val MUL_COMMAND = registerCommand{v => v.value3 = v.value1 * v.value2};
    val DIV_COMMAND = registerCommand{v => if(v.value2.value != 0) v.value3 = v.value1 / v.value2};
    
    val POSITIVE_JUMP_COMMAND = registerCommand{v => 
        if(v.value1.value == 0){
            v.pointer = v.word2;
        }else if(v.value1.value > 0){
            v.pointer = v.word1;
        };
    }
    
    val NEGATIVE_JUMP_COMMAND = registerCommand{v => 
        if(v.value1.value == 0){
            v.pointer = v.word2;
        }else if(v.value1.value < 0){
            v.pointer = v.word1;
        };
    }
    
    val EQUAL_JUMP_COMMAND = registerCommand{v => 
        if(v.value1 == v.value2){
            v.pointer = v.word3;
        }
    }
    
    val NOT_EQUAL_JUMP_COMMAND = registerCommand{v => 
        if(v.value1 != v.value2){
            v.pointer = v.word3;
        }
    }
    
    val READ_INSTRUCTION_POINTER_COMMAND = registerCommand{v => v.value1 = v.pointer + v.word2};
    val FAR_JUMP_COMMAND = registerCommand{v => v.pointer = v.value1.value};
}

class BasicCommandWriter(override val commandSet:BasicCommandSet = new BasicCommandSet()) extends CommandWriter(commandSet){
    private val cs = commandSet;
    
    def writeEntryPoint(memory:Memory, entryPoint:CWLabel){
      memory.set(0, instruction(cs.EQUAL_JUMP_COMMAND, 0, 0, entryPoint.toWord).value);
    }
    
    def writeInterrupt() = {
        write(CommandSet.INTERRUPT_COMMAND);
    }
    
    def writeDelay() = {write(cs.DELAY_COMMAND)}
    
    def writeDebug(op:Operand) = {write(cs.DEBUG_COMMAND, op)}
    
    def writeAdd(op1:Operand, op2:Operand, result:Operand) = {
        write(cs.ADD_COMMAND, op1, op2, result);
    }
    
    def writeLet(op1:Operand, result:Operand, opType:commandSet.LetOperationType = commandSet.FROM_NEAR_TO_NEAR) = {
      write(cs.LET_COMMAND, op1, result, value(opType.id));
    }
    
    def writeSub(op1:Operand, op2:Operand, result:Operand) = {
        write(cs.SUB_COMMAND, op1, op2, result);
    }
    
    def writeMul(op1:Operand, op2:Operand, result:Operand) = {
        write(cs.MUL_COMMAND, op1, op2, result);
    }    
    
    def writeDiv(op1:Operand, op2:Operand, result:Operand) = {
        write(cs.DIV_COMMAND, op1, op2, result);
    }
    
    def writePositiveJump(value:Operand, jumpPositive:CWLabel, jumpZero:CWLabel = null) = {
        val nextLabel = label();
        nextLabel.offset(1) := write(cs.POSITIVE_JUMP_COMMAND, value, jumpPositive, jumpZero ||| nextLabel);
    }
    
    def writeNegativeJump(value:Operand, jumpNegative:CWLabel, jumpZero:CWLabel = null) = {
        val nextLabel = label();
        nextLabel.offset(1) := write(cs.NEGATIVE_JUMP_COMMAND, value, jumpNegative, jumpZero ||| nextLabel);
    }
    
    def writeEqualJump(value1:Operand, value2:Operand, jump:CWLabel) = {
        write(cs.EQUAL_JUMP_COMMAND, value1, value2, jump);
    }
    
    def writeNotEqualJump(value1:Operand, value2:Operand, jump:CWLabel) = {
        write(cs.NOT_EQUAL_JUMP_COMMAND, value1, value2, jump);
    }
    
    def writeJump(jump:CWLabel) = {
        writeEqualJump(value(0), value(0), jump)
    }
    
    
    def writeReadInstructionPointer(result:Operand, offset:Int = 0) = {
        write(cs.READ_INSTRUCTION_POINTER_COMMAND, result, value(offset));
    }
    
    def writeFarJump(operand:Operand) = {
        write(cs.FAR_JUMP_COMMAND, operand);
    }
}