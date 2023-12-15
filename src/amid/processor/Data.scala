package amid.processor

import Implicits._
import amid.config.Config

//---------------------------------------------------------------------
//---------------------- DATA CONVERSION ------------------------------
//---------------------------------------------------------------------

object Implicits {
  
    type WordType  = Int;
    type ValueType = Int;
    
    def toWordType(value:Int):WordType = {
      val normalized = value & 0xFF; 
      if(!Config.WEAK_POINTER){
        assert(normalized == value, value + " can't convert to word.");
      }
      normalized
    }
    
    implicit def toValue  (value: Int)= new Value   (value)
    implicit def toCommand(value: Int) = new Command(toWordType(value))
    implicit def toPointer(value: Int) = new Pointer(toWordType(value))
    
    implicit def toInt(value: Value)   = value.value
    implicit def toInt(value: Command) = value.value
    implicit def toInt(value: Pointer) = value.value
    
    implicit def value(instruction: Instruction) = instruction.value;
    
    implicit def toInstruction(value: Value) =     new Instruction(value, (value & 0xFF)            ,
                                                                          (value & 0xFF00)     >>> 8 ,
                                                                          (value & 0xFF0000)   >>> 16,
                                                                          (value & 0xFF000000) >>> 24);
                                                                        
    def instruction(cmd:Command, op1:WordType, op2:WordType, op3:WordType)  =
                                                   new Instruction(cmd | op1 << 8 | op2 << 16 | op3 << 24,
                                                       cmd,
                                                       op1,
                                                       op2,
                                                       op3);
}



//---------------------------------------------------------------------
//---------------------- DATA DECLARATION -----------------------------
//---------------------------------------------------------------------    

case class Value  (value:ValueType)
case class Command(v:WordType){val value = toWordType(v)}
case class Pointer(v:WordType){val value = toWordType(v)}
case class Instruction(val value:ValueType, val command:Command,
                                            val word1:WordType,
                                            val word2:WordType,
                                            val word3:WordType)
                                            
//---------------------------------------------------------------------
//---------------------------- MEMORY ---------------------------------
//---------------------------------------------------------------------

class Memory extends Serializable{
   val values = new Array[ValueType](0x100) 
   
   def get(address:Pointer):Value = values(address)
   def set(address:Pointer, value:Value) = (values(address) = value)
}

class Task(val memory:Memory = new Memory(), var pointer:Pointer = 0) extends Serializable