package amid.bio

import scala.util.Random
import amid.processor.Implicits._
import amid.utils.Utils._
import amid.processor.Executable
import amid.processor.Value
import amid.processor.Instruction

object Food{
  val FOOD = 0xF00D;
  val EAT = 0xEA70;
  val DONE = 0xD04E;
}

abstract class CreatureFactory{
  def animate(bio:Bio, position:Vector) {
    val x = buildProgram(new BioWriter(bio));
    val creature = new Creature(x.task);
    bio.creatures.putCreature(position, creature, x.debugger);
  }
  
  protected def buildProgram(w: BioWriter):Executable;
}

class Food extends CreatureFactory{
    import Food._;
    
    override protected def buildProgram(w: BioWriter):Executable = {
      
      val index     = w.pointer(w.label(0));
      val status    = w.label(0xFF);
      val nextRead  = w.label();
      
                  val programOffset = 0x100 - 17;
      
                  w.debug({e => println("Food entry point, status: " + "%x" format e.memory.get(0xFF).value)})
                  w.writeLet(w.const(0), index)
                  
                  w.writeLet(w.const(FOOD), status);
                  //w.writeLet(w.const(0x777), status);
                  
      nextRead := w.writeRead(index, index, w.entryPoint);
      
                  w.debug({e => 
                    println("Status before eating is " + "%x" format e.memory.get(0xFF).value) 
                  })
                  w.debug({e => 
                    println("Eat progress: " +  index.getValue(e.memory))
                  })
                  w.writeLet(w.const(EAT), status);
                  //w.debug({e => println("Start eating, status is " + "%x" format e.memory.get(0xFF).value) })
                  //w.writeDebug(status);
                  
                  w.writeAdd(index, w.const(1), index);
                  
                  //w.writeNotEqualJump(w.const(w.programOffset.value), index, nextRead);
                  w.writeNotEqualJump(w.const(0x100 - 18), index, nextRead);
                  
                  w.writeLet(w.const(DONE), status);
                  w.writeRevive();
                  w.debug({e => println("Food eat done " + "%x" format e.memory.get(0xFF).value) })
                  
                  w.writeJump(w.label(0));
                  w.variable(FOOD);
      
      w.programOffset = programOffset;
      val executable = w.compile();
      val mem = executable.task.memory;
      
      
      executable.debugger.addBreakPoint(0, {e=> println("Food revived, " + "%x" format e.memory.get(0xFF).value)})
      
      assert(mem.get(0xFF) == Value(Food.FOOD), "Actual value: " + (mem.get(0xFF)).value.toHexString + ", size + programOffset = " + w.size + " + " + w.programOffset +  " = " + (w.size + w.programOffset) );
      
      return executable;
    }
}


class ChaoticHunger extends BaseHunger{
  override def createContext(w:BioWriter) = new Context(w){
    override def move(){
      val random = new Random();
    
      for(i <- 1 to 55){
        val next = w.label();
                   w.writeMove(w.value( random.nextInt(4) ));
                 /*w.writeMove(w.value( random.nextInt(4) ));
                   w.writeMove(w.value( random.nextInt(4) ));
                   w.writeMove(w.value( random.nextInt(4) ));
                   w.writeMove(w.value( random.nextInt(4) ));
                   w.writeMove(w.value( random.nextInt(4) ));
                   w.writeMove(w.value( random.nextInt(4) ));*/
                   checkForFood();
      
      }
    }
  }
}

class UpDownHunger extends BaseHunger{
  override def createContext(w:BioWriter) = new Context(w){
    override def move(){
      w.writeMove(w.value( 2 ));
      w.writeMove(w.value( 0 ));
    }
  }
}

class MoveUpHunger extends BaseHunger{
  override def createContext(w:BioWriter) = new Context(w){
    override def move(){
      w.writeMove(w.value( 2 ));
    }
  }
}

abstract class BaseHunger extends CreatureFactory{
  abstract protected class Context(val w:BioWriter){
    val status = w.variable();
    val statusPointer = w.pointer(status);
    
    val const0xFF = w.pointer(w.label().offset(0xFF));
    
    val eat = w.label();
    val foodReturn = w.variable();
    
    def move():Unit;
  
    def checkForFood():Unit = {
      w.writeReadInstructionPointer(foodReturn, 2);
      w.writeJump(eat);
    }
    
    def buildProgram():Executable = {
      move();
    
      checkForFood();
      w.writeJump(w.label(0));
      
      val returnLabel = w.label();
      
      eat := w.writeRead(const0xFF, statusPointer, returnLabel);
             /*w.debug { e=> 
               println("On food: " + "%x" format status.getValue(e.memory).value)
               val field = pool.getPoolField(this.position);
               println("Field: " + field);
               var food:Creature = field.creatures(0);
               println("Other creature is " + food +", status is " + food.processor.memory.get(0xFF).value.toHex());
               println();
             }*/
           
             w.writeEqualJump(w.const(Food.FOOD), status, eat);
             w.writeEqualJump(w.const(Food.EAT) , status, eat);
           
             returnLabel := w.writeFarJump(foodReturn);
    
      return w.compile();
    }
  }
  
  def createContext(w:BioWriter):Context;
  
  override def buildProgram(w:BioWriter):Executable = {
    val ctx = createContext(w);
    return ctx.buildProgram();
  }
}

class Chaotic extends CreatureFactory{
    override def buildProgram(writer:BioWriter):Executable = {
        val random = new Random();
        
        for(i <- 1 to random.nextInt(2) + 1){
          writer.writeMove(writer.value( random.nextInt(4) ));
        }
        
        writer.writeJump(writer.entryPoint);
        
        writer.compile();
    }
}