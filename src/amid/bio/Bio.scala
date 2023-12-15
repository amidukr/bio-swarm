package amid.bio

import scala.collection.mutable.HashMap
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import BioUtils._
import amid.processor.Implicits._
import amid.utils.Utils._
import java.awt.Color
import amid.utils.EventQueue
import amid.processor._
import amid.bio._
import scala.util.Random
import scala.collection.mutable.WeakHashMap
import scala.collection.mutable.HashSet
import scala.math.log;
import scala.math.min;

object BioUtils {
    def toVector(direction:Int) = direction%4 match { case 0 => Vector( 0,  1)
                                                      case 1 => Vector( 1,  0)
                                                      case 2 => Vector( 0, -1)
                                                      case 3 => Vector(-1,  0)}
}

class Bio(val pool:Pool = new Pool()){
  
  val minFood = 0.1;
  
  private var event:BioStepEvent = null;
  private val processor = new Processor(commandSet);
  private val foodFactory = new Food();
  
  def move(creature:Creature, direction:Vector){
    val newPosition = creature.position + direction;
    event.moveRequest(newPosition, creature);

    event.move.doLater {
      if (event.getMoveRequests(newPosition).size > 2) 
        true;
      else 
        creatures.putCreature(newPosition, creature);
    }
  }
  
  def read(dst:Creature, dstPointer:Pointer, src:Creature, srcPointer:Pointer){
    dst.memory.set(dstPointer, src.memory.get(srcPointer));
  }
  
  def width  = pool.width;
  def height = pool.height;
  
  def randomVector() = Vector(pool.random.nextInt(pool.width),
                              pool.random.nextInt(pool.height))
  
  def globPosition(position:Vector) = new Vector((pool.width  + position.x) % pool.width,
                                                 (pool.height + position.y) % pool.height)
  
  def doStep(creature:Creature) {
    val result = processor.doNext(Executable(creature, creatures.getDebugger(creature)));

    creature.color = blendColor(creature.color, toColor(result.lastInstruction), 0.01);
      
    if(!creature.food) creature.age += 1;
  }
  
  def doStep() {
    val foodAmount = creatures.countFood();
    
    if(foodAmount < pool.creatures.size * minFood){
      val r = pool.random.nextFloat();
      val index:Int = (log(1/r)/log(2)).toInt;
      val sorted = pool.creatures.toList.sortWith((x,y) => x.age > y.age);
      var oldCreature = sorted(min(index, sorted.length));
      creatures.removeCreature(oldCreature);
      val vector = if(pool.random.nextFloat() > 0.99) randomVector() else oldCreature.position;
      foodFactory.animate(Bio.this,  vector);
    }
    
    event = new BioStepEvent();

    for(creature <- pool.creatures) doStep(creature);

    event.move.invoke();
  }
  
  case class PoolField(creatures:List[Creature])
  
  object creatures{
    private val fieldMap = new HashMap[Vector, PoolField]();
    private val debuggers = new HashMap[Creature, Debugger]();
    
    reloadCache();
    
    def positions = fieldMap.keySet;
    
    def getDebugger(creature:Creature) = null;//debuggers.getOrElse(creature, null);
  
    def getPoolField(position:Vector) = fieldMap.getOrElse(globPosition(position), PoolField(Nil));
    
    def getSecondCreature(c:Creature) = 
      getPoolField(c.position).creatures.find(_ != c).orNull;
    
    private def cacheRemove(creature:Creature) {
      if(creature.position == null) return;
      
      val creatures = getPoolField(creature.position).creatures.filterNot(_ == creature);
      if(creatures.length == 0){
        fieldMap.remove(creature.position);
      }else{
        fieldMap.put(creature.position, PoolField(creatures));
      }
    }
  
    def removeCreature(creature:Creature) {
      pool.creatures.remove(creature);
      cacheRemove(creature);
      debuggers.remove(creature);
    }
  
    def putCreature(newPosition:Vector, creature:Creature, debugger:Debugger = null):Boolean = {
      val _newPosition = globPosition(newPosition);
      val field = getPoolField(_newPosition);
  
      if(field.creatures.length == 2) return false;
      
      cacheRemove(creature);
      creature.position = _newPosition;
      fieldMap.put(_newPosition, PoolField(creature :: field.creatures));
      pool.creatures.add(creature);
      if(debugger != null){
        debuggers.put(creature, debugger);
      }
    
      return true;
    }
    
    def countFood() = pool.creatures.count(_.food);
    
    def reloadCache(){
      fieldMap.clear();
      
      for(creature <- pool.creatures){
        putCreature(creature.position, creature);
      }
    }
  }
  
  object commandSet extends BasicCommandSet{
    
    class HandlerEvent(task:Task, instruction:Instruction) extends super.HandlerEvent(task, instruction){
       def creature = task.asInstanceOf[Creature];
    }
    
    type CreatureHandler = (HandlerEvent)=> Unit;
    
    override protected def createHandlerEvent(task:Task, instruction:Instruction) = new HandlerEvent(task, instruction);
    
    def registreCreatureCommand(handler:CreatureHandler) = registerCommand({ v => 
      handler(v.asInstanceOf[HandlerEvent])
    });
    
    val MOV_COMMAND  = registreCreatureCommand{v => move(v.creature, toVector(v.word1))};
    val READ_COMMAND = registreCreatureCommand{v =>
      val secondCreature = creatures.getSecondCreature(v.creature);
      if(secondCreature != null){
        read(v.creature, Pointer(v.value2), secondCreature, Pointer(v.value1));
      }else{
        v.pointer = v.word3
      }
    };
        
    val REVIVE_COMMAND = registreCreatureCommand { v =>
      v.creature.food = false;
      if(pool.random.nextFloat() < 1.0f/100){
        v.task.memory.set(pool.random.nextInt(0x100), pool.random.nextInt());
      }
    }
  }
  
  class BioStepEvent(){
    private val _moveRequest = new HashMap[Vector, ArrayBuffer[Creature]];
    
    def moveRequest(newPosition:Vector, creature:Creature) {
       _moveRequest.getOrElseUpdate(globPosition(newPosition), new ArrayBuffer()).append(creature)
    }
    
    def getMoveRequests(newPosition:Vector) = _moveRequest.getOrElse(globPosition(newPosition), Nil);
  
    val move = new EventQueue;
  }
}

class BioWriter(bio:Bio) extends BasicCommandWriter(bio.commandSet){
    private val cs = bio.commandSet;
    
    def writeMove(direction:OperandValue) = write(cs.MOV_COMMAND, direction);
    def writeRead(from:CWPointer, to:CWPointer, fail:CWLabel) = write(cs.READ_COMMAND, from, to, fail);
    def writeRevive() = write(cs.REVIVE_COMMAND);
  }

//class Pool{
//    
//    val width  = 100;
//    val height = 100;
//    private val fieldMap = new HashMap[Vector, PoolField]();
//    
//    def removeCreature(creature:Creature) {
//      for{
//         field    <- fieldMap.get(creature.position)
//         newField <- createField(field.creatures.filterNot(_ == creature))
//       } {
//         assert(field.creatures.length - 1 == newField.creatures.length);
//         fieldMap.put(creature.position, newField);
//       }
//    }
//    
//    def putCreature(newPosition:Vector, creature:Creature):Boolean = {
//       creature.pool = this;
//       
//       if(newPosition.x < 0 ||
//          newPosition.y < 0 ||
//          newPosition.x > width ||
//          newPosition.y > height) return false;
//       
//       val field    = getPoolField(newPosition)
//       val newField = createField(creature :: field.creatures);
//       newField.exists {newField => removeCreature(creature)
//                                    fieldMap.put(newPosition, newField);
//                                    creature.position = newPosition;
//                                    true
//                       }
//    }
//    
//    def creatures = for(field <- fieldMap.values; creature <- field.creatures) yield creature;
//    
//    def creaturesPositions = fieldMap.keySet; 
//    
//    def getPoolField(position:Vector) = fieldMap.getOrElse(position, PoolField(Nil))
//    
//    def doStep() {
//        val event = new BioStepEvent();
//        
//        for(creature <- this.creatures) creature.doStep(event);
//        
//        event.move.invoke();
//    }
//}
//
// //---------------------------------------------------------------------
// //--------------------  CREATURE LOGIC  -------------------------------
// //---------------------------------------------------------------------
//
//
//class Creature{
//    var pool:Pool       = null;
//    var position:Vector = null;
//    var color = new Color(100, 100, 100);
//    
//    protected var event:BioStepEvent = null;
//    
//    lazy val processor:Processor = new Processor(writer.commandSet, buildProgram);
//    private val maxAge = 256 * 5;
//    private var countUpAge = false;
//    private var age = 0;
//    
//    def globPosition(position:Vector) = new Vector((pool.width + position.x) % pool.width,
//                                                   (pool.height +position.y) % pool.height)
//    
//    def move(direction:Int) {
//      val newPosition = globPosition(position + toVector(direction));
//      val points = event.moveRequest.getOrElseUpdate(newPosition, {new ArrayBuffer()});
//      points += this;
//      
//      event.move.doLater {
//        val requestedCreatures = event.moveRequest.getOrElse(newPosition, Nil);
//        val existedCreatures   = pool.getPoolField(newPosition);
//        if (requestedCreatures.size > 2) true;
//        else pool.putCreature(newPosition, this);
//      }
//    }
//    
//    def doStep(event:BioStepEvent) {
//      this.event = event;
//      processor.doNext()
//      
//      color = blendColor(color, toColor(processor.lastInstruction), 0.01);
//      
//      if(countUpAge) age += 1;
//    }
//    
//    def buildProgram():Executable = new Executable(new Memory(), new Debugger());
//    
//    object writer extends CommandWriter{
//        val MOV_COMMAND = cs.registerCommand{v => move(v.word1)};
//        def writeMove(direction:OperandValue) = write(MOV_COMMAND, direction);
//        
//        val READ_COMMAND = cs.registerCommand{v => val field = pool.getPoolField(position)
//                                                   if(field.creatures.length == 2){
//                                                     val thisCreature  = Creature.this;
//                                                     val otherCreature = if(field.creatures(0) == thisCreature)
//                                                                            field.creatures(1)
//                                                                         else
//                                                                            field.creatures(0);
//                                                     thisCreature.processor.memory.set(Pointer(v.value2), otherCreature.processor.memory.get(Pointer(v.value1)));
//                                                   }else{
//                                                     v.pointer = v.word3;
//                                                   }
//                                             };
//        def writeRead(from:CWPointer, to:CWPointer, fail:CWLabel) = write(READ_COMMAND, from, to, fail);
//        
//        val REVIVE_COMMAND = cs.registerCommand { 
//          v =>
//            countUpAge = true;
//        }
//        def writeRevive() = write(REVIVE_COMMAND);
//    }
//}