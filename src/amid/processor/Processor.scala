package amid.processor

import java.lang.Exception
import java.lang.reflect.Field
import java.nio.ByteBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import amid.processor.Implicits._
import scala.collection.mutable.ListBuffer
import amid.utils.Utils._
import amid.config.Config

case class BreakPointEvent(val task:Task, val instruction:Pointer){
  val memory = task.memory;
}

class Debugger{
  
  type BreakPointHandler = (BreakPointEvent) => Unit;
  private var breakPoints = new HashMap[Pointer, ListBuffer[BreakPointHandler]];
  
  def intercept(task:Task){
    
    //println("Curr => " + instruction);
    //println("Points => " + breakPoints);
    
    breakPoints.get(task.pointer) match {
      case Some(handlers) => 
        val event = BreakPointEvent(task, task.pointer);
        for(x <- handlers) x(event);
      case _ =>
    }
  }
  
  def addBreakPoint(pointer:Pointer, handler:BreakPointHandler):BreakPointHandler = {
    breakPoints.getOrElseUpdate(pointer, new ListBuffer()) += handler;
    return handler;
  }
}

case class Executable(task:Task, debugger:Debugger);

 //---------------------------------------------------------------------
 //----------------------   COMMAND SET  -------------------------------
 //---------------------------------------------------------------------

object CommandSet{
  val INTERRUPT_COMMAND:Command = 0xFF;
}

class CommandSet{
    
    class HandlerEvent(val task:Task, val instruction:Instruction){
        var _pointer:Pointer = null;
        val memory = task.memory;
        val command = instruction.command;
        val word1 = instruction.word1;
        val word2 = instruction.word2;
        val word3 = instruction.word3;
        
        def value1                 = memory.get(word1)
        def value1_= (value:Value) = memory.set(word1, value)
        def value2                 = memory.get(word2)
        def value2_= (value:Value) = memory.set(word2, value)
        def value3                 = memory.get(word3)
        def value3_= (value:Value) = memory.set(word3, value)
        
        def pointer                   = if(_pointer == null) task.pointer else _pointer
        def pointer_= (value:Pointer) = (_pointer = value)
    }
    
    protected type Handler = (HandlerEvent)=>Unit;
    private val commands = new HashMap[Command, Handler]();
    
    var commandIndex = 0;
    
    protected def createHandlerEvent(task:Task, instruction:Instruction) = new HandlerEvent(task, instruction);
    
    protected def registerCommand(handler: Handler):Command = {
        val cmd = commandIndex;
        commandIndex+=1;
        
        commands += toCommand(cmd) -> handler;
        return cmd
    }
    
    def runInstruction(pointer:Pointer, task:Task ,inst:Instruction):Pointer = {
      
       var _instruction  = inst;
        
       if(inst.command / commands.size != 0){
        _instruction = instruction(inst.command % commands.size, inst.word1,
                                                                     inst.word2,
                                                                     inst.word3);
        task.memory.set(pointer, _instruction.value);
      }
      
      val event = createHandlerEvent(task, _instruction)
      return commands.get(_instruction.command) match {
            case Some(handler) => 
              try{ 
                handler(event);
              }catch{
                case e:Throwable => e.printStackTrace();
              }
              
              if(event._pointer == null) pointer+1 else event._pointer;
            case None          => throw new Exception("Unexpected " + _instruction.command + " found")
      }
    }
}

abstract class CommandWriter (val commandSet:CommandSet){
    private val cs = commandSet;
    
    val entryPoint = label()
    var programOffset:Pointer = 0;
    
    //---------------------------------------------------------------------
    //-----------------  COMMAND WRITER API CLASSES  ----------------------
    //---------------------------------------------------------------------
    
    protected sealed abstract class Operand{
        def toWord:WordType;
    }
    
    trait InMemoryPointer { def offset:Pointer}
    
    trait MemoryRecord extends InMemoryPointer{
        def setOffset(offset:Pointer);
    }; 
    
    class CWVariable(val v:Value) extends Operand with MemoryRecord{
      
      var offset:Pointer = null;
      def value = v;
      override def setOffset(offset:Pointer) {this.offset = offset}
      override def toWord:WordType = offset.value ||| {throw new IllegalStateException("Variable haven't been calculated yet")};
      
      def getValue(memory:Memory) = memory.get(toWord);
    }
    
    class CWPointer(val label:InMemoryPointer) extends CWVariable(0){
      override def value = Value(label.offset);
    }
    
    class OperandValue(value:WordType) extends Operand{
      override def toWord = value;
    }
    
    protected class CWLabel() extends Operand with InMemoryPointer {
      private var o:Int = 0;
      var record:InMemoryPointer = null;
      def := (record:InMemoryPointer) = {this.record = record; record}
      def offset (v:Int) = {this.o = v; this}  
      override def toWord:WordType = o + (if(record == null) 0 else record.offset.value);
      override def offset:Pointer = toWord;
    }
    
    //---------------------------------------------------------------------
    //-----------------  COMMAND WRITER INTERNAL CLASSES  -----------------
    //---------------------------------------------------------------------
    
    protected class CWInstruction(val command:WordType, val o1:Operand,
                                                        val o2:Operand,
                                                        val o3:Operand) extends MemoryRecord{
      var _offset:Pointer = null;
      override def setOffset(offset:Pointer) {this._offset = offset}
      def offset:Pointer = _offset ||| {throw new IllegalStateException("Instruction haven't been assigned yet")};
    }
    
    //---------------------------------------------------------------------
    //----------------------  COMMAND WRITER API --------------------------
    //---------------------------------------------------------------------
    
    
    private val breakPoints  = ListBuffer[(CWLabel, Debugger#BreakPointHandler)]();
    private var latestInstruction:CWInstruction = null;
    private val instructions = new ArrayBuffer[CWInstruction]();
    private val variables    = new ArrayBuffer[CWVariable]();
    private val consts       = new HashMap[Value, CWVariable]();
    
    def write(x:Instruction):CWInstruction = write(x.command, new OperandValue(x.word1),
                                                              new OperandValue(x.word2),
                                                              new OperandValue(x.word3));
    
    def write(command:Command, o1:Operand = new OperandValue(0),
                               o2:Operand = new OperandValue(0),
                               o3:Operand = new OperandValue(0)) = {
        latestInstruction = new CWInstruction(command, o1, o2, o3);
        instructions += latestInstruction;
        latestInstruction;
    }
    
    def debug(handler:Debugger#BreakPointHandler):Unit = {
       var pointer = if(latestInstruction != null){
          offset(latestInstruction, 1);
       }else{
         entryPoint;
       }
       breakPoints += ((pointer, handler));
    }
    
    private def registerVariable[T <: CWVariable](variable:T) = {
      variables += variable;
      variable;
    }
    
    def variable(defaultValue:Value = 0) = registerVariable(new CWVariable(defaultValue));
    def pointer(value:InMemoryPointer)      = registerVariable(new CWPointer(value));
    
    def const(value:Value) =  consts.getOrElseUpdate(value, variable(value));
    
    def label(v:Int = 0) = new CWLabel().offset(v);
    def offset(record:InMemoryPointer, offset:Int): CWLabel = {
      val result = label();
      
      result.offset(offset) := record;
      
      return result;
    }
    
    def value(value:WordType) = new OperandValue(value)
    
    
    
    //---------------------------------------------------------------------
    //-------------------------   COMPILER --------------------------------
    //---------------------------------------------------------------------
    
    private def initializeMemory(recrods:Iterable[MemoryRecord], offset:Int) = {
      var pointer = offset;
      for(record <- recrods){
        record.setOffset(pointer)
        pointer = (pointer + 1);
      }
      
      pointer
    }
    
    def writeEntryPoint(memory:Memory, entryPoint:CWLabel);
    
    def size() = variables.length + instructions.length;
    
    def compile() = {
      val memory = new Memory();
      
      var lastPointer = initializeMemory(variables, initializeMemory(instructions, programOffset))
      
      assert(lastPointer <= 0x100, "Proccessor memory overflow")
      
      
      for(variable <- variables){
        memory.set(variable.offset, variable.value)
      }
      
      entryPoint := instructions(0);
      
      if(entryPoint.toWord != 0){
        writeEntryPoint(memory, entryPoint);
      }
      
      for(x <- instructions){
        memory.set(x.offset,
                   instruction(x.command, x.o1.toWord,
                                          x.o2.toWord,
                                          x.o3.toWord).value)
      }
      
      val debug = new Debugger();
      for(breakPoint <- breakPoints){
        val (inst:CWLabel, handler) = breakPoint;
        debug.addBreakPoint(inst.toWord, handler);
      }
      
      Executable(new Task(memory), debug);
    }
}

//---------------------------------------------------------------------
//------------------------  PROCESSOR ---------------------------------
//---------------------------------------------------------------------

case class ExecutionResult(val online:Boolean, val lastInstruction:Instruction)

class Processor(val commandSet:CommandSet = new BasicCommandSet()){
    
    def reset(x:Executable){
        x.task.pointer = 0;
    }
    
    def doNext(x:Executable):ExecutionResult = {
        val task = x.task;
        if(x.debugger != null) {
          x.debugger.intercept(task);
        }
        
        val instruction:Instruction = x.task.memory.get(task.pointer)
        
        if(Config.PRINT_INSTRUICTION)
            println(task.pointer + ": " + instruction);
        
        if(instruction.command == CommandSet.INTERRUPT_COMMAND) return ExecutionResult(false, instruction)
        
        x.task.pointer = commandSet.runInstruction(task.pointer, task, instruction)
        
        return ExecutionResult(true, instruction)
    }
    
    def run(x:Executable){
        reset(x)
        while(!doNext(x).online){}
    }
}