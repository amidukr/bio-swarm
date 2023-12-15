package amid.bio

import amid.processor.Implicits._
import amid.bio.ui.BioScreen
import amid.processor.Value
import amid.processor.CommandWriter
import amid.processor.Processor
import amid.processor.BasicCommandWriter

object Run {
    
    def assertEquals[T](message:String, a:T, b:T)
         = assert(a equals b, message + " [ " + a + " != " + b + " ] ")
    def assertEquals[T](a:T, b:T):Unit
         = assertEquals("", a, b) 
         
sealed abstract class Command;
    case class ADD() extends Command;
    case class SUB() extends Command; 
    case class MUL() extends Command;
    case class DIV() extends Command;

         
def testMathOP(command:Command, a:Value, b:Value, result:Value){
    val w = new BasicCommandWriter();
    
    val vA      = w.variable(a);
    val vB      = w.variable(b);
    val vResult = w.variable();
                 
                 command match{
                   case ADD() => w.writeAdd(vA, vB, vResult)
                   case SUB() => w.writeSub(vA, vB, vResult)
                   case MUL() => w.writeMul(vA, vB, vResult)
                   case DIV() => w.writeDiv(vA, vB, vResult)
                 }
                 
                 w.writeInterrupt();
                 
    val x = w.compile();
    new Processor().run(x);
    
    assertEquals("" + command + " test failed", vResult.getValue(x.task.memory), result)
}
    
    def testProcessor(){
      testMathOP(ADD(),  2,  6,   8)
      testMathOP(ADD(), -1,  1,   0)
      
      testMathOP(SUB(), 3,   5,  -2)
      testMathOP(SUB(), 10,  3,   7)
      
      testMathOP(MUL(), 8,   3,  24)
      testMathOP(MUL(), 20, 30, 600)
      
      testMathOP(DIV(), 12,  6,   2)
      testMathOP(DIV(), 99,  3,  33)
    }
    
    def runPool(args: Array[String]){
        val screen = new BioScreen();
        screen.main(args)
    }
    
    def main(args: Array[String]) {
      println("Hello, world!")
      //testProcessor();
      
      runPool(args);
    }
}