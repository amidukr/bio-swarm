package amid.test

import amid.processor.Implicits._
import amid.processor.CommandWriter
import amid.processor.Processor
import amid.processor.BasicCommandWriter

object CommandSetTest {
  
  def assertEquals[T](message:String, a:T, b:T)
         = assert(a equals b, message + " [ " + a + " != " + b + " ] ")
  def assertEquals[T](a:T, b:T):Unit
         = assertEquals("", a, b) 

  def main(args: Array[String]): Unit = {
    val w = new BasicCommandWriter();
    
    val a      = w.variable(1);
    val b      = w.variable(2);
    val result = w.variable();
    
                 w.writeAdd(a, b, result);
                 w.writeInterrupt();
                 
    val x = w.compile();
    new Processor().run(x)
    
    assertEquals("" + "add" + " test failed", result.getValue(x.task.memory).value, 3)
  }

}