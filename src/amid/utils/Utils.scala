package amid.utils
import scala.collection.mutable.ListBuffer
import java.awt.Color
import scala.math._
import scala.util.Random
import amid.processor.Instruction

case class BoolUtil(b: Boolean) {
  def ?[X](t: => X) = new {
    def |(f: => X) = if(b) t else f
  }
}

case class IntUtil(i: Int) {
  def toHex():String = "%x" format i;
}

case class AnyUtil[A](o: A) {
    def |||(t: => A):A = BoolUtil(o != null) ? o | t;
}

case class BitUtils(from:Int, to:Int){
  val range:Int = ((1L << (to - from)).toInt - 1);
  val mask:Int = range << from;
  def getBits(value:Int) = (value & mask) >>> from;
  def inRange(value:Int, newRange:Int) = (newRange*1.0*getBits(value)/range) toInt;
}

object Utils {
    def out[T](v:T) = {println(v);v};
    implicit def toBoolUtil(b: Boolean) = BoolUtil(b);
    implicit def toAnyUtil[A](o: A)     = AnyUtil(o);
    implicit def toIntUtil(v: Int)     = IntUtil(v);
    
    def toColor(instruction:Instruction) = { val hash = new Random(instruction.value).nextInt();
                                             new Color(BitUtils(0, 11) .inRange(hash, 255),
                                                       BitUtils(11, 21).inRange(hash, 255),
                                                       BitUtils(21, 32).inRange(hash, 255));
                                           }
                                           
    private def blendValue(dst:Int, src:Int, alpha:Double) = min((dst*(1-alpha) + src*(alpha)) toInt, 255); 
    
    def blendColor(dst:Color, src:Color, alpha:Double) = {val a = min(1, max(0, alpha)); 
                                                        new Color(blendValue(dst.getRed(),   src.getRed(),   a),
                                                                  blendValue(dst.getGreen(), src.getGreen(), a),
                                                                  blendValue(dst.getBlue(),  src.getBlue(),  a))}
}

class EventQueue{
  type Callback = ()=> Boolean;
  private var callbacks = new ListBuffer[Callback]()
  
  def doLater(callback: => Boolean) {callbacks += {()=> callback}}
  
  def invoke() {
    
    var repeat = false;
    
    do{
      val iterator = callbacks.iterator
      callbacks = new ListBuffer[Callback]()
      
      while(iterator.hasNext){
        val callback:Callback = iterator.next();
        if(!callback()){
          callbacks += callback;
        }else{
          repeat = false;
        }
      }
    }while(repeat)
    
    callbacks.clear();
  }
}