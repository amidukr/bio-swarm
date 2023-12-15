package amid.bio

import java.awt.Color
import amid.processor.Task
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import java.util.Random

case class Vector(val x:Int, val y:Int){
  def +(o:Vector):Vector = Vector(x + o.x, y + o.y)
}

class Pool extends Serializable{
  val width  = 100;
  val height = 100;
  val creatures = HashSet[Creature]();
  val random = new Random();
  //private val fieldMap = new HashMap[Vector, PoolField]();
  
//  def getPoolField(position:Vector) = fieldMap.getOrElse(position, PoolField(Nil));
//  
//  def removeCreature(creature:Creature) {
//    val creatures = getPoolField(creature.position).creatures.remove(_ == creature);
//    if(creatures.length == 0){
//      fieldMap.remove(creature.position);
//    }else{
//      fieldMap.put(creature.position, PoolField(creatures));
//    }
//  }
//    
//  def putCreature(newPosition:Vector, creature:Creature):Boolean = {
//    
//    if(newPosition.x < 0 ||
//      newPosition.y < 0 ||
//      newPosition.x > width ||
//      newPosition.y > height) return false;
//    
//    val field = getPoolField(newPosition);
//    
//    if(field.creatures.length == 2) return false;
//    
//    creature.position = newPosition;
//    fieldMap.put(newPosition, PoolField(creature :: field.creatures));
//    
//    return true;
  //}
}

class Creature(task:Task) extends Task(task.memory, task.pointer){
  var position:Vector = null;
  var color = new Color(100, 100, 100);
  var age = 0;
  var food:Boolean = true;
}