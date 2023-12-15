

case class SampleClass1(val byte:Int){def foo1 = println("foo1")}
case class SampleClass2(val byte:Int){def foo2 = println("foo2")}
case class SampleClass3(val byte:Int){def foo3 = println("foo3")}

class Base[T](val zi:T){
     
    def m(b:this.type) = b match{  case x:this.type => x.zi == this.zi
                             case _           => false
                          }
}

class A(zi:Int) extends Base(zi){
   
}


object TestImplicits {
    
    def main(args: Array[String]) {
        val a = new A(1);
        val a1 = a;
        val b = new Base(1);
        var t = 2;
        val value = 67108871;
        
        println((value & 0xFF            ) & 0xFF)
        println((value & 0xFF00     >> 8 ) & 0xFF)
        println((value & 0xFF0000   >> 16) & 0xFF)
        println((value & 0xFF000000 >> 24) & 0xFF)
        //println(a.m(a1));
        //println(a.m(b));
        //println(b.m(a));
        //println(b.m(b));
    }
}