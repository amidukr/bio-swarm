package amid.bio.ui
import amid.bio.ChaoticHunger
import amid.bio.Food
import amid.bio.Bio

object NewPool {
   
  def createBio():Bio = {
    val bio = new Bio();
    
    val chaoticHunger = new ChaoticHunger();
    val food = new Food();
     /*for(i <- 0 to pool.width - 1;
         j <- 0 to pool.height - 1){
       println(i + "; " + j);
       pool.putCreature(Vector(i, j), new Chaotic())
     }*/
    
    val hungerPos = bio.randomVector();
    //pool.putCreature(hungerPos, new UpDownHunger())
    chaoticHunger.animate(bio, hungerPos);
    
    //for(i <- 0 to 100){pool.putCreature(randomVector(), new Hunger())}
    for(i <- 0 to 10000){
      val pos = bio.randomVector();
      if(bio.creatures.getPoolField(pos).creatures.length == 0){
        food.animate(bio, pos);
      }
    }
    
    //pool.putCreature(hungerPos, new Food())
    /*for(i <- 0 to 2000){
      var position = randomVector();
      while(pool.getPoolField(position).creatures.length != 0){position = randomVector()}
      pool.putCreature(position, new Food())
    }*/
    return bio;
   }
   
   def main(args: Array[String]) {
     val screen =new BioScreen(createBio());
     screen.main(args);
   }
}