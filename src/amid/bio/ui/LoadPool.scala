package amid.bio.ui
import amid.bio.Bio
import java.io.ObjectInputStream
import java.io.FileInputStream
import amid.config.Config
import amid.bio.Pool

object LoadPool {
  def loadBio():Bio = {
    val stream = new ObjectInputStream(new FileInputStream(Config.POOL_FILE_NAME));
    val pool = stream.readObject().asInstanceOf[Pool];
    stream.close();
    return new Bio(pool);
  }
  
  def main(args: Array[String]) {
    val screen =new BioScreen(loadBio());
    screen.main(args);
  }
}