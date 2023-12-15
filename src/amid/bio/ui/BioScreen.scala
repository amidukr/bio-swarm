package amid.bio.ui

import scala.swing._
import scala.swing.event.ButtonClicked
import scala.util.Random
import javax.swing.Timer
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import javax.swing.BorderFactory
import java.awt.Color
import scala.swing.event.MousePressed
import scala.swing.event.MouseEntered
import scala.swing.event.MouseClicked
import amid.utils.Utils
import amid.bio.Vector
import amid.bio.ChaoticHunger
import amid.bio.Food
import amid.bio.Bio
import java.io.OutputStreamWriter
import java.io.FileOutputStream
import java.io.ObjectOutputStream
import amid.config.Config
import java.awt.image.BufferedImage
import java.io.File
import java.util.concurrent.TimeUnit

class BioScreen(val bio:Bio = new Bio()) extends SimpleSwingApplication{
    
    private var movWriter:MovieWriter = null;
    nextFile();
    
    private def nextFile() {
      if(movWriter != null) movWriter.close();
      movWriter = new MovieWriter(unusedFileName(), 1024, 1024);
    }
    
    private def unusedFileName() : String = {
      var i:Int = 1;
      while(true){
        
         val file = new File(Config.POOL_FILE_NAME + "-" + i + "-.avi");
         if(!file.exists()) return file.getPath();
         i = i + 1;
      }
      
      throw new RuntimeException("Unexpected end of loop");
    }
    
    override def startup (args: Array[String]): Unit = {
        println("Hello, Swing!")
        super.startup(args)
    }
    
    private def savePool(){
      val stream = new ObjectOutputStream(new FileOutputStream(Config.POOL_FILE_NAME));
      stream.writeObject(bio.pool);
      stream.close();
    }
    
    def top = new MainFrame {
        title = "Hello World"
          
        override def closeOperation(){
           println("Closed");
           movWriter.close();
           super.closeOperation();
        }
        
        object fieldPanel extends Component{
            border = BorderFactory.createLineBorder(Color.RED)
            preferredSize = new Dimension(200, 200)
            
            
            listenTo(this.mouse.clicks);
            
            this.reactions += {
              case e:MouseClicked => println("Mouse pressed: " + e.point)
            }
            
            private def drawPool(g:Graphics2D, screenWidth:Int, screenHeight:Int) = {
              //println("Draw me!: " + size.width + "x" + size.height)
                
                //g.setColor(new Color(0xD3FFE7));
                g.setColor(Color.WHITE);
                g.fillRect(0, 0, screenWidth, screenHeight)
                
                val poolWidth  = bio.width;
                val poolHeight = bio.height;
                
                val fieldWidth  = 1.0*screenWidth  / (poolWidth + 1);
                val fieldHeight = 1.0*screenHeight / (poolHeight + 1);
                
                
                for(position <- bio.creatures.positions){
                  var field = bio.creatures.getPoolField(position);
                  if(field.creatures.length != 0){
                      var color = field.creatures(0).color;
                      if(field.creatures.length == 2){
                        color = Utils.blendColor(color, field.creatures(1).color, 0.5);
                      }
	                  g.setColor(color)
	                  val x = (position.x * fieldWidth)  toInt;
	                  val y = (position.y * fieldHeight) toInt;
	                  val w = ((position.x + 1) * fieldWidth  - x) toInt;
	                  val h = ((position.y + 1) * fieldHeight - y) toInt;
	                  g.fillRect(x, y, w, h)
                  }
                }
            }
            
            override def paintComponent(g:Graphics2D) = {
                super.paintComponent(g);
                drawPool(g, size.width, size.height);
                
                if(System.currentTimeMillis() - movWriter.startTime > 2*60*1000) nextFile();
                
                val bmp = new BufferedImage(movWriter.width, movWriter.height, BufferedImage.TYPE_3BYTE_BGR);
                drawPool(bmp.getGraphics().asInstanceOf[Graphics2D], movWriter.width, movWriter.height);
                movWriter.writeImage(bmp);
            }
        }
        contents = new BorderPanel{
          val nextStep = new Button(){text="Next step"}
          val save     = new Button(){ text="Save"}
          val south    = new BorderPanel(){
            add(nextStep, BorderPanel.Position.West);
            add(save,     BorderPanel.Position.East);
          };
          
          add(fieldPanel, BorderPanel.Position.Center);
          add(south, BorderPanel.Position.South)
          
          listenTo(nextStep);
          listenTo(save);
          
          
          nextStep.action = Action("nextStep"){
            println("Next Step");
            bio.doStep();
            fieldPanel.repaint();
          };
          
          save.action = Action("save"){
            println("Save");
            savePool();
          }
        }
        
        new Timer(1, new ActionListener(){
          def actionPerformed(e: ActionEvent) {
             try{
               nextStep();
             }catch{
               case e:Throwable => e.printStackTrace();
             }
          }
        }).start();
        
        def nextStep(){
          bio.doStep();
          fieldPanel.repaint();
        }
    }
}