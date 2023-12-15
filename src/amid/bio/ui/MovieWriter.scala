package amid.bio.ui
import com.xuggle.mediatool.ToolFactory
import java.awt.image.BufferedImage
import com.xuggle.xuggler.IRational
import java.util.concurrent.TimeUnit

class MovieWriter(val fileName:String, val width:Int, val height:Int) {
    //val FRAME_RATE = 22050;
    val FRAME_RATE = 10;
    val CHANNELS = 1;
    private val startTimeNanos = System.nanoTime();
    val startTime = System.currentTimeMillis();
    
    private val writer = ToolFactory.makeWriter(fileName);
    writer.addVideoStream(0, 0, IRational.make(FRAME_RATE, 1), width, height);
    
    def writeImage(image:BufferedImage) {
      writer.encodeVideo(0, image, System.nanoTime()-startTimeNanos, TimeUnit.NANOSECONDS)
    }
    
    def close() {
      writer.close();
    }
}