
import scala.swing._
import java.awt.Dimension
import java.awt.Color
import scala.concurrent.ops._

trait Tetris{
//  lazy val field:List[Array[(bool,Color)]] = List.tabulate
  protected def reflect
  class Block(var x:Int, var y:Int, val speed:Int, var direction:Int, val pattern:Array[(Int,Int)], var tick:Int=0)
  val pattern = Array((0,0),(1,0),(2,0),(0,1))
  var block = new Block(0, 10, 100, 0, pattern)

  def start(){
    spawn{
      while(true){
	block.tick+=1
	if(block.tick >= block.speed){
	  block.y -= 1
	  block.tick=0
	}
	if(block.y == 0){
 	  block = new Block(0, 10, 100, 2, pattern)
	}
	reflect
	Thread.sleep(10)
      }
    }
  }
}

import math.{Pi, sin, cos, round}

object Main extends SimpleGUIApplication{

  def top = new MainFrame{
    val aSize = 50
    val panel = new Panel() with Tetris{
      def reflect = repaint
      peer.setPreferredSize(new Dimension(20 * aSize, 20 * aSize))
      override def paintComponent(g:Graphics2D) = {
        super.paintComponent(g)
	g.setColor(Color.BLACK)
	block.pattern.foreach{
	  case (x,y) =>
	    val rotx:Int = round(x * cos(Pi/2.0 * block.direction) - y * sin(Pi/2.0 * block.direction)).toInt
	    val roty:Int = round(x * sin(Pi/2.0 * block.direction) + y * cos(Pi/2.0 * block.direction)).toInt
	    g.fillRect(block.x + aSize * rotx,block.y*aSize + aSize * roty, aSize, aSize)
	}
      }
    }

    contents = panel
    panel.start()
  }
}
