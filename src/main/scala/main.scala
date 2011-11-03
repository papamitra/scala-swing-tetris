
import scala.swing._
import java.awt.Dimension
import java.awt.Color
import scala.concurrent.ops._
import math.{Pi, sin, cos, round}
import scala.util.Random

trait Tetris{
  protected def reflect
  def height:Int = 20
  def width:Int = 15
  case class Point(val isExist:Boolean=false, val color:Color=Color.BLACK)
  lazy val field = List.tabulate(height+4){_ => Array.fill(width)(new Point)}
  lazy val random = new Random(System.currentTimeMillis)

  final case class BlockShape(val pattern:Array[(Int, Int)], val color:Color)
  val blocks = Array(BlockShape(Array((0,0),(1,0),(2,0),(0,1)), Color.BLUE),
		     BlockShape(Array((0,0),(1,0),(0,1),(1,1)), Color.YELLOW),
		     BlockShape(Array((0,0),(1,0),(2,0),(3,0)), Color.RED),
		     BlockShape(Array((0,0),(1,0),(1,1),(2,1)), Color.GREEN),
		     BlockShape(Array((0,0),(0,1),(1,0),(2,0)), Color.CYAN),
		     BlockShape(Array((0,0),(1,1),(1,0),(2,0)), Color.ORANGE))
  val blockNum = blocks.size
  
  def newShape = {
    val i = random.nextInt(blockNum)
    println("random ", i)
    blocks(i)
  }

  case class Block(var x:Int, var y:Int, val speed:Int, var direction:Int, val shape:BlockShape, var tick:Int=0){
    def rotate(){
      val newDirection = (direction + 1) % 4
      if(copy(direction = newDirection).isPossible)
	direction = newDirection
    }

    def color = shape.color

    def position = 
      shape.pattern.map{
	case (x,y) =>
	  val rotx:Int = round(x * cos(Pi/2.0 * direction) - y * sin(Pi/2.0 * direction)).toInt
	  val roty:Int = round(x * sin(Pi/2.0 * direction) + y * cos(Pi/2.0 * direction)).toInt
	  (rotx, roty)
      }

    def toRight(){
      if(copy(x=x+1).isPossible){
	x += 1
      }
    }
    def toLeft(){
      if(copy(x=x-1).isPossible){
	x -= 1
      }
    }

    def down():Boolean = {
      if(copy(y=y-1).isPossible){
	y -= 1
	return true
      }
      return false
    }

    def union(){
      position.foreach{
	case (posx, posy)=>
	  field(posy+y)(posx+x) = Point(true, color)
      }
    }

    def isPossible = {
      !position.exists{
	case (posx,posy) => 
	  (posx + x) < 0 || (posx + x) > width-1 || (posy + y) < 0 || field(posy+y)(posx+x).isExist
      }
    }

  }

  var block = new Block(width/2, height, 100, 2, newShape)

  def start(){
    println("start")
    spawn{
      while(true){
	block.tick+=1
	if(block.tick >= block.speed){
	  if(block.down()){
	    block.tick=0
	  }else{
	    block.union()
	    block = new Block(width/2, height, 100, 2, newShape)
	  }
	}
	reflect
	Thread.sleep(10)
      }
    }
  }

}

import scala.swing.event.KeyPressed
import scala.swing.event.KeyTyped
import scala.swing.event.MouseClicked

object Main extends SimpleSwingApplication{

  def top = new MainFrame{
    val aSize = 20
    val panel = new Panel() with Tetris{
//      val height = 20
//      val width = 15
      def reflect = repaint
      focusable = true
      peer.setPreferredSize(new Dimension(20 * aSize, 20 * aSize))
      override def paintComponent(g:Graphics2D) = {
        super.paintComponent(g)
	block.position.foreach{
	  case (x,y) =>
	    drowBlock(block.x + x, block.y + y, block.color ,g)
	}
	for((a,y) <- field.zipWithIndex;
	    (p,x) <- a.zipWithIndex){
	      if(p.isExist)
		drowBlock(x,y,p.color,g)
	    }
      }

      def drowBlock(x:Int, y:Int, c:Color, g:Graphics2D){
	g.setColor(c)
	g.fillRect(x*aSize, (height-y) * aSize, aSize, aSize)
      }

      listenTo(keys,mouse.clicks)
      reactions += {
//	case KeyPressed(source,char,modifiers,location) => println("key pressed", source)
	case KeyTyped(_,'c',_,_) => block.toRight()
	case KeyTyped(_,'x',_,_) => block.toLeft()
	case KeyTyped(_,'z',_,_) => block.rotate()
//	case MouseClicked(source, point, modifiers, clicks, triggersPopup) => block.rotate()
      }
    }

    contents = panel
    panel.start()
  }
}
