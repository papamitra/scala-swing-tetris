
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
  var field = List.fill(height+4)(Array.fill(width)(new Point))
  lazy val random = new Random(System.currentTimeMillis)

  final case class BlockShape(val pattern:Array[(Int, Int)], val color:Color)
  val blocks = Array(BlockShape(Array((0,0),(1,0),(2,0),(0,1)), Color.BLUE),
		     BlockShape(Array((0,0),(1,0),(0,1),(1,1)), Color.YELLOW),
		     BlockShape(Array((0,0),(1,0),(2,0),(3,0)), Color.RED),
		     BlockShape(Array((0,0),(1,0),(1,1),(2,1)), Color.GREEN),
		     BlockShape(Array((0,0),(2,1),(1,0),(2,0)), Color.CYAN),
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

    private def move(trans:(Int,Int)=>(Int,Int))=
      synchronized{
	val (tx, ty) = trans(x,y)
	if(copy(x=tx, y=ty).isPossible){
	  x=tx; y=ty
	  true
	}
	else false
      }

    def toRight(){
      move((x,y)=>(x+1,y))
    }
    def toLeft(){
      move((x,y)=>(x-1,y))
    }

    def down():Boolean = {
      move((x,y)=>(x,y-1))
    }

    private[Tetris] def union(){
      position.foreach{
	case (posx, posy)=>
	  field(posy+y)(posx+x) = Point(true, color)
      }
    }

    private[Tetris] def isPossible = {
      !position.exists{
	case (posx,posy) => 
	  (posx + x) < 0 || (posx + x) > width-1 || (posy + y) < 0 || field(posy+y)(posx+x).isExist
      }
    }

  }

  var block = new Block(width/2, height+1, 100, 2, newShape)

  private def deleteBlocks(){
    val newField = field.filterNot(_.forall(_.isExist))
    field = newField ++ List.fill(field.size - newField.size)(Array.fill(width)(new Point))
  }

  
  def start(){
    println("start")
    spawn{
      while(true){
	synchronized{
	block.tick+=1
	if(block.tick >= block.speed){
	  if(block.down()){
	    block.tick=0
	  }else{
	    block.union()
	    if(block.y == height+1){
	      // GameOver
	      field = List.fill(height+4)(Array.fill(width)(new Point))
	    }else
	      deleteBlocks()
	    block = new Block(width/2, height+1, 100, 2, newShape)
	  }
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
      peer.setPreferredSize(new Dimension((width + 2) * aSize, (height + 5) * aSize))
      override def paintComponent(g:Graphics2D) = {
        super.paintComponent(g)
	drawBackground(g)
	block.position.foreach{
	  case (x,y) =>
	    drawBlock(block.x + x, block.y + y, block.color ,g)
	}
	for((a,y) <- field.zipWithIndex;
	    (p,x) <- a.zipWithIndex){
	      if(p.isExist)
		drawBlock(x,y,p.color,g)
	    }
      }

      def drawBackground(g:Graphics2D){
	g.setColor(Color.BLACK)
	g.fillRect(1 * aSize, 4*aSize, width*aSize, (height+1)*aSize)
      }
      def drawBlock(x:Int, y:Int, c:Color, g:Graphics2D){
	val (offsetx, offsety) = (1, 4)
	val posx = (x + offsetx) *aSize
	val posy = ((height-y)+ offsety) * aSize

	g.setColor(c)
	g.fillRect(posx, posy, aSize-1, aSize-1)
	g.setColor(Color.WHITE)
	g.drawRect(posx, posy, aSize-1, aSize-1)
	g.setColor(Color.BLACK)
	g.drawLine(posx, posy, posx, posy+aSize-1)
	g.drawLine(posx, posy+aSize-1, posx+aSize-1, posy+aSize-1)
      }

      listenTo(keys,mouse.clicks)
      reactions += {
//	case KeyPressed(source,char,modifiers,location) => println("key pressed", source)
	case KeyTyped(_,'c',_,_) => block.toRight()
	case KeyTyped(_,'x',_,_) => block.toLeft()
	case KeyTyped(_,'z',_,_) => block.rotate()
	case KeyTyped(_,' ',_,_) => block.down()
//	case MouseClicked(source, point, modifiers, clicks, triggersPopup) => block.rotate()
      }
    }

    contents = panel
    panel.start()
  }
}
