
import scala.swing._
import java.awt.Dimension
import java.awt.Color
import scala.concurrent.ops._
import math.{Pi, sin, cos, round}
import scala.util.Random

trait TetrisModels{
  def height:Int
  def width:Int

  trait TBlock{
    def x:Int
    def y:Int
    def color:Color
    def position:Array[(Int,Int)]
    def rotate():Unit
    def toLeft():Unit
    def toRight():Unit
    def down():Boolean
  }
  case class Point(val isExist:Boolean=false, val color:Color=Color.BLACK)
  def block:TBlock
  var field:List[Array[Point]]
}

trait TetrisViews{
  def reflect:Unit
}

trait TetrisControls{
}

trait Ghost{
  self: TetrisModels=>
}

trait TetrisModesImpl extends TetrisModels{
  self: TetrisViews =>
  def height:Int = 20
  def width:Int = 10

  var field = List.fill(height+4)(Array.fill(width)(new Point))
  lazy val random = new Random(System.currentTimeMillis)

  final case class Tetrimino(val pattern:Array[(Int, Int)], val color:Color)
  val blocks = Array(Tetrimino(Array((0,0),(1,0),(2,0),(0,1)), Color.BLUE),
		     Tetrimino(Array((0,0),(1,0),(0,1),(1,1)), Color.YELLOW),
		     Tetrimino(Array((0,0),(1,0),(2,0),(3,0)), Color.RED),
		     Tetrimino(Array((0,0),(1,0),(1,1),(2,1)), Color.GREEN),
		     Tetrimino(Array((0,0),(2,1),(1,0),(2,0)), Color.CYAN),
		     Tetrimino(Array((0,0),(1,1),(1,0),(2,0)), Color.ORANGE))
  val blockNum = blocks.size
  
  def newShape = {
    val i = random.nextInt(blockNum)
    println("random ", i)
    blocks(i)
  }

  def createBlock = Block(width/2-2, height, 100, 0, newShape)
  var block:Block = createBlock

  case class Block(var x:Int, var y:Int, val speed:Int, var direction:Int, val shape:Tetrimino, var tick:Int=0) extends TBlock{
    def rotate(){
      val newDirection = (direction + 1) % 4
      if(isPossible(copy(direction = newDirection)))
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
	if(isPossible(copy(x=tx, y=ty))){
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
  }
  
  def union(block:Block){
    val (x,y) = (block.x, block.y)
    block.position.foreach{
      case (posx, posy)=>
	field(posy+y)(posx+x) = Point(true, block.color)
    }
  }

  def isPossible(block:Block) = {
    val (x,y) = (block.x, block.y)
    !block.position.exists{
      case (posx,posy) => 
	(posx + x) < 0 || (posx + x) > width-1 || (posy + y) < 0 || field(posy+y)(posx+x).isExist
    }
  }

  private def deleteBlocks(){
    val newField = field.filterNot(_.forall(_.isExist))
    field = newField ++ List.fill(field.size - newField.size)(Array.fill(width)(new Point))
  }

  private def clearBlocks(){
    field = List.fill(height+4)(Array.fill(width)(new Point))
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
	    union(block)
	    if(block.y >= height){
	      // GameOver
	      clearBlocks()
	    }else
	      deleteBlocks()
	    block = createBlock
	  }
	}
	}
	reflect
	Thread.sleep(10)
      }
    }
  }

}

trait TetrisViewsImpl extends TetrisViews{
  self: Panel with TetrisModels=>
  val aSize = 20
  def reflect = repaint

  def paintBlocks(g:Graphics2D){
    synchronized{
      drawBackground(g)
      drawMino(g)
      drawField(g)
    }
  }

  def drawMino(g:Graphics2D){
      block.position.foreach{
	case (x,y) =>
	  drawBlock(block.x + x, block.y + y, g, block.color)
      }
  }
  def drawField(g:Graphics2D){
      for((a,y) <- field.zipWithIndex;
	  (p,x) <- a.zipWithIndex){
	    if(p.isExist)
	      drawBlock(x,y,g, p.color)
	  }
  }
  def drawBackground(g:Graphics2D){
    g.setColor(Color.BLACK)
    g.fillRect(1 * aSize, 5*aSize, width*aSize, height*aSize)
    for(h <- Range(0,height)){
      drawBlock(-1, h, g, Color.LIGHT_GRAY)
      drawBlock(width, h, g, Color.LIGHT_GRAY)
    }
    for(w <- Range(-1,width+1)){
      drawBlock(w, -1, g, Color.LIGHT_GRAY)
    }
  }

  def drawBlock(x:Int, y:Int, g:Graphics2D, c:Color){
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

}

import scala.swing.event.KeyTyped
trait TetrisControlsImpl extends TetrisControls{
  self: Panel with TetrisModels =>
    listenTo(keys)
    reactions += {
      case KeyTyped(_,'c',_,_) => block.toRight()
      case KeyTyped(_,'x',_,_) => block.toLeft()
      case KeyTyped(_,'z',_,_) => block.rotate()
      case KeyTyped(_,' ',_,_) => block.down()
    }
}

import scala.swing.event.KeyPressed
import scala.swing.event.KeyTyped
import scala.swing.event.MouseClicked

object Main extends SimpleSwingApplication{

  def top = new MainFrame{
    val aSize = 20
    val panel = new Panel() with TetrisModesImpl with TetrisViewsImpl with TetrisControlsImpl{
      focusable = true
      peer.setPreferredSize(new Dimension((width + 2) * aSize, (height + 6) * aSize))
      override def paintComponent(g:Graphics2D){
	super.paintComponent(g)
	paintBlocks(g)
    }
    }

    contents = panel
    panel.start()
  }
}
