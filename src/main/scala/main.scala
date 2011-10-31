
import scala.swing._

object Main extends SimpleGUIApplication{
  def top = new MainFrame{
    title = "First Swing App"
    contents = new Button{
      text = "Click me"
    }
  }
}
