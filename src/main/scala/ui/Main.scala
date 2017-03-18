package ui

import java.awt.Color
import java.awt.event.MouseEvent
import java.awt.geom.Ellipse2D
import javax.swing.BorderFactory

import core._

import scala.swing.BorderPanel.Position
import scala.swing.Swing._
import scala.swing.event._
import scala.swing.{Label, _}

object Main extends SimpleSwingApplication {
  val panel: MyPanel = MyPanel()

  def registerDraggedEvent(component: Component) {
    component.listenTo(component.mouse.moves)
    component.listenTo(component.mouse.clicks)
    var pressed: MouseEvent = null
    component.reactions += {
      case em: MousePressed =>
        pressed = em.peer
      case dragged: MouseDragged =>
        val component = dragged.source
        val x = component.location.x - pressed.getX + dragged.peer.getX
        val y = component.location.y - pressed.getY + dragged.peer.getY
        component.peer.setLocation(x, y)
        panel.repaint()
    }
  }

  def top = new MainFrame {
    title = "SwingApp"

    menuBar = new MenuBar {
      contents += new Menu("Elements") {
        contents += new MenuItem(Action("Repeater") {
          panel.spawn(Device.repeater())
        })
        contents += new MenuItem(Action("And") {
          panel.spawn(Device.and())
        })
        contents += new MenuItem(Action("Or") {
          panel.spawn(Device.or())
        })
        contents += new MenuItem(Action("Xor") {
          panel.spawn(Device.xor())
        })
        contents += new MenuItem(Action("Not") {
          panel.spawn(Device.not())
        })
        contents += new MenuItem(Action("Split") {
          panel.spawn(Device.split())
        })
        contents += new MenuItem(Action("ZERO") {
          panel.spawn(Device.zeroConst())
        })
        contents += new MenuItem(Action("ONE") {
          panel.spawn(Device.oneConst())
        })
      }
    }
    contents = panel

    peer.setSize(400, 300)
    peer.setVisible(true)
    peer.setLocationRelativeTo(null)
  }

  var outputSelected: Option[Pin] = None

  def getDistance(from: Component, to: Component): Int = from.locationOnScreen.distance(to.locationOnScreen).asInstanceOf[Int]

  def getText(device: Device): String = device match {
    case _: Repeater => "Repeater"
    case _: And => "And"
    case _: Or => "Or"
    case _: Xor => "Xor"
    case _: Not => "Not"
    case _: Split => "Split"
    case _: ZeroConst => "ZERO"
    case _: OneConst => "ONE"
    case _ => "UNKNOWN"
  }
}

case class Pin(uiDevice: UiDevice, alignment: Double, diameter: Int, input: Boolean, var selected: Boolean = false, val pos: Int = 0) extends ShapeComponent(new Ellipse2D.Double(alignment, 0, diameter, diameter)) {
  peer.setMaximumSize(new Dimension(diameter, diameter))

  listenTo(mouse.moves)
  listenTo(mouse.clicks)
  reactions += {
    case MouseEntered(_, _, _) if !selected =>
      color = Color.white
      repaint()
    case MouseExited(_, _, _) if !selected =>
      color = Color.black
      repaint()
    case MouseClicked(_, _, _, _, _) =>
      if (!input) {
        Main.outputSelected = Some(this)
        color = Color.white
        selected = true
      } else {
        Main.outputSelected match {
          case Some(outPin: Pin) =>
            Main.panel.spawn(Wire.create(outPin.uiDevice.device, outPin.pos, uiDevice.device, pos), outPin, this)
            outPin.color = Color.black
            outPin.selected = false
            Main.outputSelected = None
          case _ =>
        }
      }
  }

  def getLocation: Point = new Point(uiDevice.location.x + (if (input) 0 else uiDevice.width), uiDevice.location.y + location.y)
}


class UiWire(val wire: Wire, var from: Pin, var to: Pin) extends LineComponent(from, to) {
  override def computeColor(): Color = {
    if (wire.signal == ONE) return Color.red
    Color.black
  }
}

class UiDevice(val device: Device) extends BorderPanel() {
  val defaultDiameter = 10
  val (width, height) = (100, 70)
  border = BorderFactory.createLineBorder(Color.BLACK)
  peer.setSize(width, height)

  override def size: Dimension = new Dimension(width, height)

  private def initPinsPanel(size: Int, pins: Seq[Pin]): BoxPanel =
    new BoxPanel(Orientation.Vertical) {
      border = BorderFactory.createLineBorder(Color.BLACK)
      pins.foreach(pin => {
        contents += VGlue
        contents += pin
      })
      contents += VGlue
    }

  private def initPins(size: Int, alignment: Int, input: Boolean): Seq[Pin] = Range(0, size).map(i => Pin(this, alignment, defaultDiameter, input, pos = i))


  val label = new Label(Main.getText(device)) {
    horizontalAlignment = Alignment.Center
    foreground = Color.blue
    opaque = true
    border = BorderFactory.createLineBorder(Color.BLACK)
  }

  val inputPins: Seq[Pin] = initPins(device.inputSize(), 0, input = true)
  val outputPins: Seq[Pin] = initPins(device.outputSize(), width - defaultDiameter, input = false)

  private val inputPinsPanel: BoxPanel = initPinsPanel(device.inputSize(), inputPins)
  private val outPinsPanel: BoxPanel = initPinsPanel(device.inputSize(), outputPins)

  layout(label) = Position.Center
  layout(inputPinsPanel) = Position.West
  layout(outPinsPanel) = Position.East

  Main.registerDraggedEvent(this)
}


case class MyPanel() extends GridBagPanel {
  peer.setLayout(null)

  def spawn(device: Device) {
    val uiDevice = new UiDevice(device)
    uiDevice.peer.setLocation(0, 0)
    _contents += uiDevice
    repaint()
    revalidate()
  }

  def spawn(wire: Wire, from: Pin, to: Pin): Unit = {
    val uiWire = new UiWire(wire = wire, from = from, to = to)
    _contents += uiWire
    repaint()
    revalidate()
  }
}