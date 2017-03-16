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
      }
    }

    def getText(device: Device): String = device match {
      case _: Repeater => "Repeater"
      case _: And => "And"
      case _: Or => "Or"
      case _: Xor => "Xor"
      case _: Not => "Not"
      case _: Split => "Split"
      case _ => "UNKNOWN"
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
    }

    def getDistance(from: Component, to: Component): Int = from.location.distance(to.location).asInstanceOf[Int]

    class UiWire(val wire: Wire, var from: UiDevice, var to: UiDevice) extends ShapeComponent(new Rectangle(from.location, new Dimension(getDistance(from, to), 5))) {}

    class Pin(val alignment: Double, val diameter: Int, val input: Boolean) extends ShapeComponent(new Ellipse2D.Double(alignment, 0, diameter, diameter))


    class UiDevice(val device: Device) extends BorderPanel() {
      val defaultDiameter = 10
      val (width, height) = (100, 70)
      border = BorderFactory.createLineBorder(Color.BLACK)
      peer.setSize(width, height)

      override def size: Dimension = new Dimension(width, height)

      private def initPinsPanel(size: Int, pins: Seq[Pin]): BoxPanel = {
        new BoxPanel(Orientation.Vertical) {
          border = BorderFactory.createLineBorder(Color.BLACK)
          pins.foreach(pin => {
            contents += VGlue
            contents += pin
          })
          contents += VGlue
        }
      }

      private def initPins(size: Int, alignment: Int, input: Boolean): Seq[Pin] = Range(0, size).map(_ => new Pin(alignment, defaultDiameter, input) {
        peer.setMaximumSize(new Dimension(defaultDiameter, defaultDiameter))

        listenTo(mouse.moves)
        listenTo(mouse.clicks)
        reactions += {
          case MouseEntered(_, _, _) =>
            color = Color.white
            repaint()
          case MouseExited(_, _, _) =>
            color = Color.black
            repaint()
        }
      })


      val label = new Label(getText(device)) {
        horizontalAlignment = Alignment.Center
        foreground = Color.blue
        opaque = true
        border = BorderFactory.createLineBorder(Color.BLACK)
      }

      val inputPins: BoxPanel = initPinsPanel(device.inputSize(), initPins(device.inputSize(), 0, input = true))
      val outPins: BoxPanel = initPinsPanel(device.inputSize(), initPins(device.outputSize(), width - defaultDiameter, input = false))

      layout(label) = Position.Center
      layout(inputPins) = Position.West
      layout(outPins) = Position.East

      registerDraggedEvent(this)
    }

    val panel = MyPanel()
    contents = panel

    peer.setSize(400, 300)
    peer.setVisible(true)
    peer.setLocationRelativeTo(null)
  }

  private def registerDraggedEvent(component: Component) = {
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
    }
  }
}
