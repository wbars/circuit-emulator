package ui

import java.awt.Color
import java.awt.event.MouseEvent
import java.awt.geom.Ellipse2D
import javax.swing.{BorderFactory, SwingWorker}

import core._

import scala.swing.BorderPanel.Position
import scala.swing.Swing._
import scala.swing._
import scala.swing.event._

object Main extends SimpleSwingApplication {
  def highligthWire(pin: Pin) {
    def blinkColor(uiWire: UiWire, count: Int = 3, rate: Int = 300, color: Color = Color.blue) = {
      new SwingWorker[Unit, Unit]() {
        override def doInBackground() {
          for (_ <- 1 to count) {
            uiWire.forcedColor = Some(color)
            uiWire.repaint()
            Thread.sleep(rate)

            uiWire.forcedColor = None
            uiWire.repaint()
            Thread.sleep(rate)
          }
        }
      }.execute()
    }

    panel.findWire(pin) match {
      case Some(uiWire: UiWire) => blinkColor(uiWire)
      case _ =>
    }
  }

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
          panel.spawn(new UiDevice(Device.repeater()))
        })
        contents += new MenuItem(Action("And") {
          panel.spawn(new UiDevice(Device.and()))
        })
        contents += new MenuItem(Action("Or") {
          panel.spawn(new UiDevice(Device.or()))
        })
        contents += new MenuItem(Action("Xor") {
          panel.spawn(new UiDevice(Device.xor()))
        })
        contents += new MenuItem(Action("Not") {
          panel.spawn(new UiDevice(Device.not()))
        })
        contents += new MenuItem(Action("Split") {
          panel.spawn(new UiDevice(Device.split()))
        })
        contents += new MenuItem(Action("ZERO") {
          panel.spawn(new UiDevice(Device.zeroConst()))
        })
        contents += new MenuItem(Action("ONE") {
          panel.spawn(new UiDevice(Device.oneConst()))
        })
        contents += new MenuItem(Action("Switch") {
          panel.spawn(new UiToggleSwitch(Device.switch()))
        })
        contents += new MenuItem(Action("Bulb") {
          panel.spawn(new UiBulb(Device.bulb()))
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
    case _: Switch => "Switch"
    case _: Bulb => "Bulb"
    case _ => "UNKNOWN"
  }

  def removeWire(pin: Pin): Unit = panel.removeWire(pin)
}

case class Pin(uiDevice: UiDevice, alignment: Double, diameter: Int, input: Boolean, var selected: Boolean = false, pos: Int = 0) extends ShapeComponent(new Ellipse2D.Double(alignment, 0, diameter, diameter)) {
  def radius: Int = diameter / 2

  def free: Boolean = if (input) {
    uiDevice.device.inputWireFree(pos)
  } else {
    uiDevice.device.outputWireFree(pos)
  }

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
        Main.outputSelected match {
          case Some(selectedThisPin: Pin) if selectedThisPin == this => resetPinSelection(this)
          case Some(selectedAnotherPin: Pin) =>
            resetPinSelection(selectedAnotherPin)
            selectedAnotherPin.repaint()
            selectInPin()
          case _ => selectInPin()
        }

      } else {
        Main.outputSelected match {
          case Some(outPin: Pin) => this match {
            case takenInputPin: Pin if !takenInputPin.free =>
              Main.panel.findWire(takenInputPin) match {
                case Some(UiWire(_, fromPin: Pin, toPin: Pin)) if fromPin.eq(outPin) && toPin.eq(takenInputPin) => Main.removeWire(takenInputPin)
                case _ => Main.highligthWire(takenInputPin)
              }
              resetPinSelection(outPin)
            case _ => outPin match {
              case _ if Wire.hasCycle(Some(outPin.uiDevice.device), Some(uiDevice.device)) =>
              case takenOutPin: Pin if !takenOutPin.free =>
                Main.removeWire(takenOutPin)
                selectOutPin(outPin)
              case _ => selectOutPin(outPin)
            }
          }
          case _ =>
        }
      }
  }

  private def selectInPin() = {
    Main.outputSelected = Some(this)
    color = Color.white
    selected = true
  }

  private def selectOutPin(outPin: Pin) = {
    Main.panel.spawn(Wire.create(outPin.uiDevice.device, uiDevice.device, outPin.pos, pos), outPin, this)
    resetPinSelection(outPin)
  }

  private def resetPinSelection(pin: Pin) = {
    pin.color = Color.black
    pin.selected = false
    Main.outputSelected = None
  }

  def getLocation: Point = new Point(uiDevice.location.x + (if (input) 0 else uiDevice.width), uiDevice.location.y + location.y)
}


case class UiWire(wire: Wire, var from: Pin, var to: Pin) extends LineComponent(from, to) {
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
    foreground = Color.black
    opaque = true
    border = BorderFactory.createLineBorder(Color.black)
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

class UiToggleSwitch(val switch: Switch) extends UiDevice(device = switch) {
  reactions += {
    case _: MouseClicked =>
      switch.toggle()
      label.foreground = if (switch.value == ONE) Color.red else Color.black
      Main.panel.refresh()
  }
}

class UiBulb(val bulb: Bulb) extends UiDevice(device = bulb) {
  def updateColor(): Unit = label.foreground = if (bulb.active) Color.red else Color.black //hack
}


case class MyPanel() extends GridBagPanel {
  def refresh(): Unit = {
    builbs.foreach(_.updateColor())
    repaint()
    revalidate()
  }

  peer.setLayout(null)

  def spawn(uiDevice: UiDevice) {
    uiDevice.peer.setLocation(0, 0)
    _contents += uiDevice
    refresh()
  }

  def spawn(wire: Wire, from: Pin, to: Pin): Unit = {
    val uiWire = UiWire(wire = wire, from = from, to = to)
    _contents += uiWire
    refresh()
  }

  def removeWire(pin: Pin) {
    findWire(pin) match {
      case Some(uiWire: UiWire) =>
        _contents -= uiWire
        uiWire.wire.remove()
        refresh()
      case _ =>
    }
  }

  def findWire(pin: Pin): Option[UiWire] = wires.find(w => w.from == pin || w.to == pin)

  def wires: Seq[UiWire] = contents.collect({ case w: UiWire => w })

  def builbs: Seq[UiBulb] = contents.collect({ case w: UiBulb => w })
}