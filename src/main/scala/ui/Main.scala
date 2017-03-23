package ui

import java.awt.event.MouseEvent
import java.awt.geom.Ellipse2D
import java.awt.{Color, Font}
import javax.swing.{BorderFactory, SwingWorker}

import core._

import scala.swing.Swing._
import scala.swing._
import scala.swing.event._

object Main extends SimpleSwingApplication {
  def highlightWire(pin: Pin) {
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

  val analysisPanel = new BoxPanel(Orientation.Vertical) {
    def setContent(truthTable: Table, formulas: Component) {
      tablePanel.contents.clear()
      tablePanel.contents += HGlue
      tablePanel.contents += new ScrollPane(truthTable)
      tablePanel.contents += HGlue

      formulaTable.contents.clear()
      formulaTable.contents += VGlue
      formulaTable.contents += formulas
      formulaTable.contents += VGlue
    }

    peer.setBackground(Color.white)
    border = BorderFactory.createLineBorder(Color.black)
    contents += new BoxPanel(Orientation.Horizontal) {
      peer.setBackground(Color.white)
      border = BorderFactory.createLineBorder(Color.black)
      contents += HGlue
      contents += new Label("Analysis")
      contents += HGlue
    }
    val tablePanel = new BoxPanel(Orientation.Horizontal)
    val formulaTable = new BoxPanel(Orientation.Vertical) {
      peer.setBackground(Color.white)
    }
    contents += tablePanel
    contents += VGlue
    contents += formulaTable
    peer.setMinimumSize(300, 300)
  }

  val splitPane = new SplitPane(Orientation.Vertical, panel, analysisPanel) {
    peer.setBackground(Color.white)
    resizeWeight = 0.8
  }

  def top = new MainFrame {
    title = "Circuit emulator"

    menuBar = createMenu()
    contents = splitPane

    peer.setVisible(true)
    peer.setLocationRelativeTo(null)
    peer.setExtendedState(java.awt.Frame.MAXIMIZED_BOTH)
    peer.setBackground(Color.white)
  }

  private def createMenu() = {
    new MenuBar {
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

      contents += new Menu("Analysis") {
        contents += new MenuItem(Action("Analyse") {
          panel.analyse()
          splitPane.resizeWeight = 0.8

          revalidate()
          repaint()
        })
      }
    }
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

case class Pin(uiDevice: UiDevice, override val width: Double, diameter: Int, override val input: Boolean, var selected: Boolean = false, pos: Int = 0) extends EllipseComponent(new Ellipse2D.Double(0, 0, diameter, diameter), width, input = input) {
  peer.setBackground(Color.white)

  def radius: Int = diameter / 2

  def free: Boolean = if (input) {
    uiDevice.device.inputWireFree(pos)
  } else {
    uiDevice.device.outputWireFree(pos)
  }

  peer.setMaximumSize(new Dimension(diameter + lineLength, diameter))

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
                case _ => Main.highlightWire(takenInputPin)
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

class PinPanel(size: Int, input: Boolean, width: Int, uiDevice: UiDevice) extends BoxPanel(Orientation.Vertical) {
  peer.setOpaque(false)
  peer.setBackground(Color.white)

  private val defaultDiameter = 10

  private def initPins(size: Int, input: Boolean): Seq[Pin] = Range(0, size).map(i => Pin(uiDevice, width, defaultDiameter, input, pos = i))

  def computeWidth: Int = if (size > 0) width else 0

  initPins(size, input).foreach(pin => {
    contents += VGlue
    contents += pin
  })
  contents += VGlue
}

class UiDevice(val device: Device) extends BoxPanel(Orientation.Horizontal) {
  def getName: String = label.textField.text

  val pinPanelWidth = 30
  val labelWidth = 60

  val inputPinsPanel = new PinPanel(device.inputSize(), input = true, width = pinPanelWidth, uiDevice = this)
  val outputPinsPanel = new PinPanel(device.outputSize(), input = false, width = pinPanelWidth, uiDevice = this)

  val (width, height) = (labelWidth + inputPinsPanel.computeWidth + outputPinsPanel.computeWidth, 70)
  peer.setSize(width, height)
  peer.setOpaque(false)
  peer.setBackground(Color.white)

  val label = new BoxPanel(Orientation.Vertical) {
    val fontSize = 12
    peer.setBackground(Color.white)
    peer.setMaximumSize(labelWidth, height)
    border = BorderFactory.createLineBorder(Color.black)

    val textField = new TextField(Main.getText(device)) {
      horizontalAlignment = Alignment.Center
      background = null
      foreground = Color.black
      border = null
      opaque = true
      xLayoutAlignment = java.awt.Component.CENTER_ALIGNMENT
      yLayoutAlignment = java.awt.Component.CENTER_ALIGNMENT
      maximumSize = new Dimension(width, fontSize)
      font = new Font("TimesNewRoman", Font.PLAIN, fontSize)
      listenTo(keys)
      reactions += {
        case _: KeyEvent => device match {
          case switch: Switch => switch.setName(text)
          case _ =>
        }
      }
    }
    contents += VGlue
    contents += textField
    contents += VGlue
  }

  private def tryAddPanel(pinPanel: PinPanel) = if (pinPanel.computeWidth > 0) contents += pinPanel

  tryAddPanel(inputPinsPanel)
  contents += label
  tryAddPanel(outputPinsPanel)

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
  val cellSize = 30


  def formulaTable(formulas: Seq[(UiBulb, String)]): Panel = new BoxPanel(Orientation.Vertical) {
    peer.setBackground(Color.white)
    formulas.foreach(t => {
      contents += VGlue
      contents += new Label(t._1.getName + ": " + t._2)
    })
    contents += VGlue
  }

  def buildTable(analysisResult: Seq[(Seq[(Switch, Signal)], Seq[(Bulb, Signal)])],
                 input: Seq[Switch],
                 outputs: Seq[Bulb],
                 formulas: Seq[(UiBulb, String)]) {
    Main.analysisPanel.setContent(truthTable(analysisResult), formulaTable(formulas))
  }

  private def truthTable(analysisResult: Seq[(Seq[(Switch, Signal)], Seq[(Bulb, Signal)])]) = {
    val columnNames: Seq[String] = switches.map(_.getName) ++ builbs.map(_.getName)
    val data: Array[Array[Any]] = analysisResult.map(t => (t._1.map(_._2.display()) ++ t._2.map(_._2.display())).toArray[Any]).toArray
    new Table(data, columnNames) {
      peer.setSize(columnNames.size * cellSize, data.length * cellSize)
    }
  }

  def analyse() {
    val input = switches.map(_.switch)
    val outputs = builbs.map(_.bulb)
    buildTable(DeviceAnalyser.analyse(input, outputs), input, outputs, builbs.map(uiBulb => (uiBulb, DeviceAnalyser.formula(uiBulb.bulb))))
  }

  def refresh(): Unit = {
    builbs.foreach(_.updateColor())
    repaint()
    revalidate()
  }

  peer.setBackground(Color.white)
  peer.setLayout(null)
  peer.setOpaque(false)

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

  def switches: Seq[UiToggleSwitch] = contents.collect({ case w: UiToggleSwitch => w })
}