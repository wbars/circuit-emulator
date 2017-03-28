package ui

import java.awt.geom.Ellipse2D
import java.awt.{Color, Point}

import core.Wire

import scala.swing.Dimension
import scala.swing.event.{MouseClicked, MouseEntered, MouseExited}

case class UiPin(uiDevice: UiDevice, override val width: Double, diameter: Int, override val input: Boolean, var selected: Boolean = false, pos: Int = 0) extends EllipseComponent(new Ellipse2D.Double(0, 0, diameter, diameter), width, input = input) {
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
          case Some(selectedThisPin: UiPin) if selectedThisPin == this => resetPinSelection(this)
          case Some(selectedAnotherPin: UiPin) =>
            resetPinSelection(selectedAnotherPin)
            selectedAnotherPin.repaint()
            selectInPin()
          case _ => selectInPin()
        }

      } else {
        Main.outputSelected match {
          case Some(outPin: UiPin) => this match {
            case takenInputPin: UiPin if !takenInputPin.free =>
              Main.panel.findWire(takenInputPin) match {
                case Some(UiWire(_, fromPin: UiPin, toPin: UiPin, _, _)) if fromPin.eq(outPin) && toPin.eq(takenInputPin) => Main.removeWire(takenInputPin)
                case _ => Main.highlightWire(takenInputPin)
              }
              resetPinSelection(outPin)
            case _ => outPin match {
              case _ if Wire.hasCycle(Some(outPin.uiDevice.device), Some(uiDevice.device)) =>
              case takenOutPin: UiPin if !takenOutPin.free =>
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

  private def selectOutPin(outPin: UiPin) = {
    Main.panel.spawn(Wire.create(outPin.uiDevice.device, uiDevice.device, outPin.pos, pos), outPin, this)
    resetPinSelection(outPin)
  }

  private def resetPinSelection(pin: UiPin) = {
    pin.color = Color.black
    pin.selected = false
    Main.outputSelected = None
  }

  def getLocation: Point = new Point(uiDevice.location.x + (if (input) 0 else uiDevice.width), uiDevice.location.y + location.y)
}
