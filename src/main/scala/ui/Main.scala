package ui

import java.awt.Color
import java.awt.event.MouseEvent

import core._

import scala.swing._
import scala.swing.event._

object Main extends SimpleSwingApplication {
  def highlightWire(pin: UiPin) {
    panel.findWire(pin) match {
      case Some(uiWire: UiWire) => panel.blinkColor(uiWire)
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

  val analysisPanel = new AnalysisPanel()

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
          analysisPanel.analyse()
          splitPane.resizeWeight = 0.8

          revalidate()
          repaint()
        })
      }
    }
  }

  var outputSelected: Option[UiPin] = None

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

  def removeWire(pin: UiPin): Unit = panel.removeWire(pin)
}