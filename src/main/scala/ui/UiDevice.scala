package ui

import java.awt.{Color, Font}
import javax.swing.BorderFactory

import core.{Device, Switch}

import scala.swing.Swing.VGlue
import scala.swing._
import scala.swing.event.KeyEvent

class UiDevice(val device: Device, val nameEditable: Boolean = false) extends BoxPanel(Orientation.Horizontal) {
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
    peer.setMaximumSize(new Dimension(labelWidth, height))
    border = BorderFactory.createLineBorder(Color.black)

    val textField = new TextField(Main.getText(device)) {
      editable = nameEditable
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
