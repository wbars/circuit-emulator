package ui

import java.awt.Color

import scala.swing.Swing.VGlue
import scala.swing.{BoxPanel, Orientation}

class PinPanel(size: Int, input: Boolean, width: Int, uiDevice: UiDevice) extends BoxPanel(Orientation.Vertical) {
  peer.setOpaque(false)
  peer.setBackground(Color.white)

  private val defaultDiameter = 10

  private def initPins(size: Int, input: Boolean): Seq[UiPin] = Range(0, size).map(i => UiPin(uiDevice, width, defaultDiameter, input, pos = i))

  def computeWidth: Int = if (size > 0) width else 0

  initPins(size, input).foreach(pin => {
    contents += VGlue
    contents += pin
  })
  contents += VGlue
}
