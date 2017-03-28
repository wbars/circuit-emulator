package ui

import java.awt.Color

import core.Bulb

class UiBulb(val bulb: Bulb) extends UiDevice(device = bulb, nameEditable = true) {
  def updateColor(): Unit = label.foreground = if (bulb.active) Color.red else Color.black //hack
}
