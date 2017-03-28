package ui

import java.awt.Color

import core.{ONE, Switch}

import scala.swing.event.MouseClicked

class UiToggleSwitch(val switch: Switch) extends UiDevice(device = switch, nameEditable = true) {
  reactions += {
    case _: MouseClicked =>
      switch.toggle()
      label.textField.foreground = if (switch.value == ONE) Color.red else Color.black
      Main.panel.refresh()
  }
}
