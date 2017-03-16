package ui

import java.awt.Color
import java.awt.event.MouseEvent
import javax.swing.BorderFactory

import scala.swing.event._
import scala.swing.{Label, _}

object Main extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "SwingApp"

    contents = new GridBagPanel {
      _contents += new Label {
        text = "Hey"
        peer.setBorder(BorderFactory.createLineBorder(Color.black))
        peer.setSize(50, 50)
        registerDraggedEvent(this)
      }
      _contents += new Label {
        text = "Over there"
        peer.setBorder(BorderFactory.createLineBorder(Color.black))
        peer.setSize(70, 70)
        registerDraggedEvent(this)
      }
    }

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
