package ui

import java.awt.{BasicStroke, Color, Point, RenderingHints}
import javax.swing.SwingWorker

import core.Wire

import scala.collection.mutable.ListBuffer
import scala.swing.event.{MouseDragged, MouseMoved, MousePressed, MouseReleased}
import scala.swing.{Graphics2D, GridBagPanel}

case class MyPanel() extends GridBagPanel {
  def refresh(): Unit = {
    bulbs.foreach(_.updateColor())
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

  var wires: ListBuffer[UiWire] = ListBuffer.empty

  def spawn(wire: Wire, from: UiPin, to: UiPin): Unit = {
    wires += UiWire(wire = wire, from = from, to = to)
    refresh()
  }

  def removeWire(pin: UiPin) {
    findWire(pin) match {
      case Some(uiWire: UiWire) =>
        wires -= uiWire
        uiWire.wire.remove()
        refresh()
      case _ =>
    }
  }

  def blinkColor(uiWire: UiWire, count: Int = 3, rate: Int = 300, color: Color = Color.blue) {
    new SwingWorker[Unit, Unit]() {
      override def doInBackground() {
        for (_ <- 1 to count) {
          uiWire.forcedColor = Some(color)
          repaint()
          Thread.sleep(rate)

          uiWire.forcedColor = None
          repaint()
          Thread.sleep(rate)
        }
      }
    }.execute()
  }


  override protected def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    val g2d = g.create().asInstanceOf[Graphics2D]
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g2d.setStroke(new BasicStroke(4))
    wires.foreach(uiWire => {
      g2d.setColor(uiWire.forcedColor.getOrElse(uiWire.computeColor()))
      g2d.draw(uiWire.path)
    })
    g2d.dispose()

  }

  def findWire(pin: UiPin): Option[UiWire] = wires.find(w => w.from == pin || w.to == pin)

  def bulbs: Seq[UiBulb] = contents.collect({ case w: UiBulb => w })

  def switches: Seq[UiToggleSwitch] = contents.collect({ case w: UiToggleSwitch => w })

  listenTo(mouse.moves)
  listenTo(mouse.clicks)

  var draggedWire: Option[UiWire] = None
  reactions += {
    case MouseMoved(_, p: Point, _) =>
      val hoverWiresPartition = wires.partition(_.isBelong(p))
      hoverWiresPartition._1.foreach(_.forcedColor = Some(Color.orange))
      hoverWiresPartition._2.filter(_.forcedColor.contains(Color.orange)).foreach(_.forcedColor = None)
      repaint()

    case MousePressed(_, p: Point, _, _, _) => draggedWire match {
      case None =>
        draggedWire = wires.find(_.isBelong(p))
      case _ =>
    }

    case MouseReleased(_, _, _, _, _) => draggedWire = None

    case MouseDragged(_, p: Point, _) => draggedWire match {
      case Some(uiWire: UiWire) =>
        uiWire.curvedPoint = Some(p)
        repaint()
      case _ =>
    }
  }

}
