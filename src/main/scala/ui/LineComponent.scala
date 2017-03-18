package ui

import java.awt.geom.Line2D
import java.awt.{Color, Dimension => _, Graphics2D => _, Panel => _, _}

import scala.swing._

class LineComponent(from: Pin, to: Pin) extends Panel {
  peer.setSize(1000, 1000)
  peer.setLocation(0, 0)
  peer.setOpaque(false)

  override protected def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    val g2d = g.create().asInstanceOf[Graphics2D]
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g2d.setColor(computeColor())
    g2d.setStroke(new BasicStroke(4))
    g2d.draw(new Line2D.Double(from.getLocation.x, from.getLocation.y, to.getLocation.x, to.getLocation.y))
    g2d.dispose()
  }

  override def preferredSize: Dimension = {
    new Dimension(Math.abs(from.getLocation.x - to.getLocation.x), Math.abs(from.getLocation.y - to.getLocation.y))
  }

  override def maximumSize: Dimension = preferredSize

  override def minimumSize: Dimension = preferredSize

  override def opaque: Boolean = false

  override def size: Dimension = preferredSize

  def computeColor(): Color = Color.black

}
