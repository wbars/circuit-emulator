package ui

import java.awt.{Color, Graphics2D, RenderingHints, Shape}

import scala.swing._


class ShapeComponent(val shape: Shape, var color: Color = Color.black) extends Panel {
  override protected def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    val bounds: Rectangle = shape.getBounds
    val insets: Insets = peer.getInsets

    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.translate(insets.left - bounds.x, insets.top - bounds.y)
    val beforeColor = g.getColor
    g.setColor(color)
    g.fill(shape)
    g.setColor(beforeColor)
  }

  override def preferredSize: Dimension = {
    val insets = peer.getInsets
    val bounds = shape.getBounds

    val width = insets.left + insets.right + bounds.width
    val height = insets.top + insets.bottom + bounds.height
    new Dimension(width, height)
  }

  override def maximumSize: Dimension = preferredSize

  override def minimumSize: Dimension = preferredSize

  override def opaque: Boolean = false

  override def size: Dimension = preferredSize
}