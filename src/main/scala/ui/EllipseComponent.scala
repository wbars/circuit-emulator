package ui

import java.awt.geom.Ellipse2D
import java.awt.{Color, Graphics2D, RenderingHints}

import scala.swing._


class EllipseComponent(val shape: Ellipse2D, val width: Double, var color: Color = Color.black, val input: Boolean) extends Panel {
  val lineLength: Int = width.toInt - shape.getWidth.toInt

  override protected def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    val bounds: Rectangle = shape.getBounds
    val insets: Insets = peer.getInsets

    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.translate(insets.left - bounds.x, insets.top - bounds.y)
    val beforeColor = g.getColor
    g.setColor(color)


    def shiftToRight(shift: Double) = g.translate(insets.left - bounds.x + shift, insets.top - bounds.y)

    def drawLine() = g.drawLine(0, (shape.getHeight / 2).toInt, lineLength, (shape.getHeight / 2).toInt)

    def drawCircle() = g.fill(shape)

    if (input) {
      drawCircle()
      shiftToRight(shape.getWidth)
      drawLine()
    } else {
      drawLine()
      shiftToRight(lineLength)
      drawCircle()
    }

    g.setColor(beforeColor)
  }

  override def preferredSize: Dimension = {
    val insets = peer.getInsets
    val bounds = shape.getBounds

    val width = insets.left + insets.right + bounds.width
    val height = insets.top + insets.bottom + bounds.height
    new Dimension(width + lineLength, height)
  }

  override def maximumSize: Dimension = preferredSize

  override def minimumSize: Dimension = preferredSize

  override def opaque: Boolean = false

  override def size: Dimension = preferredSize
}