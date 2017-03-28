package ui

import java.awt.geom.{Path2D, Rectangle2D}
import java.awt.{Color, Point}

import core.{ONE, Wire}

case class UiWire(wire: Wire, var from: UiPin, var to: UiPin, var forcedColor: Option[Color] = None, var curvedPoint: Option[Point] = None) {
  def computeColor(): Color = {
    if (wire.signal == ONE) return Color.red
    Color.black
  }

  def isBelong(p: Point): Boolean = {
    path.intersects(expendedPoint(p))
  }

  val hoverOffset = 3

  private def expendedPoint(p: Point) = new Rectangle2D.Float(p.x - hoverOffset, p.y - hoverOffset, hoverOffset * 2, hoverOffset * 2)

  def bezierPivot(start: Point, middle: Point, end: Point) = new Point(2 * middle.x - start.x / 2 - end.x / 2, 2 * middle.y - start.y / 2 - end.y / 2)

  def path: Path2D.Float = {
    val path = new Path2D.Float()
    path.moveTo(start.x, start.y)
    curvedPoint match {
      case Some(p: Point) =>
        val p1 = bezierPivot(start, p, end)
        path.curveTo(start.x, start.y, p1.x, p1.y, end.x, end.y)
      case _ => path.lineTo(end.x, end.y)
    }
    path
  }

  def end: Point = new Point(to.getLocation.x + to.radius, to.getLocation.y + to.radius)

  def start: Point = new Point(from.getLocation.x - from.radius, from.getLocation.y + from.radius)
}
