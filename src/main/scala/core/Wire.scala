package core

case class Wire(private var _signal: Signal = ZERO, var from: Option[Device] = None, var to: Option[Device] = None) {
  def sendSignal(signal: Signal) {
    _signal = signal
    to match {
      case Some(d: Device) if d.canExecute => d.reloadSignal()
      case _ =>
    }
  }

  def signal: Signal = _signal
}

object Wire {
  def create(from: Device, fromPos: Int, to: Device, toPos: Int): Wire = {
    val wire = new Wire(from = Some(from), to = Some(to))
    to.inWire(wire, toPos)
    from.outWire(wire, fromPos)
    to.reloadSignal()
    from.reloadSignal()
    wire
  }

  def empty(): Wire = new Wire()
}
