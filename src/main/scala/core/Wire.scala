package core

case class Wire(private var _signal: Signal = ZERO, var from: Option[Device] = None, var to: Option[Device] = None) {
  def remove(): Unit = from match {
    case Some(f: Device) =>
      Wire.applyByPos(this, f, f.outcoming, (device: Device, pos: Int) => device.releaseOutWire(pos))
      to match {
        case Some(t: Device) => Wire.applyByPos(this, t, t.incoming, (device: Device, pos: Int) => device.releaseInWire(pos))
        case _ => throw new IllegalStateException
      }
    case _ =>
  }

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
  def applyByPos(wire: Wire, device: Device, wires: Seq[Option[Wire]], f: (Device, Int) => Unit) {
    Wire.findWirePos(wire, wires) match {
      case Some(pos: Int) => f.apply(device, pos)
      case _ =>
    }
  }

  def hasCycle(from: Option[Device], to: Option[Device]): Boolean = {
    def hasCycleIter(device: Option[Device], visitedDevices: Seq[Device]): Boolean = device match {
      case None => false
      case Some(d: Device) if visitedDevices.exists(_.eq(d)) => true
      case Some(d: Device) => !d.outcoming.collect({ case Some(w: Wire) => w })
        .forall(w => !hasCycleIter(w.to, visitedDevices ++ Seq(d)))
    }

    val visitedDevices = from match {
      case Some(d: Device) => Seq(d)
      case _ => Seq.empty
    }
    hasCycleIter(to, visitedDevices)
  }

  def create(from: Device, to: Device, fromPos: Int = 0, toPos: Int = 0): Wire = {
    if (Wire.hasCycle(Some(from), Some(to))) throw new IllegalArgumentException
    val wire = new Wire(from = Some(from), to = Some(to))
    to.inWire(wire, toPos)
    from.outWire(wire, fromPos)
    to.reloadSignal()
    from.reloadSignal()
    wire
  }

  def empty(): Wire = new Wire()

  def findWirePos(wire: Wire, wires: Seq[Option[Wire]]): Option[Int] = wires.indices.find(wires(_) match {
    case Some(w: Wire) => w.eq(wire)
    case _ => false
  })
}
