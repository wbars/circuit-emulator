package core

import scala.collection.mutable.ListBuffer

abstract class Device() {
  def inputWireFree(position: Int): Boolean = incoming(position) match {
    case None => true
    case _ => false
  }

  def outputWireFree(position: Int): Boolean = outcoming(position) match {
    case None => true
    case _ => false
  }

  def releaseOutWire(position: Int): Unit = {
    outcoming(position) match {
      case Some(w: Wire) =>
        outcoming.update(position, None)
        w.from = None
        w.to match {
          case Some(to: Device) => to.reloadSignal()
          case _ =>
        }
      case _ =>
    }
  }

  var outcoming: ListBuffer[Option[Wire]] = ListBuffer.fill(outputSize())(Option.empty)
  var incoming: ListBuffer[Option[Wire]] = ListBuffer.fill(inputSize())(Option.empty)

  def releaseInWire(position: Int) {
    incoming(position) match {
      case Some(w: Wire) =>
        incoming.update(position, None)
        w.to = None
        reloadSignal()
      case _ =>
    }
  }

  def outWire(wire: Wire, position: Int) {
    if (!outputWireFree(position) || Wire.hasCycle(Some(this), wire.to)) throw new IllegalArgumentException
    outcoming.update(position, Some(wire))
    wire.from = Some(this)
    reloadSignal()
  }

  def outWire(wire: Wire): Unit = outWire(wire, 0)

  def inWire(wire: Wire, position: Int) {
    if (!inputWireFree(position) || Wire.hasCycle(wire.from, Some(this))) throw new IllegalArgumentException
    incoming.update(position, Some(wire))
    wire.to = Some(this)
    reloadSignal()
  }

  def inWire(wire: Wire): Unit = inWire(wire, 0)

  def inputSize(): Int

  def outputSize(): Int

  def computeSignal(): Signal

  def canAttachInputWire: Boolean = !incoming.forall(_.isDefined)

  def canAttachOutputWire: Boolean = !outcoming.forall(_.isDefined)

  def canExecute: Boolean = incoming.size == inputSize

  def reloadSignal() {
    outcoming.foreach({
      case Some(w: Wire) => w.sendSignal(computeSignal())
      case _ =>
    })
  }
}

case class Repeater() extends UnaryDevice() {
  override def computeSignal(): Signal = incoming.head match {
    case Some(w: Wire) => w.signal
    case _ => ZERO
  }
}

abstract class UnaryDevice() extends Device() {
  override def inputSize(): Int = 1

  override def outputSize(): Int = 1
}

abstract class BinaryReducer() extends Device() {
  override def inputSize(): Int = 2

  override def outputSize(): Int = 1
}

abstract class UnaryProducer() extends Device() {
  override def inputSize() = 0

  override def outputSize() = 1
}

case class ZeroConst() extends UnaryProducer() {
  override def computeSignal(): Signal = ZERO
}

case class OneConst() extends UnaryProducer() {
  override def computeSignal(): Signal = ONE
}

case class And() extends BinaryReducer() {
  override def computeSignal(): Signal = incoming.head match {
    case Some(Wire(ONE, _, _)) => incoming.last match {
      case Some(Wire(ONE, _, _)) => ONE
      case _ => ZERO
    }
    case _ => ZERO
  }
}

case class Xor() extends BinaryReducer() {
  override def computeSignal(): Signal = incoming.head match {
    case Some(Wire(ONE, _, _)) => incoming.last match {
      case Some(Wire(ONE, _, _)) => ZERO
      case _ => ONE
    }
    case _ => incoming.last match {
      case Some(Wire(ONE, _, _)) => ONE
      case _ => ZERO
    }
  }
}

case class Or() extends BinaryReducer() {
  override def computeSignal(): Signal = incoming.head match {
    case Some(Wire(ONE, _, _)) => ONE
    case _ => incoming.last match {
      case Some(w: Wire) => w.signal
      case _ => ZERO
    }
  }
}

case class Not() extends UnaryDevice() {
  override def computeSignal(): Signal = incoming.head match {
    case Some(Wire(ONE, _, _)) => ZERO
    case _ => ONE
  }
}

case class Split() extends Device() {
  override def inputSize(): Int = 1

  override def outputSize(): Int = 2

  override def computeSignal(): Signal = incoming.head match {
    case Some(w: Wire) => w.signal
    case _ => ZERO
  }
}

object Device {
  def repeater(): Device = Repeater()

  def and(): Device = And()

  def not(): Device = Not()

  def or(): Device = Or()

  def xor(): Device = Xor()

  def split(): Device = Split()

  def oneConst(): Device = OneConst()

  def zeroConst(): Device = ZeroConst()
}

