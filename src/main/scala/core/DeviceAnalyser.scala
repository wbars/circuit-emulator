package core

object DeviceAnalyser {
  def signalsCombinations(switches: Seq[Switch]): Seq[Seq[(Switch, Signal)]] = switches.size match {
    case 0 => Seq.empty
    case 1 => Seq(
      Seq((switches.head, ZERO)),
      Seq((switches.head, ONE))
    )
    case _ => signalsCombinations(Seq(switches.head))
      .flatMap(base => signalsCombinations(switches.tail).map(base ++ _))
  }

  def analyse(inputs: Seq[Switch], outputs: Seq[Bulb]): Seq[(Seq[(Switch, Signal)], Seq[(Bulb, Signal)])] = {
    val originalValues = inputs.map(s => (s, s.computeSignal()))
    val result = signalsCombinations(inputs)
      .map(combination => {
        combination.foreach(t => t._1.sendSignal(t._2))
        (combination, outputs.map(bulb => (bulb, bulb.computeSignal())))
      })
    originalValues.foreach(t => t._1.sendSignal(t._2))
    result
  }

  private def formulaWire(wire: Option[Wire], inner: Boolean = false): String = wire match {
    case Some(w: Wire) => w.from match {
      case Some(d: Device) => formula(d, inner = inner)
      case _ => "0"
    }
    case _ => "0"
  }

  private def concatBinaryOp(device: Device, delimeter: String, inner: Boolean = false): String =
    tryWrapParens(formulaWire(device.incoming.head, inner = true) + " " + delimeter + " " + formulaWire(device.incoming(1), inner = true), inner)

  private def tryWrapParens(s: String, wrapParens: Boolean) = if (wrapParens) "(" + s + ")" else s

  def formula(device: Device, inner: Boolean = false): String = device match {
    case _: ZeroConst => "0"
    case _: OneConst => "1"
    case switch: Switch => switch.name
    case _: Bulb | _: Repeater => formulaWire(device.incoming.head, inner = inner)
    case and: And => concatBinaryOp(and, "&&", inner = inner)
    case or: Or => concatBinaryOp(or, "||", inner = inner)
    case xor: Xor => concatBinaryOp(xor, "⊕", inner = inner)
    case not: Not => "!" + formulaWire(not.incoming.head, inner = true)
    case _: Split => formulaWire(device.incoming.head)
    case _ => throw new IllegalArgumentException
  }
}
