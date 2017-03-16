package core

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.ListBuffer

class DeviceSpec extends FlatSpec with Matchers {
  "A Device" should "allow to attach/remove wires to its pins" in {
    val wire = Wire.empty()
    val fromRepeater = Device.repeater()
    val toRepeater = Device.repeater()

    fromRepeater.outWire(wire)
    toRepeater.inWire(wire)

    fromRepeater.outcoming should be(ListBuffer(Some(wire)))
    fromRepeater.incoming should be(ListBuffer(None))

    toRepeater.outcoming should be(ListBuffer(None))
    toRepeater.incoming should be(ListBuffer(Some(wire)))

    wire.from should be(Some(fromRepeater))
    wire.to should be(Some(toRepeater))
  }

  it should "update outcoming wires signals when incoming are attached/removed" in {
    val and = Device.and()
    and.incoming should be(ListBuffer(None, None))
    and.outcoming should be(ListBuffer(None))

    val outWire = Wire.empty()
    val inputWire1 = Wire.empty()
    val inputWire2 = Wire.empty()

    and.outWire(outWire)
    and.outcoming should be(ListBuffer(Some(outWire)))

    and.inWire(inputWire1)
    and.incoming should be(ListBuffer(Some(inputWire1), None))

    and.inWire(inputWire2, 1)
    and.incoming should be(ListBuffer(Some(inputWire1), Some(inputWire2)))

    and.releaseInWire(0)
    and.incoming should be(ListBuffer(None, Some(inputWire2)))
  }

  it should "update outcoming wires signals when incoming wires signals are changed" in {
    val inWire = Wire.empty()
    val outWire = Wire.empty()
    val not = Device.not()

    not.inWire(inWire)
    not.outWire(outWire)

    outWire.signal should be(ONE)

    inWire.sendSignal(ONE)
    outWire.signal should be(ZERO)

    inWire.sendSignal(ZERO)
    outWire.signal should be(ONE)
  }

  "Match table" should "be true for NOT device" in {
    checkOneToOneDevice(Seq(
      (ZERO, ONE),
      (ONE, ZERO)
    ), Device.not())
  }

  it should "be true for AND device" in {
    checkTwoToOneDevice(
      Seq(
        ((ZERO, ZERO), ZERO),
        ((ONE, ZERO), ZERO),
        ((ZERO, ONE), ZERO),
        ((ONE, ONE), ONE)
      ), Device.and())
  }

  it should "be true for OR device" in {
    checkTwoToOneDevice(
      Seq(
        ((ZERO, ZERO), ZERO),
        ((ONE, ZERO), ONE),
        ((ZERO, ONE), ONE),
        ((ONE, ONE), ONE)
      ), Device.or())
  }

  it should "be true for XOR" in {
    checkTwoToOneDevice(
      Seq(
        ((ZERO, ZERO), ZERO),
        ((ONE, ZERO), ONE),
        ((ZERO, ONE), ONE),
        ((ONE, ONE), ZERO)
      ), Device.xor())
  }

  it should "be true for Split" in {
    val truthTable = Seq(
      (ZERO, (ZERO, ZERO)),
      (ONE, (ONE, ONE))
    )

    val inWire = Wire.empty()
    val outWire1 = Wire.empty()
    val outWire2 = Wire.empty()
    val split: Device = Device.split()

    split.inWire(inWire)
    split.outWire(outWire1)
    split.outWire(outWire2, 1)

    truthTable.foreach({ case (input: Signal, (output1: Signal, output2: Signal)) =>
      checkRow(Seq(inWire), Seq(input), Seq(outWire1, outWire2), Seq(output1, output2))
    })
  }

  it should "be true for Repeater" in {
    checkOneToOneDevice(Seq(
      (ZERO, ZERO),
      (ONE, ONE)
    ), Device.repeater())
  }

  it should "be true for Half adder" in {
    val truthTable = Seq(
      ((ZERO, ZERO), (ZERO, ZERO)),
      ((ONE, ZERO), (ONE, ZERO)),
      ((ZERO, ONE), (ONE, ZERO)),
      ((ONE, ONE), (ZERO, ONE))
    )

    val ((x, y), (s, c)) = halfAdder()

    truthTable.foreach({ case ((input1: Signal, input2: Signal), (output1: Signal, output2: Signal)) =>
      checkRow(Seq(x, y), Seq(input1, input2), Seq(s, c), Seq(output1, output2))
    })
  }


  def checkRow(inWires: Seq[Wire], inSignals: Seq[Signal], outWires: Seq[Wire], outSignals: Seq[Signal]): Unit = {
    inWires.indices.foreach(i => inWires(i).sendSignal(inSignals(i)))
    outWires.indices.foreach(i => outWires(i).signal should be(outSignals(i)))
  }

  it should "be true for full adder" in {
    val ((a, b), (ha1, ha2)) = halfAdder()
    val ((_, cin), (s, ha22)) = halfAdder(ha1)
    val and = Device.or()
    and.inWire(ha22)
    and.inWire(ha2, 1)
    val cout = Wire.empty()
    and.outWire(cout)

    val truthTable = Seq(
      ((ZERO, ZERO, ZERO), (ZERO, ZERO)),
      ((ZERO, ZERO, ONE), (ONE, ZERO)),
      ((ZERO, ONE, ZERO), (ONE, ZERO)),
      ((ZERO, ONE, ONE), (ZERO, ONE)),
      ((ONE, ZERO, ZERO), (ONE, ZERO)),
      ((ONE, ZERO, ONE), (ZERO, ONE)),
      ((ONE, ONE, ZERO), (ZERO, ONE)),
      ((ONE, ONE, ONE), (ONE, ONE))
    )
    truthTable.foreach({ case ((input1: Signal, input2: Signal, input3: Signal), (output1: Signal, output2: Signal)) =>
      checkRow(Seq(a, b, cin), Seq(input1, input2, input3), Seq(s, cout), Seq(output1, output2))
    })
  }

  def testDefaultSignal(device: Device) {
    val outWires: Seq[Wire] = Seq.fill(device.outputSize())(Wire.empty())
    outWires.indices.foreach(i => device.outWire(outWires(i), i))
    all(outWires.map(_.signal)) should be(ZERO)
  }

  "All Devices" should "produce default signal (all input zeroes) when input wires count < inputSize" in {
    Seq(Device.repeater(), Device.and(), Device.or(), Device.xor(), Device.split()).foreach(testDefaultSignal)

    val not = Device.not()
    val out = Wire.empty()
    not.outWire(out)
    out.signal should be(ONE)
  }

  def testCanAttach(device: Device) {
    device.incoming.indices.foreach(i => {
      device.canAttachInputWire should be(true)
      device.inWire(Wire.empty(), i)
    })
    device.canAttachInputWire should be(false)
    device.releaseInWire(0)
    device.canAttachInputWire should be(true)

    device.outcoming.indices.foreach(i => {
      device.canAttachOutputWire should be(true)
      device.outWire(Wire.empty(), i)
    })
    device.canAttachOutputWire should be(false)
    device.releaseOutWire(0)
    device.canAttachOutputWire should be(true)
  }

  it should "produce canAttachInput/canAttachOutput only if attached wires count is less than size" in {
    Seq(Device.repeater(), Device.and(), Device.or(), Device.xor(), Device.split(), Device.not()).foreach(testCanAttach)
  }

  private def halfAdder(x: Wire = Wire.empty(),
                        y: Wire = Wire.empty()): ((Wire, Wire), (Wire, Wire)) = {
    val s: Wire = Wire.empty()
    val c: Wire = Wire.empty()
    val splitX1 = Wire.empty()
    val splitX2 = Wire.empty()
    val splitX = Device.split()
    splitX.inWire(x)
    splitX.outWire(splitX1)
    splitX.outWire(splitX2, 1)

    val splitY1 = Wire.empty()
    val splitY2 = Wire.empty()
    val splitY = Device.split()
    splitY.inWire(y)
    splitY.outWire(splitY1)
    splitY.outWire(splitY2, 1)

    val xors = Device.xor()
    xors.inWire(splitX1)
    xors.inWire(splitY1, 1)
    xors.outWire(s)

    val andc = Device.and()
    andc.inWire(splitX2)
    andc.inWire(splitY2, 1)
    andc.outWire(c)

    ((x, y), (s, c))
  }

  private def checkTwoToOneDevice(truthTable: Seq[((Signal, Signal), Signal)], device: Device) = {
    val inWire1 = Wire.empty()
    val inWire2 = Wire.empty()
    val outWire = Wire.empty()
    device.inWire(inWire1)
    device.inWire(inWire2, 1)
    device.outWire(outWire)

    truthTable.foreach({ case ((input1: Signal, input2: Signal), output: Signal) =>
      checkRow(Seq(inWire1, inWire2), Seq(input1, input2), Seq(outWire), Seq(output))
    })
  }

  private def checkOneToOneDevice(truthTable: Seq[(Signal, Signal)], device: Device) = {
    val inWire = Wire.empty()
    val outWire = Wire.empty()

    device.inWire(inWire)
    device.outWire(outWire)

    truthTable.foreach({ case (input: Signal, output: Signal) =>
      checkRow(Seq(inWire), Seq(input), Seq(outWire), Seq(output))
    })
  }
}
