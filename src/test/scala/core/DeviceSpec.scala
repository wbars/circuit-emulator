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


  it should "allow attaching only one wire to each input wire" in {
    val and = Device.and()
    and.inputWireFree(0) should be(true)
    and.inputWireFree(1) should be(true)
    and.inWire(Wire.empty())
    and.inputWireFree(0) should be(false)
    and.inputWireFree(1) should be(true)
    assertThrows[IllegalArgumentException] {
      and.inWire(Wire.empty())
    }
  }

  it should "allow attaching only one wire to each output wire" in {
    val splitter = Device.split()
    splitter.outputWireFree(0) should be(true)
    splitter.outputWireFree(1) should be(true)
    splitter.outWire(Wire.empty())
    splitter.outputWireFree(0) should be(false)
    splitter.outputWireFree(1) should be(true)
    assertThrows[IllegalArgumentException] {
      splitter.outWire(Wire.empty())
    }
  }

  it should "not allow connecting device to itself" in {
    val and = Device.and()
    val wire = Wire.empty()
    and.inWire(wire)
    assertThrows[IllegalArgumentException] {
      and.outWire(wire)
    }

    val and1 = Device.and()
    val wire1 = Wire.empty()
    and1.outWire(wire1)
    assertThrows[IllegalArgumentException] {
      and1.inWire(wire1, 1)
    }

    val and2 = Device.and()
    assertThrows[IllegalArgumentException] {
      Wire.create(and2, and2)
    }
  }

  it should "not allow connecting devices into cycle" in {
    val repeater1 = Device.repeater()
    val repeater2 = Device.repeater()
    val repeater3 = Device.repeater()
    val wrongRepeater = Device.repeater()
    Wire.create(from = repeater1, to = repeater2)
    Wire.create(from = repeater2, to = repeater3)
    Wire.create(from = repeater3, to = wrongRepeater)

    Wire.hasCycle(Some(wrongRepeater), Some(repeater1)) should be(true)
    assertThrows[IllegalArgumentException] {
      Wire.create(from = wrongRepeater, to = repeater1)
    }

    val wrongWire = Wire.empty()
    val anotherWrongRepeater = Device.repeater()

    Wire.hasCycle(Some(anotherWrongRepeater), Some(anotherWrongRepeater)) should be(true)
    anotherWrongRepeater.outWire(wrongWire)
    assertThrows[IllegalArgumentException] {
      anotherWrongRepeater.inWire(wrongWire)
    }
  }

  it should "allow repeating devices that does no produce loop" in {
    val one = Device.oneConst()
    val splitter = Device.split()
    val and = Device.and()

    Wire.create(one, splitter)
    Wire.create(splitter, and)
    Wire.create(splitter, and, 1, 1)

    val out = Wire.empty()
    and.outWire(out)

    out.signal should be(ONE)
  }

  it should "produce default signal (all input zeroes) when input wires count < inputSize" in {
    Seq(Device.repeater(), Device.and(), Device.or(), Device.xor(), Device.split(), Device.switch()).foreach(testDefaultSignal)

    val not = Device.not()
    val out = Wire.empty()
    not.outWire(out)
    out.signal should be(ONE)
  }

  it should "produce canAttachInput/canAttachOutput only if attached wires count is less than size" in {
    Seq(Device.repeater(), Device.and(), Device.or(), Device.xor(), Device.split(), Device.not(), Device.switch()).foreach(testCanAttach)
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

  "ONE constant" should "be producer of ONE signal for one output" in {
    val oneConst = Device.oneConst()
    oneConst.inputSize() should be(0)
    oneConst.outputSize() should be(1)

    val outWire = Wire.empty()
    oneConst.outWire(outWire)
    outWire.signal should be(ONE)
  }

  "ZERO constant" should "be producer of ZERO signal for one output" in {
    val oneConst = Device.zeroConst()
    oneConst.inputSize() should be(0)
    oneConst.outputSize() should be(1)

    val outWire = Wire.empty()
    oneConst.outWire(outWire)
    outWire.signal should be(ZERO)
  }

  "Toggle switch" should "change value by toggling" in {
    val switch = Device.switch()
    val outWire = Wire.empty()
    switch.outWire(outWire)

    outWire.signal should be(ZERO)
    switch.toggle()
    outWire.signal should be(ONE)
    switch.toggle()
    outWire.signal should be(ZERO)
  }

  "Bulb" should "be simple consumer" in {
    val bulb = Device.bulb()
    val one = Device.oneConst()
    bulb.active should be(false)

    val wire = Wire.create(one, bulb)
    bulb.active should be(true)
    wire.remove()

    val zero = Device.zeroConst()
    val wire1 = Wire.create(zero, bulb)
    bulb.active should be(false)
    wire1.remove()

    Wire.create(one, bulb)
    bulb.active should be(true)
  }


  def checkRow(inWires: Seq[Wire], inSignals: Seq[Signal], outWires: Seq[Wire], outSignals: Seq[Signal]): Unit = {
    inWires.indices.foreach(i => inWires(i).sendSignal(inSignals(i)))
    outWires.indices.foreach(i => outWires(i).signal should be(outSignals(i)))
  }

  def testDefaultSignal(device: Device) {
    val outWires: Seq[Wire] = Seq.fill(device.outputSize())(Wire.empty())
    outWires.indices.foreach(i => device.outWire(outWires(i), i))
    all(outWires.map(_.signal)) should be(ZERO)
  }

  def testCanAttach(device: Device) {
    if (device.incoming.nonEmpty) {
      device.incoming.indices.foreach(i => {
        device.canAttachInputWire should be(true)
        device.inWire(Wire.empty(), i)
      })
      device.canAttachInputWire should be(false)
      device.releaseInWire(0)
      device.canAttachInputWire should be(true)
    }

    if (device.outcoming.nonEmpty) {
      device.outcoming.indices.foreach(i => {
        device.canAttachOutputWire should be(true)
        device.outWire(Wire.empty(), i)
      })
      device.canAttachOutputWire should be(false)
      device.releaseOutWire(0)
      device.canAttachOutputWire should be(true)
    }
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
