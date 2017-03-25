package core

import org.scalatest.{FlatSpec, Matchers}

class DeviceAnalyserSpec extends FlatSpec with Matchers {
  "Device analyzer" should "be able to enumerate all input signals combinations" in {
    val switch1 = Device.switch()
    val switch2 = Device.switch()
    val switch3 = Device.switch()

    DeviceAnalyser.signalsCombinations(Seq(switch1, switch2, switch3)) should be(Seq(
      Seq((switch1, ZERO), (switch2, ZERO), (switch3, ZERO)),
      Seq((switch1, ZERO), (switch2, ZERO), (switch3, ONE)),
      Seq((switch1, ZERO), (switch2, ONE), (switch3, ZERO)),
      Seq((switch1, ZERO), (switch2, ONE), (switch3, ONE)),
      Seq((switch1, ONE), (switch2, ZERO), (switch3, ZERO)),
      Seq((switch1, ONE), (switch2, ZERO), (switch3, ONE)),
      Seq((switch1, ONE), (switch2, ONE), (switch3, ZERO)),
      Seq((switch1, ONE), (switch2, ONE), (switch3, ONE))
    ))
  }

  it should "produce truth table for all input signals combinations for all outputs (AND device)" in {
    val switch1 = Device.switch()
    val switch2 = Device.switch()
    val and = Device.and()
    val bulb = Device.bulb()

    Wire.create(switch1, and)
    Wire.create(switch2, and, toPos = 1)
    Wire.create(and, bulb)

    DeviceAnalyser.analyse(Seq(switch1, switch2), Seq(bulb)) should be(Seq(
      (Seq((switch1, ZERO), (switch2, ZERO)), Seq((bulb, ZERO))),
      (Seq((switch1, ZERO), (switch2, ONE)), Seq((bulb, ZERO))),
      (Seq((switch1, ONE), (switch2, ZERO)), Seq((bulb, ZERO))),
      (Seq((switch1, ONE), (switch2, ONE)), Seq((bulb, ONE)))
    ))
  }

  it should "produce truth table for all input signals combinations for all outputs (OR device)" in {
    val switch1 = Device.switch()
    val switch2 = Device.switch()
    val or = Device.or()
    val bulb = Device.bulb()

    Wire.create(switch1, or)
    Wire.create(switch2, or, toPos = 1)
    Wire.create(or, bulb)

    DeviceAnalyser.analyse(Seq(switch1, switch2), Seq(bulb)) should be(Seq(
      (Seq((switch1, ZERO), (switch2, ZERO)), Seq((bulb, ZERO))),
      (Seq((switch1, ZERO), (switch2, ONE)), Seq((bulb, ONE))),
      (Seq((switch1, ONE), (switch2, ZERO)), Seq((bulb, ONE))),
      (Seq((switch1, ONE), (switch2, ONE)), Seq((bulb, ONE)))
    ))
  }

  it should "keep original switches values after analysing" in {
    val switch1 = Device.switch()
    val switch2 = Device.switch()
    val switch3 = Device.switch()
    switch2.toggle()

    val or = Device.or()
    val bulb = Device.bulb()

    Wire.create(switch1, or)
    Wire.create(switch2, or, toPos = 1)
    Wire.create(or, bulb)

    DeviceAnalyser.analyse(Seq(switch1, switch2, switch3), Seq(bulb))
    switch1.computeSignal() should be(ZERO)
    switch2.computeSignal() should be(ONE)
    switch3.computeSignal() should be(ZERO)
  }

  it should "be able to print logic formulas for scheme outputs (trivial cases)" in {
    val oneConst = Device.oneConst()
    val zeroConst = Device.zeroConst()
    val switch = Device.switch()
    switch.setName("ab")

    val bulb = Device.bulb()
    DeviceAnalyser.formula(bulb) should be("0")

    val wire = Wire.create(oneConst, bulb)
    DeviceAnalyser.formula(bulb) should be("1")
    wire.remove()

    val wire1 = Wire.create(zeroConst, bulb)
    DeviceAnalyser.formula(bulb) should be("0")
    wire1.remove()

    Wire.create(switch, bulb)
    DeviceAnalyser.formula(bulb) should be("ab")
  }

  it should "be able to print logic formulas for scheme outputs" in {
    val a = Device.switch("a")
    val b = Device.switch("b")
    val and = Device.and()
    val or = Device.or()
    val not = Device.not()
    val xor = Device.xor()
    val split = Device.split()
    val repeater = Device.repeater()
    val oneConst = Device.oneConst()
    val zeroConst = Device.zeroConst()

    val bulb1 = Device.bulb()
    val bulb2 = Device.bulb()

    Wire.create(a, split)
    Wire.create(split, or)
    Wire.create(oneConst, or, toPos = 1)
    Wire.create(or, repeater)
    Wire.create(repeater, not)
    Wire.create(not, bulb1)

    Wire.create(b, xor)
    Wire.create(zeroConst, xor, toPos = 1)
    Wire.create(xor, and)
    Wire.create(split, and, fromPos = 1, toPos = 1)
    Wire.create(and, bulb2)

    DeviceAnalyser.formula(bulb1) should be("!(a || 1)")
    DeviceAnalyser.formula(bulb2) should be("(b ⊕ 0) && a")
  }

  it should "keep parens value for split and repeater devices" in {
    val a = Device.switch("a")
    val b = Device.switch("b")
    val c = Device.switch("c")
    val or = Device.or()
    val and = Device.and()
    val bulb = Device.bulb()
    Wire.create(a, or)
    Wire.create(b, or, toPos = 1)
    val orAndWire = Wire.create(or, and)
    Wire.create(c, and, toPos = 1)
    Wire.create(and, bulb)

    val correctFormula = "(a || b) && c"
    DeviceAnalyser.formula(bulb) should be(correctFormula)

    val split = Device.split()
    orAndWire.remove()
    val orSplitWire = Wire.create(or, split)
    val splitAndWire = Wire.create(split, and)
    DeviceAnalyser.formula(bulb) should be(correctFormula)

    val repeater = Device.repeater()
    orSplitWire.remove()
    splitAndWire.remove()
    Wire.create(or, repeater)
    Wire.create(repeater, and)
    DeviceAnalyser.formula(bulb) should be(correctFormula)
  }
}
