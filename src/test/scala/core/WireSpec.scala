package core

import org.scalatest._
import org.scalatest.mockito.MockitoSugar

class WireSpec extends FlatSpec with Matchers with MockitoSugar {
  "A wire" should "have default signal ZERO" in {
    val wire = Wire.empty()
    wire.signal should be(ZERO)
  }

  it should "persist signal that was sent" in {
    val wire = Wire.empty()
    wire.sendSignal(ONE)
    wire.signal should be(ONE)
    wire.signal should be(ONE)

    wire.sendSignal(ZERO)
    wire.signal should be(ZERO)
  }
}
