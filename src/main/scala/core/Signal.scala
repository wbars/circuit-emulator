package core

sealed abstract class Signal {
  def display(): String = this match {
    case ZERO => "ZERO"
    case ONE => "ONE"
    case _ => throw new IllegalArgumentException
  }

  def unary_! : Signal = this match {
    case ZERO => ONE
    case ONE => ZERO
  }
}

case object ZERO extends Signal

case object ONE extends Signal
