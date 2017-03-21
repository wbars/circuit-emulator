package core

sealed abstract class Signal {
  def unary_! : Signal = this match {
    case ZERO => ONE
    case ONE => ZERO
  }
}

case object ZERO extends Signal

case object ONE extends Signal
