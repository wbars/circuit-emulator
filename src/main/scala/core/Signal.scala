package core

sealed trait Signal {}

case object ZERO extends Signal
case object ONE extends Signal
