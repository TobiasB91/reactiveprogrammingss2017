sealed trait Color

object Color {
  sealed trait Player extends Color
  sealed trait Meteor extends Color
  sealed trait Laser extends Color
  sealed trait Enemy extends Color
  sealed trait Metal extends Color

  sealed trait RGBY extends Color
  type Pill = RGBY
  type Wing = RGBY
  type Cockpit = RGBY
  type UFO = RGBY
  type Powerup = RGBY
  type Button = RGBY

  case object Black extends Enemy
  case object Blue extends Player with Laser with Enemy with RGBY
  case object Green extends Player with Laser with Enemy with RGBY
  case object Orange extends Player
  case object Red extends Player with Laser with Enemy with RGBY
  case object Yellow extends RGBY
  case object Grey extends Meteor
  case object Brown extends Meteor
  case object Bronze extends Metal
  case object Gold extends Metal
  case object Silver extends Metal

  object Metal {
    val Bronze = Color.Bronze
    val Gold = Color.Gold
    val Silver = Color.Silver
  }

  object Player {
    val Blue = Color.Blue
    val Green = Color.Green
    val Orange = Color.Orange
    val Red = Color.Red
  }

  object Enemy {
    val Black = Color.Black
    val Blue = Color.Black
    val Green = Color.Green
    val Red = Color.Red
  }

  object Meteor {
    val Grey = Color.Grey
    val Brown = Color.Brown
  }

  object RGBY {
    val Blue = Color.Blue
    val Green = Color.Green
    val Red = Color.Red
    val Yellow = Color.Yellow
  }

  val Pill = RGBY
  val Wing = RGBY
  val UFO = RGBY
  val Powerup = RGBY
  val Cockpit = RGBY
  val Button = RGBY

  object Laser {
    val Blue = Color.Blue
    val Green = Color.Green
    val Red = Color.Red
  }
}

sealed trait Size
object Size {
  sealed trait Meteor extends Size
  sealed trait TurretBase extends Size

  case object Big extends Meteor with TurretBase
  case object Medium extends Meteor
  case object Small extends Meteor with TurretBase
  case object Tiny extends Meteor
}

sealed trait PowerupType
object PowerupType {
  case object Plain extends PowerupType
  case object Bolt extends PowerupType
  case object Shield extends PowerupType
  case object Star extends PowerupType
}