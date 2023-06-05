package dimensional.additionalunits

import dimensional.dimension.Dimensions.{*, given}
import dimensional.typelevelint.*

import scala.language.implicitConversions

val minute    : Time = 60 * second
val hour      : Time = 60 * minute
val day       : Time = 24 * hour
val julianYear: Time = 365.25 * day

val kPH        : Velocity = kilo(metre) / hour
val planckSpeed: Velocity = 299792458 * metre / second

val angstrom    : Length = 1e-10 * metre
val meter       : Length = metre
val inch        : Length = 0.0254 * metre
val foot        : Length = 12 * inch
val yard        : Length = 3 * foot
val furlong     : Length = 220 * yard
val mile        : Length = 8 * furlong
val nauticalMile: Length = 1852 * metre
val lightYear   : Length = planckSpeed * julianYear

val knot: Velocity = nauticalMile / hour

val litre      : Volume = 1e-3 * metre ~ _3
val liter      : Volume = litre
val fluidOunce : Volume = 29.5735295625 * milli(litre)
val liquidPint : Volume = 16 * fluidOunce
val liquidQuart: Volume = 2 * liquidPint
val usGallon   : Volume = 4 * liquidQuart
