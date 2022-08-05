package dimensional

import dimensional.AdditionalUnits.*
import dimensional.Dimensions.{*, given}
import dimensional.IntType.*

import scala.language.implicitConversions

object Example:
  @main def main(): Unit =
    val v1: Velocity      = 0.1 * metre / milli(second)
    val v2: Length / Time = 15 * metre / second
    println((v1 + v2).in(kilo(metre) / hour))

    val waterDensity = 0.997 * kilogram / litre
    val waterViscosity = 1 * centiPoise

    val lightYear: Length = 9460730472580800.0 * metre

    println(lightYear.in(giga(kilo(metre))))

    _9: NatSum[_2, _7]
    _1: NatRemainder[_9, _4]
    summon[NatDivides[_2, _2]]
    summon[NatDivides[_3, _9]]

//    val incorrect1 = v1 + lightYear
//    val incorrect2: Time = 1 * metre
end Example