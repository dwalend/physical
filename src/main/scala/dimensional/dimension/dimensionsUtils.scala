package dimensional.dimension

import dimensional.typelevelint.*

private inline val tau = 2 * math.Pi

private inline def fractionalPart(x: Double): Double = x - math.floor(x)

/**
 * @return the given dimension exponents as a string
 */
def dimensionsAsString[
  L2 <: IntT, T2 <: IntT, P2 <: IntT
](
   l: L2, t: T2, p: P2
 ): String =
  Seq(
     unitString(l, "m"), unitString(t, "s"), unitString(p, "K"), 
  ).filter((i, s) => i != 0).sortBy((i, s) => -i).map((i, s) => s).mkString("·")

/**
 * @param e the exponent to which to raise the unit
 * @param s the unit symbol
 * @return a string for the given unit symbol raised to the given power, preceded by the exponent, as an Int
 */
private def unitString(e: IntT, s: String): (Int, String) =
  val exponent = intTAsInt(e)
  (exponent, s + exponentString(exponent))

/**
 * @return a string representation of the given exponent
 */
private def exponentString(e: Int): String =
  if e == 1 then "" else if e < 0 then "⁻" + intAsSuperscript(-e) else intAsSuperscript(e)

/**
 * @return a string of the given non-negative int as a superscript
 */
private def intAsSuperscript(i: Int): String =
  assert(i >= 0)
  if i == 0 then "" else intAsSuperscript(i / 10) + digitAsSuperscript(i % 10)

/**
 * @return a string of the given digit as a superscript
 */
private def digitAsSuperscript(i: Int): String = i match
  case 0 => "⁰"
  case 1 => "¹"
  case 2 => "²"
  case 3 => "³"
  case 4 => "⁴"
  case 5 => "⁵"
  case 6 => "⁶"
  case 7 => "⁷"
  case 8 => "⁸"
  case 9 => "⁹"
  case _ => assert(false)
