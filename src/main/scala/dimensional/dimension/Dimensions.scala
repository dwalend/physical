package dimensional.dimension

import dimensional.typelevelint
import dimensional.typelevelint.{*, given}

import scala.annotation.targetName

/**
 * Where the main definitions are, in particular Dim for physical quantities.
 *
 * Note: trying to break up the file leads to obscure errors from the compiler.
 */
object Dimensions:
  /**
   * Type that keeps track of the dimension of a quantity.
   *
   * @tparam Length exponent for the length component of the dim
   * @tparam Time exponent for the time component of the dim
   * @tparam Temperature exponent for the temperature component of the dim
   */
  opaque type Dim[
    Length <: IntT,            // l
    Time <: IntT,              // t
    Temperature <: IntT,       // p
  ] = Double

  /**
   * Maps the given function over the components of the given dim.
   * @tparam F the given type function
   * @tparam D the given dim
   */
  type DimMap[F[_ <: IntT] <: IntT, D] = D match
    case Dim[l, t, p] => Dim[
      F[l], F[t], F[p]
    ]

  /**
   * Maps the given binary function over the components of the two given dims.
   *
   * @tparam F the given binary type function
   * @tparam D1 the first given dim
   * @tparam D2 the second given dim
   */
  type DimMap2[Op[_ <: IntT, _ <: IntT] <: IntT, D1, D2] = D1 match
    case Dim[l1, t1, p1] => D2 match
      case Dim[l2, t2, p2] =>
        Dim[
          Op[l1, l2], Op[t1, t2], Op[p1, p2]
        ]

  /**
   * Multiplies the two given dims.
   */
  @targetName("times") type *[D1, D2] = DimMap2[Sum, D1, D2]

  /**
   * Divides the two given dims.
   */
  @targetName("over") type /[D1, D2] = DimMap2[Diff, D1, D2]

  /**
   * Raises the given dim to the given power.
   * @tparam D the given dim
   * @tparam P the given power
   */
  @targetName("toThe") type ~[D, P <: IntT] = DimMap[[Q <: IntT] =>> Prod[Q, P], D]

  /**
   * Returns the Nth root of the given dim, assuming it's a valid operation.
   * @tparam D the given dim, whose exponents should be divisible by N
   * @tparam N the root to take
   */
  type Root[D, N <: NonZeroIntT] = DimMap[[Z <: IntT] =>> IntQuotient[Z, N], D]

  /**
   * Helper for abstract dimension setters.
   */
  type SetterHelper[D, R, M <: IntT] = DimMap2[[I <: IntT, J <: IntT] =>> Sum[I, Prod[M, J]], D, R]

  /**
   * Trivial dimension to represent dimensionless quantities
   */
  type Uno = Dim[_0, _0, _0]

  // Base dimensions
  type Length            = Dim[_1, _0, _0]
  type Time              = Dim[_0, _1, _0]
  type Temperature       = Dim[_0, _0, _1]

  // Derived dimensions
  type Acceleration = Velocity / Time
  type Area = Length ~ _2
  type Diffusivity = Area / Time
  type Frequency = Uno / Time
  type Velocity = Length / Time
  type Volume = Length ~ _3

  // Standard units (SI units for SI dimensions)
  val hertz    : Frequency          = 1
  val kelvin   : Temperature        = 1
  val metre    : Length             = 1
  val second   : Time               = 1

  /**
   * Dimensionless quantities can be auto-converted to Doubles.
   */
  given Conversion[Double, Uno] with
    inline def apply(d: Double): Uno = d

  /**
   * Int can be auto-converted to Uno.
   */
  given Conversion[Int, Uno] with
    inline def apply(d: Int): Uno = d.toDouble

  /**
   * Double can be auto-converted to Uno.
   */
  given Conversion[Uno, Double] with
    inline def apply(d: Uno): Double = d

  /**
   * Absolute value
   */
  inline def abs[
    L <: IntT, T <: IntT, P <: IntT
  ](
    x: Dim[L, T, P]
  ): Dim[L, T, P] = math.abs(x)

  /**
   * Floor
   */
  inline def floor[
    L <: IntT, T <: IntT, P <: IntT
  ](
    x: Dim[L, T, P]
  ): Dim[L, T, P] = math.floor(x)

  /**
   * Ceiling
   */
  inline def ceilNotClean[
    L <: IntT, T <: IntT, P <: IntT
  ](
    x: Dim[L, T, P]
  ): Dim[L, T, P] = math.ceil(x)

  /**
   * Functions that apply to any quantity, regardless of its dimension.
   */
  extension[
    L <: IntT, T <: IntT, P <: IntT
  ] (x: Dim[L, T, P])

    /**
     * String representation of this quantity, using base dimensions and standard units
     */
    def asString(using L, T, P): String =
      x.toString + " " + dimensionsAsString(
        summon[L], summon[T], summon[P]
      )

    /**
     * String representation of this quantity, using the given unit, as well as base dimensions and standard units
     */
    def asStringWith[
      L2 <: IntT, T2 <: IntT, P2 <: IntT
    ](unit: Dim[L2, T2, P2], unitString: String)(using
      l: L, t: T, p: P,
      l2: L2, t2: T2, p2: P2
    ): String =
      val remainingUnits = dimensionsAsString(
        diff(l, l2), diff(t, t2), diff(p, p2)
      )
      (x / unit).toString + " " + Seq(unitString, remainingUnits).filter(_.nonEmpty).mkString("·")

    /**
     * @return the magnitude of this quantity in the given unit
     */
    inline def in(unit: Dim[L, T, P]): Double = x / unit

    /**
     * Usual smaller-than comparison; only defined if the two quantities to be compared have the same dimension
     */
    @targetName("smallerThan") inline def <(
      y: Dim[L, T, P]
    ): Boolean =
      assert(!(x.isNaN || y.isNaN))
      x < y

    /**
     * Usual larger-than comparison; only defined if the two quantities to be compared have the same dimension
     */
    @targetName("largerThan") inline def >(
      y: Dim[L, T, P]
    ): Boolean =
      assert(!(x.isNaN || y.isNaN))
      x > y

    /**
     * Usual smaller-or-equal comparison; only defined if the two quantities to be compared have the same dimension
     */
    @targetName("smallerOrEqual") inline def <=(
      y: Dim[L, T, P]
    ): Boolean =
      assert(!(x.isNaN || y.isNaN))
      x <= y

    /**
     * Usual larger-or-equal comparison; only defined if the two quantities to be compared have the same dimension
     */
    @targetName("largerOrEqual") inline def >=(
      y: Dim[L, T, P]
    ): Boolean =
      assert(!(x.isNaN || y.isNaN))
      x >= y

    /**
     * Usual equality; only defined if the two quantities to be compared have the same dimension
     */
    @targetName("equal") inline def =:=(
      y: Dim[L, T, P]
    ): Boolean =
      assert(!(x.isNaN || y.isNaN))
      x == y

    /**
     * Usual addition; only defined if the two quantities to be added have the same dimension
     */
    @targetName("plus") inline def +(
      y: Dim[L, T, P]
    ): Dim[L, T, P] = x + y

    /**
     * Usual subtraction; only defined if the two quantities to be subtracted have the same dimension
     */
    @targetName("minus") inline def -(
      y: Dim[L, T, P]
    ): Dim[L, T, P] = x - y

    /**
     * Negation
     */
    inline def unary_- : Dim[L, T, P] = -x

    /**
     * Usual multiplication; dimensions are also multiplied
     */
    @targetName("times") inline def *[
      Ly <: IntT, Ty <: IntT, Py <: IntT
    ](
      y: Dim[Ly, Ty, Py]
     ): Dim[L, T, P] *
        Dim[Ly, Ty, Py] = x * y

    /**
     * Usual % operator (Behaves like the Scala % operator on Doubles.)
     */
    @targetName("modulo") inline def %[
      Ly <: IntT, Ty <: IntT, Py <: IntT
    ](
      y: Dim[Ly, Ty, Py]
     ): Dim[L, T, P] = x % y

    /**
     * Usual division; dimensions are also divided
     */
    @targetName("over") inline def /[
      Ly <: IntT, Ty <: IntT, Py <: IntT
    ](
       y: Dim[Ly, Ty, Py]
     ): Dim[L, T, P] /
      Dim[Ly, Ty, Py] = x / y

    /**
     * Usual exponentiation; dimensions are also exponentiated
     */
    @targetName("toThe") inline def ~[E <: IntT](
      y: E
    ): Dim[L, T, P] ~ E = power(x, y)

    /**
     * @return the nth root of this quantity
     */
    inline def root[E <: NonZeroIntT](n: E)(using
      Divides[E, L], Divides[E, T], Divides[E, P]
    ): Root[Dim[L, T, P], E] = typelevelint.root(x, n)
end Dimensions
