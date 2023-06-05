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
   * @tparam Mass exponent for the mass component of the dim
   * @tparam ElectricCharge exponent for the electric charge component of the dim
   * @tparam SubstanceAmount exponent for the substance amount component of the dim
   * @tparam Cost exponent for the cost component of the dim
   * @tparam Angle exponent for the angle component of the dim
   */
  opaque type Dim[
    Length <: IntT,            // l
    Time <: IntT,              // t
    Temperature <: IntT,       // p
    Mass <: IntT,              // m
    ElectricCharge <: IntT,    // q
    SubstanceAmount <: IntT,   // n
    Cost <: IntT,              // c
    Angle <: IntT,             // a
  ] = Double

  /**
   * Maps the given function over the components of the given dim.
   * @tparam F the given type function
   * @tparam D the given dim
   */
  type DimMap[F[_ <: IntT] <: IntT, D] = D match
    case Dim[l, t, p, m, q, n, c, a] => Dim[
      F[l], F[t], F[p], F[m], F[q], F[n], F[c], F[a]
    ]

  /**
   * Maps the given binary function over the components of the two given dims.
   *
   * @tparam F the given binary type function
   * @tparam D1 the first given dim
   * @tparam D2 the second given dim
   */
  type DimMap2[Op[_ <: IntT, _ <: IntT] <: IntT, D1, D2] = D1 match
    case Dim[l1, t1, p1, m1, q1, n1, c1, a1] => D2 match
      case Dim[l2, t2, p2, m2, q2, n2, c2, a2] =>
        Dim[
          Op[l1, l2], Op[t1, t2], Op[p1, p2], Op[m1, m2], Op[q1, q2], Op[n1, n2], Op[c1, c2], Op[a1, a2]
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
  type Uno = Dim[_0, _0, _0, _0, _0, _0, _0, _0]

  // Base dimensions
  type Length            = Dim[_1, _0, _0, _0, _0, _0, _0, _0]
  type Time              = Dim[_0, _1, _0, _0, _0, _0, _0, _0]
  type Temperature       = Dim[_0, _0, _1, _0, _0, _0, _0, _0]
  type Mass              = Dim[_0, _0, _0, _1, _0, _0, _0, _0]
  type ElectricCharge    = Dim[_0, _0, _0, _0, _1, _0, _0, _0]
  type SubstanceAmount   = Dim[_0, _0, _0, _0, _0, _1, _0, _0]
  type Cost              = Dim[_0, _0, _0, _0, _0, _0, _1, _0]
  type Angle             = Dim[_0, _0, _0, _0, _0, _0, _0, _1]

  // Derived dimensions
  type Acceleration = Velocity / Time
  type Area = Length ~ _2
  type Density = Mass / Volume
  type Diffusivity = Area / Time
  type Energy = Force * Length
  type Force = Mass * Acceleration
  type Frequency = Uno / Time
  type Momentum = Mass * Velocity
  type Power = Energy / Time
  type Pressure = Force / Area
  type Velocity = Length / Time
  type Viscosity = Pressure * Time
  type Volume = Length ~ _3

  // Standard units (SI units for SI dimensions)
  val dollar   : Cost               = 1
  val hertz    : Frequency          = 1
  val joule    : Energy             = 1
  val kelvin   : Temperature        = 1
  val kilogram : Mass               = 1
  val metre    : Length             = 1
  val mole     : SubstanceAmount    = 1
  val newton   : Force              = 1
  val pascal   : Pressure           = 1
  val radian   : Angle              = 1
  val second   : Time               = 1
  val watt     : Power              = 1

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

  // Trigonometric functions
  inline def sin(a: Angle): Uno = math.sin(a)
  inline def cos(a: Angle): Uno = math.cos(a)
  inline def tan(a: Angle): Uno = math.tan(a)
  inline def sec(a: Angle): Uno = 1 / math.cos(a)
  inline def csc(a: Angle): Uno = 1 / math.sin(a)
  inline def cot(a: Angle): Uno = 1 / math.tan(a)

  extension (a: Angle)
    /**
     * Normalizes this angle two be between 0 and 2 Pi.
     */
    inline def normalized: Angle = (tau * fractionalPart(a / tau))

  // Inverse trigonometric functions
  inline def asin(x: Uno): Angle = math.asin(x)
  inline def acos(x: Uno): Angle = math.acos(x)
  inline def atan(x: Uno): Angle = math.atan(x)
  inline def asec(x: Uno): Angle = math.acos(1 / x)
  inline def acsc(x: Uno): Angle = math.asin(1 / x)
  inline def acot(x: Uno): Angle = math.atan(1 / x)

  /**
   * Absolute value
   */
  inline def abs[
    L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, N <: IntT, C <: IntT, A <: IntT
  ](
    x: Dim[L, T, P, M, Q, N, C, A]
  ): Dim[L, T, P, M, Q, N, C, A] = math.abs(x)

  /**
   * Floor
   */
  inline def floor[
    L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, N <: IntT, C <: IntT, A <: IntT
  ](
    x: Dim[L, T, P, M, Q, N, C, A]
  ): Dim[L, T, P, M, Q, N, C, A] = math.floor(x)

  /**
   * Ceiling
   */
  inline def ceil[
    L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, N <: IntT, C <: IntT, A <: IntT
  ](
    x: Dim[L, T, P, M, Q, N, C, A]
  ): Dim[L, T, P, M, Q, N, C, A] = math.ceil(x)

  /**
   * Functions that apply to any quantity, regardless of its dimension.
   */
  extension[
    L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, N <: IntT, C <: IntT, A <: IntT
  ] (x: Dim[L, T, P, M, Q, N, C, A])

    /**
     * String representation of this quantity, using base dimensions and standard units
     */
    def asString(using L, T, P, M, Q, N, C, A): String =
      x.toString + " " + dimensionsAsString(
        summon[L], summon[T], summon[P], summon[M], summon[Q], summon[N], summon[C], summon[A]
      )

    /**
     * String representation of this quantity, using the given unit, as well as base dimensions and standard units
     */
    def asStringWith[
      L2 <: IntT, T2 <: IntT, P2 <: IntT, M2 <: IntT, Q2 <: IntT, N2 <: IntT, C2 <: IntT, A2 <: IntT
    ](unit: Dim[L2, T2, P2, M2, Q2, N2, C2, A2], unitString: String)(using
      l: L, t: T, p: P, m: M, q: Q, n: N, c: C, a: A,
      l2: L2, t2: T2, p2: P2, m2: M2, q2: Q2, n2: N2, c2: C2, a2: A2
    ): String =
      val remainingUnits = dimensionsAsString(
        diff(l, l2), diff(t, t2), diff(p, p2), diff(m, m2), diff(q, q2), diff(n, n2), diff(c, c2), diff(a, a2)
      )
      (x / unit).toString + " " + Seq(unitString, remainingUnits).filter(_.nonEmpty).mkString("·")

    /**
     * @return the magnitude of this quantity in the given unit
     */
    inline def in(unit: Dim[L, T, P, M, Q, N, C, A]): Double = x / unit

    /**
     * Usual smaller-than comparison; only defined if the two quantities to be compared have the same dimension
     */
    @targetName("smallerThan") inline def <(
      y: Dim[L, T, P, M, Q, N, C, A]
    ): Boolean =
      assert(!(x.isNaN || y.isNaN))
      x < y

    /**
     * Usual larger-than comparison; only defined if the two quantities to be compared have the same dimension
     */
    @targetName("largerThan") inline def >(
      y: Dim[L, T, P, M, Q, N, C, A]
    ): Boolean =
      assert(!(x.isNaN || y.isNaN))
      x > y

    /**
     * Usual smaller-or-equal comparison; only defined if the two quantities to be compared have the same dimension
     */
    @targetName("smallerOrEqual") inline def <=(
      y: Dim[L, T, P, M, Q, N, C, A]
    ): Boolean =
      assert(!(x.isNaN || y.isNaN))
      x <= y

    /**
     * Usual larger-or-equal comparison; only defined if the two quantities to be compared have the same dimension
     */
    @targetName("largerOrEqual") inline def >=(
      y: Dim[L, T, P, M, Q, N, C, A]
    ): Boolean =
      assert(!(x.isNaN || y.isNaN))
      x >= y

    /**
     * Usual equality; only defined if the two quantities to be compared have the same dimension
     */
    @targetName("equal") inline def =:=(
      y: Dim[L, T, P, M, Q, N, C, A]
    ): Boolean =
      assert(!(x.isNaN || y.isNaN))
      x == y

    /**
     * Usual addition; only defined if the two quantities to be added have the same dimension
     */
    @targetName("plus") inline def +(
      y: Dim[L, T, P, M, Q, N, C, A]
    ): Dim[L, T, P, M, Q, N, C, A] = x + y

    /**
     * Usual subtraction; only defined if the two quantities to be subtracted have the same dimension
     */
    @targetName("minus") inline def -(
      y: Dim[L, T, P, M, Q, N, C, A]
    ): Dim[L, T, P, M, Q, N, C, A] = x - y

    /**
     * Negation
     */
    inline def unary_- : Dim[L, T, P, M, Q, N, C, A] = -x

    /**
     * Usual multiplication; dimensions are also multiplied
     */
    @targetName("times") inline def *[
      Ly <: IntT, Ty <: IntT, Py <: IntT, My <: IntT, Qy <: IntT, Ny <: IntT, Cy <: IntT, Ay <: IntT
    ](
      y: Dim[Ly, Ty, Py, My, Qy, Ny, Cy, Ay]
     ): Dim[L, T, P, M, Q, N, C, A] *
        Dim[Ly, Ty, Py, My, Qy, Ny, Cy, Ay] = x * y

    /**
     * Usual % operator (Behaves like the Scala % operator on Doubles.)
     */
    @targetName("modulo") inline def %[
      Ly <: IntT, Ty <: IntT, Py <: IntT, My <: IntT, Qy <: IntT, Ny <: IntT, Cy <: IntT, Ay <: IntT
    ](
      y: Dim[Ly, Ty, Py, My, Qy, Ny, Cy, Ay]
     ): Dim[L, T, P, M, Q, N, C, A] = x % y

    /**
     * Usual division; dimensions are also divided
     */
    @targetName("over") inline def /[
      Ly <: IntT, Ty <: IntT, Py <: IntT, My <: IntT, Qy <: IntT, Ny <: IntT, Cy <: IntT, Ay <: IntT
    ](
       y: Dim[Ly, Ty, Py, My, Qy, Ny, Cy, Ay]
     ): Dim[L, T, P, M, Q, N, C, A] /
      Dim[Ly, Ty, Py, My, Qy, Ny, Cy, Ay] = x / y

    /**
     * Usual exponentiation; dimensions are also exponentiated
     */
    @targetName("toThe") inline def ~[E <: IntT](
      y: E
    ): Dim[L, T, P, M, Q, N, C, A] ~ E = power(x, y)

    /**
     * @return the nth root of this quantity
     */
    inline def root[E <: NonZeroIntT](n: E)(using
      Divides[E, L], Divides[E, T], Divides[E, P], Divides[E, M], Divides[E, Q], Divides[E, N], Divides[E, C],
      Divides[E, A]
    ): Root[Dim[L, T, P, M, Q, N, C, A], E] = typelevelint.root(x, n)
end Dimensions
