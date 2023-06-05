package dimensional.additionalunits

import dimensional.dimension.Dimensions.{*, given}
import dimensional.typelevelint.*

import scala.language.implicitConversions

inline def nano[
  L <: IntT, T <: IntT
](x: Dim[L, T]): Dim[
  L, T
] = x * 1e-9

inline def micro[
  L <: IntT, T <: IntT
](x: Dim[L, T]): Dim[
  L, T
] = x * 1e-6

inline def milli[
  L <: IntT, T <: IntT
](x: Dim[L, T]): Dim[
  L, T
] = x * 1e-3

inline def centi[
  L <: IntT, T <: IntT
](x: Dim[L, T]): Dim[
  L, T
] = x * 1e-2

inline def deci[
  L <: IntT, T <: IntT
](x: Dim[L, T]): Dim[
  L, T
] = x * 1e-1

inline def kilo[
  L <: IntT, T <: IntT
](x: Dim[L, T]): Dim[
  L, T
] = x * 1e3

inline def mega[
  L <: IntT, T <: IntT
](x: Dim[L, T]): Dim[
  L, T
] = x * 1e6

inline def giga[
  L <: IntT, T <: IntT
](x: Dim[L, T]): Dim[
  L, T
] = x * 1e9

inline def tera[
  L <: IntT, T <: IntT
](x: Dim[L, T]): Dim[
  L, T
] = x * 1e12

inline def peta[
  L <: IntT, T <: IntT
](x: Dim[L, T]): Dim[
  L, T
] = x * 1e15

inline def kibi[
  L <: IntT, T <: IntT
](x: Dim[L, T]): Dim[
  L, T
] = x * 1024

inline def mebi[
  L <: IntT, T <: IntT
](x: Dim[L, T]): Dim[
  L, T
] = x * math.pow(1024, 2)

inline def gibi[
  L <: IntT, T <: IntT
](x: Dim[L, T]): Dim[
  L, T
] = x * math.pow(1024, 3)

inline def tebi[
  L <: IntT, T <: IntT
](x: Dim[L, T]): Dim[
  L, T
] = x * math.pow(1024, 4)

inline def pebi[
  L <: IntT, T <: IntT
](x: Dim[L, T]): Dim[
  L, T
] = x * math.pow(1024, 5)

