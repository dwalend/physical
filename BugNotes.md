Moving to Scala 3.3.0 - fails to compile after 12 seconds. First error is

```
[error] -- [E007] Type Mismatch Error: /Users/dwalend/projects/physical/src/main/scala/dimensional/additionalunits/additionalUnits.scala:20:45 
[error] 20 |val electronVolt: Energy = 1.602176634e-19 * joule
[error]    |                                             ^^^^^
[error]    |                            Found:    (y$proxy6 :
[error]    |                              (dimensional.dimension.Dimensions.joule :
[error]    |                                dimensional.dimension.Dimensions.Energy)
[error]    |                             & $proxy12.Energy)
[error]    |                            Required: Double
```



