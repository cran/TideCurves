<!-- README.md is generated from README.Rmd. Please edit that file -->
Why should i use this package?
------------------------------

You should use this package for producing tide curves from past data.

How do i use it?
----------------

Import your data set first and transform it to a readable form. See attached data 'observation' for an example data frame.

``` r
library(TideCurves)
tideObservation[1:10, ]
#>    observation_date observation_time height
#> 1        2015/06/01         00:00:00  6.570
#> 2        2015/06/01         00:30:00  6.528
#> 3        2015/06/01         01:00:00  6.410
#> 4        2015/06/01         01:30:00  6.200
#> 5        2015/06/01         02:00:00  5.893
#> 6        2015/06/01         02:30:00  5.556
#> 7        2015/06/01         03:00:00  5.248
#> 8        2015/06/01         03:30:00  4.970
#> 9        2015/06/01         04:00:00  4.745
#> 10       2015/06/01         04:30:00  4.550

sapply(tideObservation, typeof)
#> observation_date observation_time           height 
#>      "character"      "character"         "double"
```

You can now use your data as input for the function 'TideCurve'. Setting the periods for analyzing and synthesizing and wait for the table to be produced. Afterwards you can compute the differences between the observed data and lunar and solar tide curves

``` r
mytidecurve<-TideCurve(dataInput = tideObservation, asdate = "2015/12/06",
             astime = "00:00:00",      aedate = "2015/12/31",
             aetime = "23:30:00",      ssdate = "2015/12/17",
             sstime = "00:00:00",      sedate = "2015/12/31",
             setime = "23:30:00", mindt = 30)

mydifferences <- ResCurve(tcData = mytidecurve, obsData = tideObservation)
```
