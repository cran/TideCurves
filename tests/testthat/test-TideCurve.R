library(TideCurves)

tc <- TideCurve(dataInput = tideObservation, asdate = "2014/06/01",
          astime = "00:00:00",      aedate = "2015/12/31",
          aetime = "23:30:00",      ssdate = "2015/01/01",
          sstime = "00:00:00",      sedate = "2015/12/31",
          setime = "23:30:00")

tc_model <- BuildTC(dataInput = tideObservation,
                    asdate = "2014/06/01", aedate = "2015/12/31",
                    astime = "00:00:00", aetime = "23:30:00")
tc_m <- SynTC(tmodel = tc_model,  ssdate = "2015/01/01", sstime = "00:00:00" ,
              sedate = "2015/12/31", setime = "23:30:00")

test_that("equal lunar synthesis", {
  local_edition(2)
  expect_equal(tc$synthesis.lunar, tc_m$synthesis.lunar)
})

test_that("equal solar synthesis", {
  local_edition(2)
  expect_equal(tc$tide.curve, tc_m$tide.curve)
})