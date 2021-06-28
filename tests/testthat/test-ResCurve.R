library(TideCurves)

tc <- TideCurve(dataInput = tideObservation, asdate = "2015/06/01",
                astime = "00:00:00",      aedate = "2015/12/31",
                aetime = "23:30:00",      ssdate = "2015/01/01",
                sstime = "00:00:00",      sedate = "2015/12/31",
                setime = "23:30:00")

res_tc <- ResCurve(tc, tideObservation)

tc_model <- BuildTC(dataInput = tideObservation,
                    asdate = "2015/06/01", aedate = "2015/12/31",
                    astime = "00:00:00", aetime = "23:30:00", keep_data = TRUE)
syn_tc <- SynTC(tmodel = tc_model,  ssdate = "2015/01/01", sstime = "00:00:00" ,
              sedate = "2015/12/31", setime = "23:30:00")

syn_tc$data_matrix <- tc_model$data_matrix

res_tc_n <- ResCurve(syn_tc, tideObservation)

test_that("equal lunar res", {
  local_edition(2)
  expect_equal(res_tc$lunar.res, res_tc_n$lunar.res)
})

test_that("equal solar res", {
  local_edition(2)
  expect_equal(res_tc$solar.res, res_tc$solar.res)
})