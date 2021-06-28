#' Computes the residuum between the observed data and the synthesis
#' @description This function computes the residuum of the computed lunar and solar synthesis
#' and the observed data
#' @param tcData The results from TideCurve or BuildTC + SynTC. Warning: The synthesis period must overlap with the analysis period.
#' Must be a data.table object. Please see examples.
#' @param obsData The observation data with the columns observation_date, observation_time and height. See attached data for correct formats.
#' @return A list with two data.tables with the joined data input and the computed difference between the observed data and
#' the synthesis (res)
#' @export
#'
#' @examples
#' \dontrun{
#' #Using TideCurve
#' tc <- TideCurve(dataInput = tideObservation, asdate = "2015/12/06",
#'              astime = "00:00:00",      aedate = "2015/12/31",
#'              aetime = "23:30:00",      ssdate = "2015/12/17",
#'              sstime = "00:00:00",      sedate = "2015/12/31",
#'              setime = "23:30:00")
#' res_tc <- ResCurve(tc, tideObservation)
#'
#' #Using BuildTC and SynTC
#' your_model <- BuildTC(dataInput = tideObservation, asdate = "2015/12/06",
#'              astime = "00:00:00", aedate = "2016/12/31",
#'              aetime = "23:30:00", keep_data = TRUE)
#'
#' syn_tc <- SynTC(tmodel = your_model, ssdate = "2015/12/17", sstime = "00:00:00",
#' sedate = "2015/12/31", setime = "23:30:00")
#'
#' syn_tc$data_matrix <- your_model$data_matrix
#'
#' res_tc_n <- ResCurve(syn_tc, tideObservation)
#'
#' }

ResCurve<- function(tcData, obsData){


  numm_imm  <- NULL
  i         <- NULL
  k         <- NULL
  numm      <- NULL
  imm       <- NULL
  res       <- NULL
  height    <- NULL
  dmheight  <- NULL
  lsheight  <- NULL
  tsheight  <- NULL
  obheight  <- NULL
  date_time <- NULL
  observation_date <- NULL
  observation_time <- NULL
  prediction_date  <- NULL
  prediction_time  <- NULL
  time1 <- NULL


  syl <- data.table()
  dam <- data.table()
  tic <- data.table()
  obs <- data.table()

  #Lunar residuum
  syl <- copy(tcData$synthesis.lunar)
  dam <- copy(tcData$data_matrix)
  syl[, numm_imm := paste(i, k, sep="_")]
  dam[, numm_imm := paste(numm, imm, sep="_")]
  setnames(syl, "height", "lsheight")
  setnames(dam, "height", "dmheight")

  setkey(syl, numm_imm)
  setkey(dam, numm_imm)

  lTable <- dam[syl][order(time1)]
  lTable[, res := dmheight - lsheight]

  #Solar residuum
  tic  <- copy(tcData$tide.curve)

  obs  <- as.data.table(obsData)
  obs[, date_time := paste(observation_date, observation_time, sep = " ")]
  obs <- obs[chmatch(tic$date_time, date_time)]

  setnames(tic, "height", "tsheight")
  setnames(obs, "height", "obheight")


  setkey(tic, date_time)
  setkey(obs, date_time)

  sTable <- obs[tic]
  sTable[, res := obheight - tsheight]

  resList <-list()

  resList$lunar.res <- lTable
  resList$solar.res <- sTable

  return(resList)

}

