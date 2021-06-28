#' Synthesizes a tide curve
#'
#' @description Synthesizes a tide curve; model built with BuildTC().
#' @param tmodel The model you built with BuildTC(). Please see examples.
#' @param ssdate Synthesis start date. This indicates the date you want your tide curve to start with.
#' @param sstime Synthesis start time. The starting time for your tide table.
#' @param sedate Synthesis end date.
#' @param setime Synthesis end time.
#' @param solar_syn Compute a solar synthesis? Default is TRUE.
#' @references \url{https://www.bsh.de/DE/PUBLIKATIONEN/_Anlagen/Downloads/Meer_und_Umwelt/Berichte-des-BSH/Berichte-des-BSH_50_de.pdf?__blob=publicationFile&v=13/}
#' @references \doi{https://doi.org/10.5194/os-15-1363-2019}
#' @return Returns a list with two elements, which are of class data.table and data.frame.
#' \item{synthesis.lunar}{The lunar synthesis data as a data.table object in UTC.}
#' \item{tide.curve}{The solar tide curve as a data.table or NULL object (time zone of the observations).}
#' @export
#'
#' @examples
#' \dontrun{SynTC(tmodel = your_model, ssdate = "2015/12/17", sstime = "00:00:00",
#' sedate = "2015/12/31", setime = "23:30:00")}
#'
SynTC <- function(tmodel = NULL, ssdate, sstime, sedate, setime, solar_syn = TRUE){

  stopifnot(class(tmodel) == "tidecurve")

  #Retrieving objects from tmodel

  fitting.coef  <- tmodel[["lm.coeff"]]
  tdiff.analyse <- tmodel[["tdiff.analyse"]]
  km            <- tmodel[["km"]]
  mindt         <- tmodel[["mindt"]]
  otz.24        <- tmodel[["otz.24"]]
  tplus         <- tmodel[["tplus"]]
  tm24          <- tmodel[["tm24"]]

  tmmh         <- tm24 / km
  tdtobs       <- mindt / 1440
  tdtobs.2     <- tdtobs / 2
  tdtobs.105   <- tdtobs + 10^-5

  chron.origin <- chron(dates. = "1900/01/01",
                        times. = "00:00:00",
                        format = c(dates = "y/m/d", times = "h:m:s"),
                        out.format = c(dates = "y/m/d", times = "h:m:s"))

  #Synthesis Period
  ssdate.time <- chron(dates. = ssdate,
                       times. = sstime,
                       format = c(dates = "y/m/d", times = "h:m:s"),
                       out.format = c(dates = "y/m/d", times = "h:m:s")) - chron.origin - otz.24
  sedate.time <- chron(dates. = sedate,
                       times. = setime,
                       format = c(dates = "y/m/d", times = "h:m:s"),
                       out.format = c(dates = "y/m/d", times = "h:m:s")) - chron.origin - otz.24
  #Transit indices
  nummsa  <- as.numeric(floor((ssdate.time - tplus) / tm24))
  nummse  <- as.numeric(floor((sedate.time - tplus) / tm24))

  xdesign.matrix <- BuildDesign(tdiffa = tdiff.analyse, numma = nummsa, numme = nummse)
  matrix.cols     <- ncol(xdesign.matrix) - 1L
  xdesign.matrix        <- data.table(xdesign.matrix, key = "numm")
  m.length              <- (nummse - nummsa + 1) * km
  time1                 <- numeric(length = m.length)
  height                <- numeric(length = m.length)
  tobstime              <- numeric(length = m.length)
  afunc                 <- vector()
  coeff                 <- numeric(length = km)
  time.height           <- matrix(1.0, nrow = (m.length), ncol = 4)
  colnames(time.height) <- c("height", "i", "k", "time1")


  #Lunar synthesis
  numm <- NULL
  mm <- as.matrix(xdesign.matrix[numm >= nummsa][numm <= nummse])
  m  <- 0L
  n  <- 0L

  for (ii in nummsa : nummse) {
    n     <- n + 1L
    afunc <- mm[n, 2:(matrix.cols + 1)]
    for (k in 1 : km) {
      m             <- m + 1L
      coeff         <- fitting.coef[[k]]
      summe         <- coeff %*% afunc
      time1[m]      <- ii * tm24 + tplus + (k - 0.5) * tmmh
      height[m]     <- summe
      tobstime[m]   <- round(time1[m] / tdtobs) * tdtobs

      time.height[m, ] <- c(height[m], ii, k, time1[m])
    }
  }

  prediction_date <- NULL
  prediction_time <- NULL
  date_time       <- NULL

  time.height <- data.table(time.height)
  time.height[,date_time := format(chron(dates. = (round(time1 * 86400, digits = 0) / 86400) + 1 / 864000,
                                         origin. = c(month = 1, day = 1, year = 1900)),
                                   format = "%Y/%m/%d %H:%M:%S")]
  setcolorder(time.height, c("date_time", "time1", "height", "i", "k"))
  time.height[, c("prediction_date", "prediction_time") := tstrsplit(date_time, split = " ")]

  if(isTRUE(solar_syn)) {

  #Solar synthesis
  l           <- 1L
  xa          <- numeric(length = 7)
  ya          <- numeric(length = 7)
  tsyntstd    <- numeric(length = 1.04 * (m - 4 - 3 + 1))
  ty          <- numeric(length = 1.04 * (m - 4 - 3 + 1))
  ssdate.time <- as.numeric(ssdate.time)
  sedate.time <- as.numeric(sedate.time)

  for(j in 4 : (m - 3)){
    tsyn  <- tobstime[j]
    tsynp <- tsyn + tdtobs.2
    tsynm <- tsyn - tdtobs.2
    if((tsynp < ssdate.time) || (tsynm > sedate.time)) next

    insp <- 0
    for(is in (j - 3) : (j + 3)){
      insp     <- insp + 1
      xa[insp] <- time1[is]
      ya[insp] <- height[is]
    }

    tdtt <- tobstime[j] - tobstime [j - 1]

    if(tdtt > (tdtobs.105)){

      tsynn       <- tobstime[j] - tdtobs
      ty[l]       <- splint(xa, ya, tsynn)
      tsyntstd[l] <- tsynn + otz.24
      l           <- l + 1L
    }
    ty[l]       <- splint(xa, ya, tsyn)
    tsyntstd[l] <- tsyn + otz.24
    l           <- l + 1L
  }

  tidal.curve     <- data.table(date_time = format(chron(dates. = (tsyntstd + 1 / 864000),
                                                         origin. = c(month = 1, day = 1, year = 1900)),
                                                   format = "%Y/%m/%d %H:%M:00"),
                                time1  = as.numeric(tsyntstd) + 1 / 864000,
                                height = ty)
  tidal.curve[, c("prediction_date", "prediction_time") := tstrsplit(date_time, split = " ")]
  tidal.curve <- tidal.curve[(prediction_date != "1900/01/01" & height != 0)]

  } else {
    tidal.curve <- NULL
  }

  syn <- list("synthesis.lunar" = time.height,
              "tide.curve"      = tidal.curve)

  return(syn)

}
