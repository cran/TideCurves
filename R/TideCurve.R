#' Computes tide curves
#'
#' @description Takes a data frame as input with three columns (see example dataset) and returns a tide curve.
#' Internally the analysis is carried out in lunar days.
#' One mean lunar day lasts 1.0350501 mean solar days.
#' Therefore the analysis time period
#' should start one lunar day after the first observation and end one lunar day before the last observation.
#' @param dataInput A data frame with the columns observation_date, observation_time and height. See attached data for correct formats.
#' @param otz The time zone of the observations
#' @param km The number of nodes between two consecutive mean moon transits. Shall be less or equal to: round(1440 [min] / time step [min])
#' Example: Time step 5 min: Use km = 288 or even smaller. Leave on default (km = -1) and supply mindt, when unsure.
#' @param mindt Observation time step in [min]. Default is 30.
#' @param asdate A string indication the date you want the analysis to start with. Format: "yyyy/mm/dd".
#' @param astime A string indicating the time you want the analysis to start with. Format: "hh:mm:ss"
#' @param aedate A string indication the date you want the analysis to end with. Format: "yyyy/mm/dd".
#' @param aetime A string indicating the time you want the analysis to end with. Format: "hh:mm:ss"
#' @param ssdate Synthesis start date. This indicates the date you want your tide curve to start with. Format: See above
#' @param sstime Synthesis start time. The starting time for your tide table. Format: See above
#' @param sedate Synthesis end date. Format: See above
#' @param setime Synthesis end time. Format: See above
#' @return Returns a list with elements of the analysis, fitting and the tidal curve for given data
#' \item{synthesis.lunar}{The lunar synthesis data as a data.table object in UTC}
#' \item{data.matrix}{The data needed for analysis}
#' \item{tide.curve}{The solar tide curve as a data.table object (provided time zone)}
#' \item{lm.coeff}{Coefficients for the km fitted linear models used in the synthesis as a list of 1-row matrices}
#' \item{diff.analyse}{Time in days spanning the analysis}
#' @references  Godin, Gabriel (1972) The Analysis of Tides. Toronto, 264pp
#' @references \doi{10.5194/os-15-1363-2019}
#' @references \url{https://www.bsh.de/DE/PUBLIKATIONEN/_Anlagen/Downloads/Meer_und_Umwelt/Berichte-des-BSH/Berichte-des-BSH_50_de.pdf?__blob=publicationFile&v=13}
#' @examples
#' \dontrun{TideCurve(dataInput = tideObservation, asdate = "2015/12/06",
#'              astime = "00:00:00",      aedate = "2015/12/31",
#'              aetime = "23:30:00",      ssdate = "2015/12/17",
#'              sstime = "00:00:00",      sedate = "2015/12/31",
#'              setime = "23:30:00")}
#'
#' @import data.table
#' @import chron
#' @importFrom stats coef
#' @importFrom stats lm.fit
#' @importFrom stats sd
#' @importFrom fields splint
#' @export
#'
TideCurve <- function(dataInput, otz = 1, km = -1, mindt = 30, asdate, astime, aedate, aetime, ssdate, sstime, sedate, setime) {

  if(km == -1){
    km <- round(1440 / mindt)
  }

  height  <- dataInput[["height"]]
  nspline <- 7
  #options(chron.origin = c(month = 1, day = 1, year = 1900))
  chron.origin <- chron(dates. = "1900/01/01",
                        times. = "00:00:00",
                        format = c(dates = "y/m/d", times = "h:m:s"),
                        out.format = c(dates = "y/m/d", times = "h:m:s"))

  tperiode.m2  <- 360 / 28.9841042373
  tmean.moon   <- tperiode.m2 * 2
  tm24         <- tmean.moon / 24
  tmoon.0      <- as.numeric(chron(dates. = "1949/12/31",
                                   times. = "21:08:00",
                                   format = c(dates = "y/m/d", times = "h:m:s"),
                                   out.format = c(dates = "y/m/d", times = "h:m:s")) - chron.origin)
  tmdm         <- 24.2325 / 1440
  tplus        <- tmoon.0 + tmdm
  tmmh         <- tm24 / km
  tdtobs       <- mindt / 1440
  tdtobsn      <- tdtobs * (nspline - 1)
  tdtobsn.105  <- tdtobsn + 10^-5
  tdtobs.105   <- tdtobs + 10^-5
  otz.24       <- otz / 24
  tmondkm      <- numeric(length = km)

  for (i in 1 : km) {
    tmondkm[i] <- tmmh * (i - 0.5)
  }

  chron.beob      <- chron(dates. = dataInput[["observation_date"]],
                           times. = dataInput[["observation_time"]],
                           format = c(dates = "y/m/d", times = "h:m:s"),
                           out.format = c(dates = "y/m/d", times = "h:m:s"))

  diff.days       <- as.numeric((chron.beob - chron.origin) - otz.24)
  length.diffdays <- length(diff.days)
  moona           <- as.numeric(floor((diff.days[1] - tplus) / tm24))
  moone           <- as.numeric(floor((diff.days[length.diffdays] - tplus) / tm24))
  tmmt.numm       <- numeric(length = length(moona:moone))

  for(i in moona : moone){
    tmmt.numm[i] <- i * tm24 + tplus
  }
  #Analysis
  asdate.time <- chron(dates. = asdate,
                       times. = astime,
                       format = c(dates = "y/m/d", times = "h:m:s"),
                       out.format = c(dates = "y/m/d", times = "h:m:s")) - chron.origin - (otz.24)

  aedate.time <- chron(dates. = aedate,
                       times. = aetime,
                       format = c(dates = "y/m/d", times = "h:m:s"),
                       out.format = c(dates = "y/m/d", times = "h:m:s")) - chron.origin - (otz.24)

  numma   <- as.numeric(floor((asdate.time - tplus) / tm24))
  numme   <- as.numeric(floor((aedate.time - tplus) / tm24))

  tdiff.analyse    <- numme - numma + 1

  #Synthesis
  ssdate.time <- chron(dates. = ssdate,
                       times. = sstime,
                       format = c(dates = "y/m/d", times = "h:m:s"),
                       out.format = c(dates = "y/m/d", times = "h:m:s")) - chron.origin - otz.24
  sedate.time <- chron(dates. = sedate,
                       times. = setime,
                       format = c(dates = "y/m/d", times = "h:m:s"),
                       out.format = c(dates = "y/m/d", times = "h:m:s")) - chron.origin - otz.24

  nummsa  <- as.numeric(floor((ssdate.time - tplus) / tm24))
  nummse  <- as.numeric(floor((sedate.time - tplus) / tm24))

  #Computing Funcs for all cases
  min_numm <- min(c(numma, nummsa))
  max_numm <- max(c(numme, nummse))
  matrix.cols      <- length(Funcs(tdiff = tdiff.analyse, xi = max_numm)[[3]])

  xdesign.matrix      <- matrix(0.0, nrow=(max_numm - min_numm + 1), ncol = matrix.cols + 1)
  xdesign.matrix[, 1] <- seq.int(min_numm, max_numm, 1)

  for(i in 1 : nrow(xdesign.matrix)){
    xdesign.matrix[i, 2: (matrix.cols + 1)] <- Funcs(xi = xdesign.matrix[i, 1], tdiff = tdiff.analyse)[[3]]
  }


  xa                    <- numeric(length = 7) #time vector
  ya                    <- numeric(length = 7) #height vector
  ty                    <- numeric() #height interpolated by splint
  data_matrix           <- matrix(0.0, nrow = length.diffdays, ncol = 4) #the result matrix
  colnames(data_matrix) <- c("numm", "imm", "tmmttmond", "height")
  numm                  <- NULL
  floored               <- floor((diff.days - tplus) / tm24)
  tdtobs.2              <- tdtobs / 2
  imm                   <- numeric()
  tmmttmond             <- numeric()
  tmd                   <- numeric()
  dx                    <- numeric()
  ld.3                  <- length.diffdays - 3



  for (ii in 4 : ld.3) {
    ik <- floored[ii]

    if((ik < numma) || (ik > numme)) next

    imm       <- floor((diff.days[ii] - tmmt.numm[ik]) / tmmh) + 1
    tmmttmond <- tmmt.numm[ik] + tmondkm [imm]
    tmd       <- diff.days[ii] - tmmttmond

    if (abs(tmd) > tdtobs.2) next

    insp <- 0
    for (j in (ii - 3) : (ii + 3)){
      insp     <- insp + 1
      xa[insp] <- diff.days[j]
      ya[insp] <- height[j]
    }
    dx       <- xa[insp] - xa[1]

    if(dx > tdtobsn.105) next

    ty                 <- splint(xa, ya, tmmttmond)

    data_matrix[ii, ]  <- c(ik, imm, tmmttmond, ty)
  }

  #Prepare joins on numm for xdesign and design.frame
  colnames(xdesign.matrix) <- c("numm", paste0("V","", seq(1 : matrix.cols)))
  xdesign.matrix           <- data.table(xdesign.matrix, key = "numm")
  data_matrix              <- data.table(data_matrix, key = "numm")

  design.frame     <- data_matrix[(numm >= numma) & (numm <= numme)]
  design.frame     <- xdesign.matrix[design.frame]
  setkey(design.frame, "imm")

  fitting.coef <- design.frame[,{
    m.h   <- mean(height)
    sd.h  <- 3 * sd(height)
    m_mat <- as.matrix(.SD[(height >= m.h - sd.h) & (height <= m.h + sd.h)])
    as.list(coef(lm.fit(x = m_mat[, !colnames(m_mat) %in% c("height", "numm", "imm", "tmmttmond")],
                        y = m_mat[, "height"])))
  }
  , by = "imm"]

  for(j in seq_len(ncol(fitting.coef))) set(fitting.coef,which(is.na(fitting.coef[[j]])),j,0)

  fitting.coef <- lapply(split(fitting.coef, by = "imm", keep.by = FALSE), as.matrix)

  m.length              <- (nummse - nummsa + 1) * km
  time1                 <- numeric(length = m.length)
  height                <- numeric(length = m.length)
  tobstime              <- numeric(length = m.length)
  afunc                 <- vector()
  coeff                 <- numeric(length = km)
  time.height           <- matrix(1.0, nrow = (m.length), ncol = 4)
  colnames(time.height) <- c("height", "i", "k", "time1")


  #Lunar synthesis
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
  time.height[, date_time := format(chron(dates. = (round(time1 * 86400, digits = 0) / 86400) + 1 / 864000,
                                          origin. = c(month = 1, day = 1, year = 1900)),
                                   "%Y/%m/%d %H:%M:%S")]
  setcolorder(time.height, c("date_time", "time1", "height", "i", "k"))
  time.height[, c("prediction_date", "prediction_time") := tstrsplit(date_time, split = " ")]

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

  #Add date/time columns to solar synthesis

  tidal.curve     <- data.table(date_time = format(chron(dates. = (tsyntstd + 1 / 864000),
                                                         origin. = c(month = 1, day = 1, year = 1900)),
                                                   "%Y/%m/%d %H:%M:00"),
                                time1  = as.numeric(tsyntstd) + 1 / 864000,
                                height = ty)
  tidal.curve[, c("prediction_date", "prediction_time") := tstrsplit(date_time, split = " ")]
  tidal.curve <- tidal.curve[(prediction_date != "1900/01/01" & height != 0)]

  #we return a list containing the tide curve (lunar and solar), diff.analyse, lm.coeff and data matrix
  report                 <- list("data_matrix"     = data_matrix[(numm >= numma) & (numm <= numme)],
                                 "synthesis.lunar" = time.height,
                                 "tide.curve"      = tidal.curve,
                                 "lm.coeff"        = fitting.coef,
                                 "diff.analyse"    = tdiff.analyse)
  return(report)
}


