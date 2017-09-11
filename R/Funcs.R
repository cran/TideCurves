#' Returns predictor vector for design matrix
#' @description Returns predictor vector for design matrix from 44 astronomical angular velocities.
#' @param xi Transit index
#' @param tdiff Length of input time series.
#' @return A list with the selected angular velocities, their ranks and the predictor vector (Values between -1, 1).

Funcs <-  function(tdiff, xi) {

  #Vector with angular velocities
  rad <- 0.017453292519943
  xi  <- rad * xi

  omegas <- vector()
  omegas[ 1]<-0.054809904
  omegas[ 2]<-0.115308512
  omegas[ 3]<-0.904885870
  omegas[ 4]<-1.020194382
  omegas[ 5]<-1.809771741
  omegas[ 6]<-2.040388764
  omegas[ 7]<-11.597841752
  omegas[ 8]<-11.713150263
  omegas[ 9]<-13.468112100
  omegas[10]<-13.522922004
  omegas[11]<-13.583420612
  omegas[12]<-13.638230516
  omegas[13]<-13.693040419
  omegas[14]<-15.563310768
  omegas[15]<-23.426300526
  omegas[16]<-24.215877885
  omegas[17]<-25.181262364
  omegas[18]<-25.236072267
  omegas[19]<-25.290882171
  omegas[20]<-25.351380779
  omegas[21]<-27.045844008
  omegas[22]<-27.161152519
  omegas[23]<-27.221651128
  omegas[24]<-27.276461031
  omegas[25]<-27.331270935
  omegas[26]<-36.949222530
  omegas[27]<-37.738799889
  omegas[28]<-38.704184367
  omegas[29]<-38.758994271
  omegas[30]<-38.813804174
  omegas[31]<-38.874302783
  omegas[32]<-38.989611294
  omegas[33]<-40.799383035
  omegas[34]<-49.451950152
  omegas[35]<-50.472144534
  omegas[36]<-52.281916275
  omegas[37]<-52.512533298
  omegas[38]<-54.552922062
  omegas[39]<-62.185294797
  omegas[40]<-63.995066538
  omegas[41]<-66.035455302
  omegas[42]<-75.708216801
  omegas[43]<-77.748605565
  omegas[44]<-100.944289068

  omega_pos <- omegas

  iranks <- vector()
  iranks[ 1]<-16
  iranks[ 2]<-14
  iranks[ 3]<-13
  iranks[ 4]<-6
  iranks[ 5]<-30
  iranks[ 6]<-8
  iranks[ 7]<-12
  iranks[ 8]<-9
  iranks[ 9]<-17
  iranks[10]<-3
  iranks[11]<-23
  iranks[12]<-2
  iranks[13]<-5
  iranks[14]<-24
  iranks[15]<-44
  iranks[16]<-28
  iranks[17]<-21
  iranks[18]<-1
  iranks[19]<-36
  iranks[20]<-19
  iranks[21]<-31
  iranks[22]<-10
  iranks[23]<-26
  iranks[24]<-4
  iranks[25]<-35
  iranks[26]<-11
  iranks[27]<-37
  iranks[28]<-38
  iranks[29]<-15
  iranks[30]<-23
  iranks[31]<-39
  iranks[32]<-29
  iranks[33]<-27
  iranks[34]<-32
  iranks[35]<-7
  iranks[36]<-43
  iranks[37]<-18
  iranks[38]<-25
  iranks[39]<-40
  iranks[40]<-34
  iranks[41]<-42
  iranks[42]<-20
  iranks[43]<-41
  iranks[44]<-33

  omega_pos_rank <- iranks
  omega_sel      <- vector()
  omega_sel_rank <- vector()

  while (length(omega_pos) > 0) {
    #Find most relevant velocities
    maxrank <- which.min(omega_pos_rank)
    omega_pos_maxrank <- omega_pos[maxrank]
    if( omega_pos_maxrank * tdiff > 360){
      omega_sel       <- c(omega_sel,  omega_pos_maxrank)
      omega_sel_rank  <- c(omega_sel_rank, omega_pos_rank[maxrank])
      del_index       <- which((abs(omega_pos -  omega_pos_maxrank) * tdiff) < 360)
      omega_pos       <- omega_pos[-del_index]
      omega_pos_rank  <- omega_pos_rank[-del_index]
    } else {
      omega_pos      <- omega_pos[-maxrank]
      omega_pos_rank <- omega_pos_rank[-maxrank]
    }
  }
  omega_sel <- omega_sel[order(omega_sel)]

  #Computing the afuncs for the selected omegas
  afunc <- vector(mode = "numeric", length = 2 * length(omega_sel) + 1)
  afunc[1] <- 1.00000

  for(i in seq(2, 2 * length(omega_sel) + 1, 2)) {
    oxi          <- omega_sel[i / 2] * xi
    afunc[i]     <- cos(oxi)
    afunc[i + 1] <- sin(oxi)
  }

  return(list(omega_sel, omega_sel_rank, afunc))
}
