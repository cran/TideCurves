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
  omegas[ 1]<-0.0548098
  omegas[ 2]<-0.2306165
  omegas[ 3]<-1.0201944
  omegas[ 4]<-1.8097724
  omegas[ 5]<-2.0403886
  omegas[ 6]<-11.5978420
  omegas[ 7]<-11.7131503
  omegas[ 8]<-12.3874200
  omegas[ 9]<-12.6180365
  omegas[10]<-12.7881545
  omegas[11]<-13.5229227
  omegas[12]<-13.6382309
  omegas[13]<-13.6930407
  omegas[14]<-13.7535391
  omegas[15]<-15.9640460
  omegas[16]<-24.2158785
  omegas[17]<-25.1812631
  omegas[18]<-25.2360729
  omegas[19]<-26.2562673
  omegas[20]<-27.0458453
  omegas[21]<-27.1611535
  omegas[22]<-27.2764618
  omegas[23]<-27.3312716
  omegas[24]<-36.9492232
  omegas[25]<-38.7589956
  omegas[26]<-38.8743038
  omegas[27]<-38.9896120
  omegas[28]<-40.7993844
  omegas[29]<-49.4519514
  omegas[30]<-50.4721458
  omegas[31]<-52.5125347
  omegas[32]<-52.5673444
  omegas[33]<-54.5529235
  omegas[34]<-62.1852961
  omegas[35]<-63.9950685
  omegas[36]<-64.2256849
  omegas[37]<-75.7082187
  omegas[38]<-77.7486076
  omegas[39]<-100.9442917
  # omegas[41]<-66.035455302
  # omegas[42]<-75.708216801
  # omegas[43]<-77.748605565
  # omegas[44]<-100.944289068

  omega_pos <- omegas

  iranks <- vector()
  iranks[ 1]<-6
  iranks[ 2]<-13
  iranks[ 3]<-7
  iranks[ 4]<-31
  iranks[ 5]<-17
  iranks[ 6]<-14
  iranks[ 7]<-8
  iranks[ 8]<-34
  iranks[ 9]<-19
  iranks[10]<-39
  iranks[11]<-3
  iranks[12]<-4
  iranks[13]<-38
  iranks[14]<-21
  iranks[15]<-36
  iranks[16]<-11
  iranks[17]<-35
  iranks[18]<-1
  iranks[19]<-12
  iranks[20]<-33
  iranks[21]<-15
  iranks[22]<-2
  iranks[23]<-27
  iranks[24]<-10
  iranks[25]<-16
  iranks[26]<-24
  iranks[27]<-22
  iranks[28]<-23
  iranks[29]<-29
  iranks[30]<-5
  iranks[31]<-9
  iranks[32]<-37
  iranks[33]<-30
  iranks[34]<-25
  iranks[35]<-28
  iranks[36]<-26
  iranks[37]<-20
  iranks[38]<-18
  iranks[39]<-32
  # iranks[40]<-34
  # iranks[41]<-42
  # iranks[42]<-20
  # iranks[43]<-41
  # iranks[44]<-33

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
