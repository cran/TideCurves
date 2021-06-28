#' Builds the design matrix
#' @description Builds the xdesign.matrix by calling Funcs. For internal use.
#'
#' @param tdiffa The difference in days as double which stems from the analysis period.
#' @param numma The transit number (start).
#' @param numme The transit number (end).
#'
#' @return Returns a matrix
#'
BuildDesign <- function(tdiffa, numma, numme) {
  matrix.cols      <- length(Funcs(tdiff = tdiffa, xi = 1)[[3]])

  xdesign.matrix      <- matrix(0.0, nrow = (numme - numma + 1), ncol = matrix.cols + 1)
  xdesign.matrix[, 1] <- seq.int(numma, numme, 1)

  for(i in 1 : nrow(xdesign.matrix)){
    xdesign.matrix[i, 2: (matrix.cols + 1)] <- Funcs(xi = xdesign.matrix[i, 1], tdiff = tdiffa)[[3]]
  }
  colnames(xdesign.matrix) <- c("numm", paste0("V","", seq(1L : matrix.cols)))
  return(xdesign.matrix)
}