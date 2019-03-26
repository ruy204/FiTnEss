## 4. denotate core TAs

#' A quick and dirty search for core TAs.
#'
#' @param tally Parsed tally data.
#' @param bp Minimum number of bps!
#' @return a new tally.
denote_coreTA <- function(tally, bp = 50) {
  tally$coreTA <- ((tally$TA.gene.pos > bp) &
                   (tally$TA.gene.pos < (tally$gene.size.CIA - bp + 1)))
  return(tally)
}
