## 2. denotate TAs that are homologous

#' Find TAs which are homologous, to what I am not certain.
#'
#' @param tally Parsed tally.txt
#' @param homofile File containing homology information.
find_homo <- function(tally, homofile) {
  homofile2 <- homofile %>%
    dplyr::group_by(V2) %>%
    dplyr::summarise(side = n()) %>%
    dplyr::select(V2, side)
  colnames(homofile2) <- c("TA_start", "side")
  tally$homo <- FALSE
  tally$homo[which(tally$TA_start %in% homofile2$TA_start)] <- TRUE
  tally <- tally %>%
    dplyr::left_join(homofile2, by = "TA_start")
  tally$side[which(is.na(tally$side))] <- 0
  return(tally)
}
