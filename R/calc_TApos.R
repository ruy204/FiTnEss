## 3. calculate number of TA sites

#' Map TA sites onto corresponding genes
#'
#' This function is part of the \code{tallyprepfun} function, which pre-process
#' raw tally file for analysis. In this step, the function uses raw tally .txt
#' file (imported from function \code{import_raw_tally})
#'
#' @param tally raw tally.txt file that imported from \code{import_raw_tally}
#'   function.
#' @param internal data in the package, strain-specific, containing gene name,
#'   TA site positions of each gene.
#' @return numeric vector of positions.
calc_TApos <- function(tally, genelist) {
  x <- merge(tally, genelist, by = c("Locus.CIA", "strain"), all.x = TRUE)
  ## x <- tally %>% full_join(genelist,by = c('Locus.CIA','strain'))
  x <- dplyr::filter(x, type == "CDS")  #5858 genes
  x <- dplyr::group_by(x, Locus.CIA) %>%
    dplyr::mutate(TA.gene.pos = TA_start - gene_start + 1) %>%
    dplyr::mutate(gene.size.CIA = gene_stop - gene_start + 1)
  x <- as.data.frame(x)
  x$TA.gene.pos[x$strand == "-"] <- x$gene.size.CIA[x$strand == "-"] -
    x$TA.gene.pos[x$strand == "-"] + 1
  x <- dplyr::mutate(x, TA.gene.percent = TA.gene.pos/gene.size.CIA)
  x <- x[!duplicated(x$TA_start), ]
  x$TAindex <- as.numeric(x$TAindex)
  return(x)
}
