
#' Cluster information
#'
#' @format A data frame with variables:
#' \describe{
#' \item{gene_name}{Identified strain name where gene comes from}
#' \item{patric_id}{
#'     \url{http://www.fueleconomy.gov/feg/ws/wsData.shtml#VClass}}}
#' \item{Locus.CIA}{Gene name}
#' \item{length}{Gene length}
#' \item{cluster}{Cluster information about this gene}
#' \item{desc}{Description of gene functions}
#' \item{strain}{Strain}
#' }
"cluster"

#' Raw tally file processing
#'
#' @format A list contains nine sub-lists for tally processing supporting files
#' \describe{ A list containing nine sub-lists: \code{PA14}, \code{X13273},
#'     \code{PS75}, \code{CF77}, \code{BWH015}, \code{BWH013}, \code{BWH005},
#'     \code{BL23}, \code{19660}.
#'  In each sub-list there are 4 files:
#' \item{nonPermissiveTA}{Denotes non-permissive TA sites}
#' \item{homo}{Denotes homology TA sites}
#' \item{genelist}{Denotes gene name and start and stop}
#' \item{geneinfo}{Denotes gene type (CDS), strand information and gene name}
#' }
"support_list"




