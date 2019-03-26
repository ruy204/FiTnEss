## 2. multiple Nta categories

#' I have to admit, when I look at this, I see only number of bagels for fun!
#'
#' @param q because p would be absurd!
#' @param lp hmm long play?
#' @param sigma The sigma parameter.
#' @param nta The number of TAs observed.
#' @return Vector of sums.
nblgnfun2 <- function(q, lp, sigma, nta) {
  lbd1 <- 1/(rlnorm(1e+05, meanlog = lp, sdlog = sigma))
  ndist <- rnbinom(1e+05, nta, lbd1)
  pvec <- sapply(q, function(x) {
    sum(x >= ndist, na.rm = TRUE) / length(ndist)
  })
  return(pvec)
}

#' Get a vector describing a negative binomial distribution.
#'
#' @param q not p
#' @param nta Number of TAs.
#' @return binomial vector.
lownbfun3 <- function(q, nta) {
  pvec <- pnbinom(q, nta, 0.7)
  return(pvec)
}

#' jointing things is indeed fun!  I need to read around to figure out what this
#' is doing.
#'
#' I want to grumble for a moment; why do science people insist on such stupid
#' names for variables. Look, I get it that sigma usually means standard
#' deviation and mu mean; but cannot we get a frigging clue to remind us what is
#' being counted?
#'
#' @param q not p
#' @param lambda not mu
#' @param lp Something which I do not know what it is.
#' @param sigma no, I want tau!  0.5 pi makes more sense.
#' @param nta The number of TAs.
#' @return vector of lambda * (the bagelfuns() + (1 - labmda) * the low n
#'   bfuns!)
jointfun <- function(q, lambda, lp, sigma, nta) {
  pvec <- lambda * nblgnfun2(q, lp, sigma, nta) + (1 - lambda) * lownbfun3(q, nta)
  return(pvec)
}
