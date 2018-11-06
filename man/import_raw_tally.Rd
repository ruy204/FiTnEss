\name{import_raw_tally}
\alias{import_raw_tally}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Import raw tally file for processing
}
\description{
The function imports raw tally .txt file into the function and start processing.
}
\usage{
import_raw_tally(filename,CIA=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{directory and name for raw tally .txt file, for example: \code{"/folder/raw_tally_rep1.txt"}}
  \item{CIA}{gene type, \code{CIA=TRUE} only include}
}
\details{
The input data needs to be .txt file, with 6 columns: \code{index}(row index, or chromosome index), \code{TA_start}(start position of TA site),
\code{TA_end}(end position of TA site), \code{Locus.CIA}(gene name), \code{plus_n}(number of reads aligned on + strand), \code{minus_n}(number of reads aligned on - strand).

For example, \code{raw_tally[1,]}

\code{ChrI  3  4  PA14_00010  0  1}
}
\value{
Returns a dataframe
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
