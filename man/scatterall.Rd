\name{scatterall}
\alias{scatterall}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  scatterall
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
scatterall(data, no.export = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{no.export}{
%%     ~~Describe \code{no.export} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
function (data, no.export = TRUE, ...) 
{
    lst <- init(data)
    quantitatives <- lst[[1]]
    factors <- lst[[2]]
    for (i in 1:length(quantitatives)) {
        for (j in i:length(quantitatives)) {
            if (i != j) {
                if (no.export == FALSE) {
                  png(filename = paste("scatter_", paste(i), 
                    "_", paste(j), ".png", sep = ""))
                }
                plot(x = get(quantitatives[i], pos = data), y = get(quantitatives[j], 
                  pos = data), xlab = quantitatives[i], ylab = quantitatives[j], 
                  ...)
                if (no.export == FALSE) {
                  dev.off()
                }
            }
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
