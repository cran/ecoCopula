% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cgr_s3.R
\name{print.cgr}
\alias{print.cgr}
\title{Print function for cgr object}
\usage{
\method{print}{cgr}(x, ...)
}
\arguments{
\item{x}{is a cgr object, e.g. from output of \code{\link{cgr}}.}

\item{...}{not used}
}
\description{
Print function for cgr object
}
\examples{
abund <- spider$abund[,1:5]
spider_mod <- stackedsdm(abund,~1, data = spider$x, ncores=2) 
spid_graph=cgr(spider_mod)
print(spid_graph)
}
\seealso{
\code{\link{cgr}}
}
