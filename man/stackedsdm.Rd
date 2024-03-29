% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stackedsdm.R
\name{stackedsdm}
\alias{stackedsdm}
\title{Stacked species regression models, possibly fitted in parallel}
\usage{
stackedsdm(
  y,
  formula_X = ~1,
  data = NULL,
  family = "negative.binomial",
  trial_size = 1,
  do_parallel = FALSE,
  ncores = NULL,
  trace = FALSE
)
}
\arguments{
\item{y}{A matrix of species responses}

\item{formula_X}{An object of class \code{formula} representing the relationship to the covariates to be fitted. There should be nothing to the left hand side of the "~" sign.}

\item{data}{Data frame of the covariates}

\item{family}{Either a single character vector, in which case all responses are assumed to be from this family, or a vector of character strings of the same length as the number of columns of y. Families as strings and not actual \code{family} class objects. This could be changed though if desired in the future e.g., for custom link functions. Currently, the following families are supported (hopefully properly!): "gaussian", "negative.binomial" (with quadratic mean-variance relationship), "poisson", "binomial" (with logit link), "tweedie", "Gamma" (with log link), "exponential", "beta" (with logit link), "ordinal" (cumulative logit model), "ztpoisson", "ztnegative.binomial", "zipoisson", "zinegative.binomial".}

\item{trial_size}{The trial size if any of the responses are binomial. Is either a single number or a matrix with the same dimension as y. If the latter, then all columns that do not correspond to binomial responses are ignored.}

\item{do_parallel}{Do the separate species model fits in parallel? Defaults to \code{TRUE}}

\item{ncores}{The number of cores to use if separate the species model fits are done in parallel. If \code{do_parallel = TRUE}, then it defaults to \code{detectCores() - 2}}

\item{trace}{Print information. This is not actually used currently}
}
\value{
A object of class \code{stackedsdm} with the following components:
\code{call} The function call;
\code{fits} A list where the j-th element corresponds to the to the fitted model for species j i.e., the j-th column in \code{y};
\code{linear_predictor} A matrix of the fitted linear predictors
\code{fitted} A matrix of the fitted values
}
\description{
Stacked species regression models, possibly fitted in parallel
}
\section{Details}{

\code{stackedsdm} behaves somewhat like the \code{manyglm} or \code{manyany} function in the package \code{\link{mvabund}}, in the sense that it fits a separate regression to each species response i.e., column of \code{y}. The main difference is that different families can be permitted for each species, which thus allows for mixed responses types.
}

\section{Author(s)}{

Francis K.C. Hui <francis.hui@anu.edu.au>.
}

\examples{
data(spider)
X <- spider$x
abund <- spider$abund

# Example 1: Simple example
myfamily <- "negative.binomial"
# Example 1: Funkier example where Species are assumed to have different distributions
# Fit models including all covariates are linear terms, but exclude for bare sand
fit0 <- stackedsdm(abund, formula_X = ~. -bare.sand, data = X, family = myfamily, ncores = 2) 

# Example 2: Funkier example where Species are assumed to have different distributions
abund[,1:3] <- (abund[,1:3]>0)*1 # First three columns for presence absence
myfamily <- c(rep(c("binomial"), 3),
              rep(c("negative.binomial"), (ncol(abund)-3)))
fit0 <- stackedsdm(abund, formula_X = ~ bare.sand, data = X, family = myfamily, ncores = 2)
}
