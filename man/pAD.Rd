\name{pAD}
\alias{pAD}
\alias{qAD}
\title{
  Null Distribution of Anderson-Darling Test Statistic
}
\description{
  \code{pAD} computes the cumulative distribution function,
  and \code{qAD} computes the quantile function,
  of the null distribution of the Anderson-Darling test
  statistic.
}
\usage{
  pAD(q, n = Inf, lower.tail = TRUE, fast=TRUE)
  qAD(p, n = Inf, lower.tail = TRUE, fast=TRUE)
}
\arguments{
  \item{q}{
    Numeric vector of quantiles (values for which the
    cumulative probability is required).
  }
  \item{p}{
    Numeric vector of probabilities.
  }
  \item{n}{
    Integer. Sample size for the Anderson-Darling test.
  }
  \item{lower.tail}{
    Logical. If \code{TRUE} (the default),
    probabilities are \eqn{P(X \le q)}{P(X <= q)},
    and otherwise they are \eqn{P(X > q)}.
  }
  \item{fast}{
    Logical value indicating whether to use a fast algorithm
    or a slower, more accurate algorithm, in the case \code{n=Inf}.
  }
}
\details{
  \code{pAD} uses the algorithms and C code described
  in Marsaglia and Marsaglia (2004).

  \code{qAD} uses \code{\link[stats]{uniroot}} to find the
  quantiles.

  The argument \code{fast} applies only when \code{n=Inf}
  and determines whether the asymptotic distribution is approximated
  using the faster algorithm \code{adinf} (accurate to 4-5 places)
  or the slower algorithm \code{ADinf} (accurate to 11 places)
  described in Marsaglia and Marsaglia (2004).
}
\value{
  A numeric vector of the same length as \code{p} or \code{q}.
}
\references{
  Anderson, T.W. and Darling, D.A. (1952)
  Asymptotic theory of certain 'goodness-of-fit' criteria based
  on stochastic processes.
  \emph{Annals of Mathematical Statistics} \bold{23}, 193--212.

  Anderson, T.W. and Darling, D.A. (1954)
  A test of goodness of fit.
  \emph{Journal of the American Statistical Association} \bold{49}, 765--769.

  Marsaglia, G. and Marsaglia, J. (2004)
  Evaluating the Anderson-Darling Distribution.
  \emph{Journal of Statistical Software} \bold{9} (2), 1--5.
  February 2004. 
  \doi{10.18637/jss.v009.i02}
}
\author{
  Original C code by G. and J. Marsaglia.
  \R interface by Adrian Baddeley.
}
\seealso{
 \code{\link{ad.test}}
}
\examples{
  pAD(1.1, n=5)
  pAD(1.1)
  pAD(1.1, fast=FALSE)

  qAD(0.5, n=5)
  qAD(0.5)
}
\keyword{distribution}
\keyword{htest}
