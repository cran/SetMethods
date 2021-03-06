\name{EMMF}
\alias{EMMF}
\docType{data}
\title{EMMFnegger (2011)}
\description{The \code{EMMF} data frame has 19 rows and 8 sets}
\usage{data(EMMF)}
\format{
  A data frame with 19 observations on the following 8 sets.
  \describe{
    \item{\code{country}}{a factor with levels \code{Australia} \code{Austria} \code{Belgium} \code{Canada} \code{Denmark} \code{Finland} \code{France} \code{Germany} \code{Ireland} \code{Italy} \code{Netherlands} \code{NewZealand} \code{Norway} \code{Portugal} \code{Spain} \code{Sweden} \code{Switzerland} \code{UK} \code{USA}}
    \item{\code{s}}{a numeric vector. Condition, state-society relationships.}
    \item{\code{c}}{a numeric vector. Condition, non-market coordination.}
    \item{\code{l}}{a numeric vector. Condition, strength of the labour movement.}
    \item{\code{r}}{a numeric vector. Condition, religious denomination.}
    \item{\code{p}}{a numeric vector. Condition, strenght of religious parties.}
    \item{\code{v}}{a numeric vector. Condition, institutional veto points.}
    \item{\code{jsr}}{a numeric vector. Outcome, job-secturity regulations.}
  }
}
\details{Data are used by Emmenegger (2011) to analyze job-security regulations in Western democracies. The data are fuzzy-sets.}
%\source{}
\references{Emmenegger, P. (2011) "Job-security regulations in Western democracies", European Journal of Political Research 50, pp. 336-364.
	
Schneider, C. Q., Wagemann, C. (2012) Set-Theoretic Methods for the Social Sciences, Cambridge University Press: Cambridge.
}
\examples{data(EMMF)}
\keyword{datasets}
