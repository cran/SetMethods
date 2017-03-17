\name{pimdata}
\alias{pimdata}
\title{
	Function to extract prime implicants table from object of class "qca"	
}
\description{
	A function that displays each case's set membership scores in each sufficient term, the solution formula, and the outcome from an object of class
	"qca".  
}
\usage{
pimdata(results, outcome, intermed = FALSE, sol = 1)
}
\arguments{
	\item{results}{
    An object of class "qca". For performing pimdata of the sufficient solution for the negated outcome one     must only use the \code{eqmcc()} result from the sufficiency analysis of the negated outcome.
	}
	\item{outcome}{
		A character string with the name of the outcome in capital letters. When performing pimdata of the          sufficient solution for the negated outcome one must only use the \code{eqmcc()} result from the            sufficiency analysis of the negated outcome in the argument \code{results}. Changing the name in the        argument \code{outcome} or using a tilde is not necessary.
	}
	\item{intermed}{
		Logical. Use the intermediate solution?
	}
	\item{sol}{
		A numeric vector where the first number indicates the number of the
		solution in case of model ambiguity according to the order in the "qca" object.
	}
}
\value{
	A table with set memberships.
	\item{solution_formula}{The solution formula.}
	\item{out}{Membership in the outcome.}
}
%\references{}
\author{
	Ioana-Elena Oana and Juraj Medzihorsky
}
\seealso{
	\code{\link[QCA:eqmcc]{eqmcc}}
	\code{\link[SetMethods:pimplot]{pimplot}}
}
\examples{
# Import your data. For example:

data(Schneider)

# Get the parsimonious solution:


sol_yp <- eqmcc(Schneider, outcome = "EXPORT",
                conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                incl.cut1 = .9, 
                include = "?", 					   
                details = TRUE, show.cases = TRUE)

# Get the intermediate solution:

sol_yi <- eqmcc(Schneider, outcome = "EXPORT",
                conditions = c("EMP","BARGAIN","UNI","OCCUP","STOCK", "MA"),
                incl.cut1 = .9, 
                include = "?", 					   
                details = TRUE, show.cases = TRUE, dir.exp = c(0,0,0,0,0,0))


# Get the prime implicants table for the parsimonious solution:

pimdata(results = sol_yp, outcome = "EXPORT")

# Get the prime implicants table for the first intermediate solution:

pimdata(results = sol_yi, outcome = "EXPORT", intermed = TRUE, sol = 1)

}
\keyword{QCA}
