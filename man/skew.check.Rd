\name{skew.check}
\alias{skew.check}
\title{
	Function for checking how skewed sets are.
}
\description{
	A function that identifies how skewed sets are by returning the number and percentage of cases with higher than 0.5 fuzzy-set values. The function can also return histograms of the calibrated sets.}
\usage{ 
skew.check(data, hist =  FALSE, main = NULL)
}
\arguments{
	\item{data}{
		A datafarme, a subset of a dataframe, or a vector (i.e. single column in a dataframe). The function should      be used for calibrated data and will give an error if the data contains uncalibrated scores. However, if you     have both calibrated and uncalibrated data in the same dataframe, it is possible to use the function only       for the calibrated subset of that data. 
	}
	\item{hist}{Logical. Should the function also return histograms of the sets?
	}
	\item{main}{Title for the plot. When provided with a dataframe, the function automatically assigns the column name to the plot.
	}
}
\author{
	Ioana-Elena Oana
}
\examples{
# Import your data. For example:

data(SCHF)

# Check skewness for the entire dataframe:

skew.check(SCHF)

# Check skewness for the column "EMP" in the dataframe:

skew.check(SCHF$EMP)

# Check skewness for the 5th column of the dataframe:

skew.check(SCHF[,5])

}
\keyword{QCA}