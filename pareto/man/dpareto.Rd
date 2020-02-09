\name{dpareto}
\alias{dpareto}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Density function of the Pareto distribution
}
\description{
A function to compute the density of the Pareto distribution.
}
\usage{
dpareto(x, a, b, log = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
vector of quantiles.
}
  \item{a}{
vector of scales.
}
  \item{b}{
vector of shapes.
}
  \item{log}{
logical; if TRUE, probabilities p are given as log(p).
}
}
\details{
The pareto distribution has density

\eqn{f(x|\alpha,\beta) = (\beta\alpha^\beta)/(x^{\beta + 1})}

where \eqn{0  < \alpha < x and \beta > 0}

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\code{dpareto} gives the density, 

Invalid arguments will result in return value NaN, with a warning.

The length of the result is determined by the maximum of the lengths of the numerical arguments.

The numerical arguments other than n are recycled to the length of the result. Only the first elements of the logical arguments are used.
}

\author{
Yikai Zhang
}


\examples{
dpareto(3,2,1)
dpareto(1,2,3)
dpareto(1:5,2, 1)
dpareto(1:5,2, 1, log = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{arith}% use one of  RShowDoc("KEYWORDS")

