% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_cost.R
\name{minimum_cost}
\alias{minimum_cost}
\title{Computes the minimum cost function}
\usage{
minimum_cost(a, alpha, beta, delta, gamma, tau, R)
}
\arguments{
\item{a}{vector with the breaking points of charging function in the x-axis}

\item{alpha}{vector with the slopes of the charging function on each segment}

\item{beta}{vector with the y-intercepts of the charging function on each segment}

\item{delta}{vector with the times duration of each segment of electricity price function}

\item{gamma}{vector with the prices of the electricity on each segment of electricity price function}

\item{tau}{consumption of the vehicle (numerical value)}

\item{R}{range of the vehicle (numerical value)}
}
\value{
list with the x-values and y-values of the minimum cost function
}
\description{
Function that computes the minimal cost function for a given charging function (given by a, alpha and beta), an electricity price function (given by delta and gamma), a consumption tau and a range R
}
\examples{
a <- c(0,3.3,6.6,10)
alpha <- c(0.1757576, 0.07272727, 0.05294118)
beta <- c(0, 0.34, 0.4705882)
delta <- c(4, 3, 5)
gamma <- c(0.45, 0.25, 0.5)
tau <- 0.15
R <- 250
opt_cost_function = minimum_cost(a, alpha, beta, delta, gamma, tau, R)
print(opt_cost_function)
}
