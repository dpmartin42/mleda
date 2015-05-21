\name{mleda}
\alias{mleda}
\docType{package}
\title{
Multilevel Exploratory Data Analysis
}
\description{
Perform efficient exploratory data analysis for multilevel data 
structures (limited to two levels) using random forests and 
mixed-effects models. Uses quick-and-dirty partial dependence 
plots for visualization (see plotmo package).
}
\details{
\tabular{ll}{
Package: \tab mleda\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-05-20\cr
License: \tab Apache License 2.0\cr
}
This package contains two main functions. The first, plot_ml, allows the user to plot the relations between a set of
variables and a given outcome. Plots include both main effects and two-way interactions, and can be based on either
loess curves using raw data or predictions from a randomForest, cforest, or lme4 model object. The second, validate_ml
performs either split-half or 5-fold cross-validation (both at the cluster level) to estimate test performance for 
randomForest, cforest, or lme4 models. In the continuous case, proportion of variation is reported (i.e., 1 - MSE/var(y)).
In the classification case, accuracy is reported (i.e., (TP + TN)/(TP + TN + FP + FN)).
}
\author{
Daniel Martin <dpmartin42@gmail.com>
}
\references{
Martin, D. P. (2015). Efficiently exploring multilevel data with recursive partitioning (Unpublished doctoral
dissertation). University of Virginia, Charlottesville, VA. 
}
\keyword{multilevel random forest, exploratory data analysis}
\seealso{
}
\examples{
~~ simple examples of the most important functions ~~
}
