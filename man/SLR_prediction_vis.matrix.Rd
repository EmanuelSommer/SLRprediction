% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SLR_prediction_vis.R
\name{SLR_prediction_vis.matrix}
\alias{SLR_prediction_vis.matrix}
\title{SLR prediction visualization S3 method for matrices}
\usage{
\method{SLR_prediction_vis}{matrix}(y, col_response = 1,
  col_exploratory = 2, pre_x_value = mean(y[, col_exploratory]),
  pre_col = "red", reg_col = "blue")
}
\arguments{
\item{y}{a numeric \code{matrix} containing the response and exploratory variable}

\item{col_response}{column number of the response variable within y (default is 1)}

\item{col_exploratory}{column number of the exploratory variable within y (default is 2)}

\item{pre_x_value}{the numeric value (within the range of the exploratory vector) for which the prediction is made}

\item{pre_col}{color of the highlighted prediction within the scatterplot}

\item{reg_col}{color of the regression line}
}
\value{
a scatterplot of the two variables + regression line + highlighted prediction
}
\description{
Simple visualization tool for predictions within a simple linear regression model.
}
\details{
The generic function first builds a simple linear regression model with one response and one exploratory variable. Then the prediction for a given value of the exploratory variable (default the mean and no out of sample predictions) gets computed.(see \link{SLR_prediction}) Finally using these bits of information a scatterplot of the two variables with the regression line gets returned. Moreover the predicition will be highlighted in the scatterplot and the exact values of the predicition will be displayed above the plot.
}
\examples{
special <- 6
Y<-matrix(data = append(1:5,c(1,3,4.5,7,8)),byrow = FALSE,ncol = 2)
SLR_prediction_vis(Y,pre_x_value = special)
}
\seealso{
\code{\link{SLR_prediction}}
}
\author{
Emanuel Sommer
}
