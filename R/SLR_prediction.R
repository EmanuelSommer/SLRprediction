
#' SLR prediction
#'
#' Predicting with a simple linear regression model. (S3 generic function)
#'
#' The generic function first builds a simple linear regression model with one response and one exploratory variable. Then the prediction for a given value of the exploratory variable (default the mean and no out of sample predictions) gets returned.
#'
#'
#' @param y depends on the class of the input: for
#' \describe{
#'  \item{numeric vectors}{a numeric vector representing the response variable}
#'  \item{data frames}{a numeric \code{data.frame} containing the response and exploratory variable}
#'  \item{matrix}{a numeric \code{matrix} containing the response and exploratory variable}
#'  }
#' @param exploratory numeric vector representing the exploratory variable, must have the same length as y (for the input class numeric only)
#' @param col_response column number of the response variable within y (default is 1) (for data frames and matrices only)
#' @param col_exploratory column number of the exploratory variable within y (default is 2) (for data frames and matrices only)
#' @param pre_x_value the numeric value (within the range of the exploratory vector) for which the prediction is made
#'
#' @return the predicted value of the response variable
#' @export
#' @author Emanuel Sommer
#'
#' @examples
#' ## numeric S3 method
#' response <- c(1:5)
#' exploratory <- c(1, 3, 4.5, 7, 8)
#' special <- 6
#' SLR_prediction(response, exploratory, pre_x_value = special)
#'
#' ## data.frame S3 method
#' Y <- data.frame(c(1:5), c(1, 3, 4.5, 7, 8))
#' SLR_prediction(Y, pre_x_value = special)
#'
#' ## matrix S3 method is eqivalently used as the one for data frames
#' @seealso \code{\link{SLR_prediction_vis}} \code{\link{SLR_prediction.numeric}} \code{\link{SLR_prediction.data.frame}} \code{\link{SLR_prediction.matrix}}
SLR_prediction <- function(y, exploratory, col_response, col_exploratory, pre_x_value) {
  UseMethod("SLR_prediction")
}



#' SLR prediction default method
#'
#' @param y  an object of a wrong class
#' @export
#' @author Emanuel Sommer
SLR_prediction.default <- function(y) {
  warning("The function is not defined for this class of input")
}

#' SLR prediction numeric S3 method
#'
#' Predicting with a simple linear regression model
#'
#' The function first builds a simple linear regression model with one response and one exploratory variable. Then the prediction for a given value of the exploratory variable (default the mean and no out of sample predictions) gets returned.
#'
#'
#' @param y a numeric vector (the response variable)
#' @param exploratory a numeric vector (the exploratory variable with the same length as y)
#' @param pre_x_value the numeric value (within the range of the exploratory vector) for which the prediction is made
#'
#' @return the predicted value of the response variable
#' @export
#' @author Emanuel Sommer
#'
#'
#' @examples
#' ## numeric S3 method
#' response <- c(1:5)
#' exploratory <- c(1, 3, 4.5, 7, 8)
#' special <- 6
#' SLR_prediction(response, exploratory, pre_x_value = special)
#' @seealso \code{\link{SLR_prediction_vis}}
SLR_prediction.numeric <- function(y, exploratory, pre_x_value = mean(exploratory)) {
  if (pre_x_value > max(exploratory) | pre_x_value < min(exploratory)) {
    stop("prediction out of sample")
  }
  model <- lm(y ~ exploratory)
  pre_y_value <- predict(model, data.frame(exploratory = pre_x_value),
    interval = "prediction"
  )[1]
  pre_y_value
}

#' SLR prediction S3 method for data frames
#'
#' Predicting with a simple linear regression model
#'
#' The function first builds a simple linear regression model with one response and one exploratory variable. Then the prediction for a given value of the exploratory variable (default the mean and no out of sample predictions) gets returned.
#'
#'
#' @param y a numeric \code{data.frame} containing the response and exploratory variable
#' @param col_response column number of the response variable within y (default is 1)
#' @param col_exploratory column number of the exploratory variable within y (default is 2)
#' @param pre_x_value the numeric value (within the range of the exploratory vector) for which the prediction is made
#'
#' @return the predicted value of the response variable
#' @export
#' @author Emanuel Sommer
#'
#' @examples
#' special <- 6
#' Y <- data.frame(c(1:5), c(1, 3, 4.5, 7, 8))
#' SLR_prediction(Y, pre_x_value = special)
#' @seealso \code{\link{SLR_prediction_vis}}
SLR_prediction.data.frame <- function(y, col_response = 1, col_exploratory = 2,
                                      pre_x_value = mean(y[, col_exploratory])) {
  res <- y[, col_response]
  exp <- y[, col_exploratory]
  if (pre_x_value > max(exp) | pre_x_value < min(exp)) {
    stop("prediction out of sample")
  }
  model <- lm(res ~ exp)
  pre_y_value <- predict(model, data.frame(exp = pre_x_value),
    interval = "prediction"
  )[1]
  pre_y_value
}

#' SLR prediction S3 method for matrices
#'
#' Predicting with a simple linear regression model
#'
#' The function first builds a simple linear regression model with one response and one exploratory variable. Then the prediction for a given value of the exploratory variable (default the mean and no out of sample predictions) gets returned.
#'
#'
#' @param y a numeric \code{matrix} containing the response and exploratory variable
#' @param col_response column number of the response variable within y (default is 1)
#' @param col_exploratory column number of the exploratory variable within y (default is 2)
#' @param pre_x_value the numeric value (within the range of the exploratory vector) for which the prediction is made
#'
#' @return the predicted value of the response variable
#' @export
#' @author Emanuel Sommer
#'
#' @examples
#' special <- 6
#' Y <- matrix(data = append(1:5, c(1, 3, 4.5, 7, 8)), byrow = FALSE, ncol = 2)
#' SLR_prediction(Y, pre_x_value = special)
#' @seealso \code{\link{SLR_prediction_vis}}
SLR_prediction.matrix <- function(y, col_response = 1, col_exploratory = 2,
                                  pre_x_value = mean(y[, col_exploratory])) {
  res <- y[, col_response]
  exp <- y[, col_exploratory]
  if (pre_x_value > max(exp) | pre_x_value < min(exp)) {
    stop("prediction out of sample")
  }
  model <- lm(res ~ exp)
  pre_y_value <- predict(model, data.frame(exp = pre_x_value),
    interval = "prediction"
  )[1]
  pre_y_value
}

# methods(SLR_prediction)
