#' SLR prediction visualization
#'
#' Simple visualization tool for predictions within a simple linear regression model. (S3 generic function)
#'
#' The generic function first builds a simple linear regression model with one response and one exploratory variable. Then the prediction for a given value of the exploratory variable (default the mean and no out of sample predictions) gets computed.(see \link{SLR_prediction}) Finally using these bits of information a scatterplot of the two variables with the regression line gets returned. Moreover the predicition will be highlighted in the scatterplot and the exact values of the predicition will be displayed above the plot.
#'
#' @param y depends on the class of the input: for
#' \describe{
#'  \item{numeric vectors}{see \link{SLR_prediction_vis.numeric}}
#'  \item{data frames}{see \link{SLR_prediction_vis.data.frame}}
#'  \item{matrix}{see \link{SLR_prediction_vis.matrix}}
#'  }
#' @param ... also depending on the class of the desired input
#'
#' @return a scatterplot of the two variables + regression line + highlighted prediction
#' @export
#' @author Emanuel Sommer
#'
#' @examples ##numeric S3 method
#' response <- c(1:5)
#' exploratory <- c(1,3,4.5,7,8)
#' special <- 6
#' SLR_prediction_vis(response,exploratory,pre_x_value = special)
#'
#' ##data.frame S3 method
#' Y<-data.frame(c(1:5),c(1,3,4.5,7,8))
#' SLR_prediction_vis(Y,pre_x_value = special)
#'
#' ##matrix S3 method is eqivalently used as the one for data frames
#' @import ggplot2
#' @seealso \link{SLR_prediction}
SLR_prediction_vis<-function(y,...){UseMethod("SLR_prediction_vis")}

#' SLR prediction visualization default method
#'
#' @param y  an object of a wrong class
#' @export
#' @author Emanuel Sommer
SLR_prediction_vis.default<-function(y){warning("The function is not defined for this class")}

#' SLR prediction visualization numeric S3 method
#'
#' Simple visualization tool for predictions within a simple linear regression model.
#'
#' The generic function first builds a simple linear regression model with one response and one exploratory variable. Then the prediction for a given value of the exploratory variable (default the mean and no out of sample predictions) gets computed.(see \link{SLR_prediction}) Finally using these bits of information a scatterplot of the two variables with the regression line gets returned. Moreover the predicition will be highlighted in the scatterplot and the exact values of the predicition will be displayed above the plot.
#'
#' @param y a numeric vector (the response variable)
#' @param exploratory a numeric vector (the exploratory variable with the same length as y)
#' @param pre_x_value the numeric value (within the range of the exploratory vector) for which the prediction is made
#' @param pre_col color of the highlighted prediction within the scatterplot
#' @param reg_col color of the regression line
#'
#' @return a scatterplot of the two variables + regression line + highlighted prediction
#' @export
#' @author Emanuel Sommer
#' @import ggplot2
#'
#' @examples response <- c(1:5)
#' exploratory <- c(1,3,4.5,7,8)
#' special <- 6
#' SLR_prediction_vis(response,exploratory,pre_x_value = special)
#' @seealso \link{SLR_prediction}
SLR_prediction_vis.numeric<-function(y,exploratory,pre_x_value=mean(exploratory),pre_col="red",reg_col="blue"){
  #library(ggplot2)
  pre_y_value<-SLR_prediction(y,exploratory,pre_x_value)
  vis_data<-data.frame(y,exploratory,pre_x_value,pre_y_value)
  ggplot2::ggplot(vis_data,aes(x=exploratory,y=y))+geom_point()+
    geom_vline(xintercept = pre_x_value,col=pre_col)+
    geom_hline(yintercept = pre_y_value,col=pre_col)+
    geom_smooth(method = "lm",se=F,col=reg_col,alpha=0.6)+
    labs(y="response variable",x="exploratory variable",
         title=paste("For the exploratory value",round(pre_x_value,digits = 3),"the simple regression model predicts the response value:",round(pre_y_value,digits = 3)))
}

#' SLR prediction visualization S3 method for data frames
#'
#' Simple visualization tool for predictions within a simple linear regression model.
#'
#' The generic function first builds a simple linear regression model with one response and one exploratory variable. Then the prediction for a given value of the exploratory variable (default the mean and no out of sample predictions) gets computed.(see \link{SLR_prediction}) Finally using these bits of information a scatterplot of the two variables with the regression line gets returned. Moreover the predicition will be highlighted in the scatterplot and the exact values of the predicition will be displayed above the plot.
#'
#'
#' @param y a numeric \code{data.frame} containing the response and exploratory variable
#' @param col_response column number of the response variable within y (default is 1)
#' @param col_exploratory column number of the exploratory variable within y (default is 2)
#' @param pre_x_value the numeric value (within the range of the exploratory vector) for which the prediction is made
#' @param pre_col color of the highlighted prediction within the scatterplot
#' @param reg_col color of the regression line
#'
#' @return a scatterplot of the two variables + regression line + highlighted prediction
#' @export
#' @author Emanuel Sommer
#' @import ggplot2
#'
#'
#' @examples response <- c(1:5)
#' exploratory <- c(1,3,4.5,7,8)
#' Y <- data.frame(response,exploratory)
#' special <- 6
#' SLR_prediction_vis(Y,pre_x_value = special)
#' @seealso \link{SLR_prediction}
SLR_prediction_vis.data.frame<-function(y,col_response=1,col_exploratory=2,pre_x_value=mean(y[,col_exploratory]),pre_col="red",reg_col="blue"){
  #library(ggplot2)
  res<-y[,col_response]
  exp<-y[,col_exploratory]
  pre_y_value<-SLR_prediction(y,col_response,col_exploratory,pre_x_value)
  vis_data<-data.frame(res,exp,pre_x_value,pre_y_value)
  ggplot2::ggplot(vis_data,aes(x=exp,y=res))+geom_point()+
    geom_vline(xintercept = pre_x_value,col=pre_col)+
    geom_hline(yintercept = pre_y_value,col=pre_col)+
    geom_smooth(method = "lm",se=F,col=reg_col,alpha=0.6)+
    labs(y="response variable",x="exploratory variable",
         title=paste("For the exploratory value",round(pre_x_value,digits = 3),"the simple regression model predicts the response value:",round(pre_y_value,digits = 3)))
}

#' SLR prediction visualization S3 method for matrices
#'
#' Simple visualization tool for predictions within a simple linear regression model.
#'
#' The generic function first builds a simple linear regression model with one response and one exploratory variable. Then the prediction for a given value of the exploratory variable (default the mean and no out of sample predictions) gets computed.(see \link{SLR_prediction}) Finally using these bits of information a scatterplot of the two variables with the regression line gets returned. Moreover the predicition will be highlighted in the scatterplot and the exact values of the predicition will be displayed above the plot.
#'
#'
#' @param y a numeric \code{matrix} containing the response and exploratory variable
#' @param col_response column number of the response variable within y (default is 1)
#' @param col_exploratory column number of the exploratory variable within y (default is 2)
#' @param pre_x_value the numeric value (within the range of the exploratory vector) for which the prediction is made
#' @param pre_col color of the highlighted prediction within the scatterplot
#' @param reg_col color of the regression line
#'
#' @return a scatterplot of the two variables + regression line + highlighted prediction
#' @export
#' @author Emanuel Sommer
#' @import ggplot2
#'
#'
#' @examples
#' special <- 6
#' Y<-matrix(data = append(1:5,c(1,3,4.5,7,8)),byrow = FALSE,ncol = 2)
#' SLR_prediction_vis(Y,pre_x_value = special)
#' @seealso \link{SLR_prediction}
SLR_prediction_vis.matrix<-function(y,col_response=1,col_exploratory=2,pre_x_value=mean(y[,col_exploratory]),pre_col="red",reg_col="blue"){
  #library(ggplot2)
  res<-y[,col_response]
  exp<-y[,col_exploratory]
  pre_y_value<-SLR_prediction(y,col_response,col_exploratory,pre_x_value)
  vis_data<-data.frame(res,exp,pre_x_value,pre_y_value)
  ggplot2::ggplot(vis_data,aes(x=exp,y=res))+geom_point()+
    geom_vline(xintercept = pre_x_value,col=pre_col)+
    geom_hline(yintercept = pre_y_value,col=pre_col)+
    geom_smooth(method = "lm",se=F,col=reg_col,alpha=0.6)+
    labs(y="response variable",x="exploratory variable",
         title=paste("For the exploratory value",round(pre_x_value,digits = 3),"the simple regression model predicts the response value:",round(pre_y_value,digits = 3)))
}

#methods(prediction_vis)
#prediction_vis(a,b,pre_x_value=9)
