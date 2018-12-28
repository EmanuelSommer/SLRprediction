
SLR_prediction<-function(y,...){UseMethod("SLR_prediction",y)}


SLR_prediction.default<-function(y){"The function is not defined for this class"}

SLR_prediction.numeric<-function(y,exploratory,pre_x_value=mean(exploratory)){
  if(pre_x_value>max(exploratory)|pre_x_value<min(exploratory)){
    stop("prediction out of sample")}
  model<-lm(y~exploratory)
  pre_y_value<-predict(model,data.frame(exploratory=pre_x_value),
                       interval="prediction")[1]
  pre_y_value
}

SLR_prediction.data.frame<-function(y,col_response=1,col_exploratory=2,
                                pre_x_value=mean(y[,col_exploratory])){
  res<-y[,col_response]
  exp<-y[,col_exploratory]
  if(pre_x_value>max(exp)|pre_x_value<min(exp)){
    stop("prediction out of sample")}
  model<-lm(res~exp)
  pre_y_value<-predict(model,data.frame(exp=pre_x_value),
                       interval="prediction")[1]
  pre_y_value
}

SLR_prediction.matrix<-function(y,col_response=1,col_exploratory=2,
                                pre_x_value=mean(y[,col_exploratory])){
  res<-y[,col_response]
  exp<-y[,col_exploratory]
  if(pre_x_value>max(exp)|pre_x_value<min(exp)){
    stop("prediction out of sample")}
  model<-lm(res~exp)
  pre_y_value<-predict(model,data.frame(exp=pre_x_value),
                       interval="prediction")[1]
  pre_y_value
}

#methods(SLR_prediction)

