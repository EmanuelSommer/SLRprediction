SLR_prediction_vis<-function(y,...){UseMethod("SLR_prediction_vis")}

SLR_prediction_vis.default<-function(y){"The function is not defined for this class"}

SLR_prediction_vis.numeric<-function(y,exploratory,pre_x_value=mean(exploratory)){
  library(ggplot2)
  pre_y_value<-SLR_prediction(y,exploratory,pre_x_value)
  vis_data<-data.frame(y,exploratory,pre_x_value,pre_y_value)
  ggplot(vis_data,aes(x=exploratory,y=y))+geom_point()+
    geom_vline(xintercept = pre_x_value,col="red")+
    geom_hline(yintercept = pre_y_value,col="red")+
    geom_smooth(method = "lm",se=F,col="blue",alpha=0.6)+
    labs(y="response variable",x="exploratory variable",
         title=paste("For the exploratory value",round(pre_x_value,digits = 3),"the simple regression model predicts the response value:",round(pre_y_value,digits = 3)))
}

SLR_prediction_vis.data.frame<-function(y,col_response=1,col_exploratory=2,pre_x_value=mean(y[,col_exploratory])){
  library(ggplot2)
  res<-y[,col_response]
  exp<-y[,col_exploratory]
  pre_y_value<-SLR_prediction(y,col_response,col_exploratory,pre_x_value)
  vis_data<-data.frame(res,exp,pre_x_value,pre_y_value)
  ggplot(vis_data,aes(x=exp,y=res))+geom_point()+
    geom_vline(xintercept = pre_x_value,col="red")+
    geom_hline(yintercept = pre_y_value,col="red")+
    geom_smooth(method = "lm",se=F,col="blue",alpha=0.6)+
    labs(y="response variable",x="exploratory variable",
         title=paste("For the exploratory value",round(pre_x_value,digits = 3),"the simple regression model predicts the response value:",round(pre_y_value,digits = 3)))
}

SLR_prediction_vis.matrix<-function(y,col_response=1,col_exploratory=2,pre_x_value=mean(y[,col_exploratory])){
  library(ggplot2)
  res<-y[,col_response]
  exp<-y[,col_exploratory]
  pre_y_value<-SLR_prediction(y,col_response,col_exploratory,pre_x_value)
  vis_data<-data.frame(res,exp,pre_x_value,pre_y_value)
  ggplot(vis_data,aes(x=exp,y=res))+geom_point()+
    geom_vline(xintercept = pre_x_value,col="red")+
    geom_hline(yintercept = pre_y_value,col="red")+
    geom_smooth(method = "lm",se=F,col="blue",alpha=0.6)+
    labs(y="response variable",x="exploratory variable",
         title=paste("For the exploratory value",round(pre_x_value,digits = 3),"the simple regression model predicts the response value:",round(pre_y_value,digits = 3)))
}

#methods(prediction_vis)
#prediction_vis(a,b,pre_x_value=9)
