distPlot <- renderPlot({
    
  dam_prediction <- read.table("./River.csv",header=TRUE,sep=",")
  dam_prediction$Wind<- recode(dam_prediction$Wind, "0:1.5='False'; 1.6:21.0='True'")
  
  fit <- rpart(Rank_Dam ~ Day + Month + River + Wind + Rainfall ,method="class", data=dam_prediction,control=rpart.control(minsplit=4))

  rpart.plot(fit, type=4, extra=1)
})