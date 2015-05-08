# This app can change values daily:
# Day 1, Month: Jan, River: Galas, Dabong, Wind: False, Rain: No - Very Heavy & Extreme

damcat <- function(daySelect = "1", monthSelect = "January", riverSelect = "Sungai Galas, Dabong", rainSelect = "1", windSelect = "True") {
  dam_prediction <- read.table("./River.csv",header=TRUE,sep=",") 
  dam_prediction$Wind<- recode(dam_prediction$Wind, "0:1.5='False'; 1.6:21.0='True'")
  
  fit <- rpart(Rank_Dam ~ Day + Month + River + Wind + Rainfall ,method="class", data=dam_prediction,control=rpart.control(minsplit=4))
  
  plotdiv <- renderPlot({
    rpart.plot(fit, type=4, extra=1)
  })
  print(plotdiv)
  invisible()
}