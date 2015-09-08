library(car)
library("rpart")
library("rpart.plot")
library("ggplot2")

rain_prediction <- get(load('./data/River.rda'))

rain_prediction$Wind<- recode(rain_prediction$Wind, "0:1.5='False'; 1.6:21.0='True'")
rain_prediction$Rank_Dam <- recode(rain_prediction$Rank_Dam, "0:20='Dry'; 21:60='Critical Low'; 61:80='Average'; 81:99='Critical High';else='Overflow'")

raincategory <- function(monthSelect = "1", riverSelect = "Butterworth", rainSelect = "Dry", windSelect = "True"){
  
fit <- rpart(Rain ~ Month + River + Wind + Rank_Dam,method="class", data=rain_prediction,control=rpart.control(minsplit=4))

rainplot <- function() {
  rpart.plot(fit, type=4, extra=1)
}
  
  newdata <- data.frame(    
    Month = as.numeric(monthSelect),
    River= riverSelect,
    Rank_Dam = rainSelect,
    Wind = windSelect)
  
  return(result = predict(fit,newdata=newdata,type=c("class")))
}
