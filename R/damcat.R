# This app can change values monthly:
# Day 1, Month: Jan, River: Galas, Dabong, Wind: False, Rain: No - Very Heavy & Extreme

library(car)
library("rpart")
library("rpart.plot")
library("ggplot2")

dam_prediction <- get(load('./data/damitv2.rda'))

dam_prediction$wind <- recode(dam_prediction$wind, "0:1.5='False'; 1.6:21.0='True'")

fit <- rpart(dam ~ month + station + wind + rain + year,method="class", data=dam_prediction,control=rpart.control(minsplit=4))

damplot <- function() {
  rpart.plot(fit, type=4, extra=1)
}

damcategory <- function(monthSelect = "1", riverSelect = "Butterworth", rainSelect = "1", windSelect = "True"){

  newdata <- data.frame(
    Day = as.numeric(daySelect),
    Month = monthSelect,
    River= riverSelect,
    Rain =as.numeric(rainSelect),
    Wind = windSelect)

  return(result = predict(fit,newdata=newdata,type=c("class")))
}
