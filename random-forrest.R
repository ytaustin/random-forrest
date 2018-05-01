library(tidyverse)
library(caret)
library(mlbench)
require(randomForest)
data("Glass")
Glass <- as.tibble(Glass)

set.seed(75769)
trainIndex <-
  createDataPartition(Glass$Type,
                      p = 0.8,
                      list = FALSE,
                      times = 1)
GlassTrain <- Glass[trainIndex, ]
GlassTest <- Glass[-trainIndex, ]
scaler <- preProcess(Glass, method = c("center", "scale"))
GlassTrain <- predict(scaler, GlassTrain)
GlassTest <- predict(scaler, GlassTest)

rf <- train(Type ~ .-Fe-Ba-Ca,
            data = GlassTrain,
            method = "rf")
Predictions <-
  predict(rf, GlassTest)
confusionMatrix(Predictions, GlassTest$Type)

print(rf)