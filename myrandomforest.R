library(caret)
library(randomForest)
library(munsell)
set.seed(3)

mtcars$cyl <- factor(mtcars$cyl, labels=c('2C','6C','8C'))

idx <- createDataPartition(mtcars$cyl,p=0.7,list=FALSE)
train <- mtcars[idx,]
test <- mtcars[-idx,]

model <- randomForest(cyl ~ hp + wt + disp + qsec, data=train)

test$pred <- predict(model, test)
confusionMatrix(test$pred, test$cyl)

getTree(model, labelVar = TRUE)


seebycyl <- function(code){
  return(test[test$cyl==code,c('cyl','hp','disp')])
}


results <- predict(model, test, predict.all = TRUE)


