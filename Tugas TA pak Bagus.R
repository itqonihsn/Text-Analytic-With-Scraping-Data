library(tibble)
library(tidyverse)
library(ggplot2)
library(mlr) # Machine learning with r package

getwd()
setwd("C:/Users/itqon/OneDrive/Documents/S2/Semester 2/TA/UAS")

df <- read.csv("data tugas text analytics.csv",stringsAsFactors = TRUE)
head(df)
glimpse(df)
#df_tibble<-as_tibble(df)
#view(head(df_tibble))

#colnames(df_tibble)
#glimpse(df_tibble$`Rating Class`)

dim(df)
names(df)
class(df)
summary(df)


df$Rating.Class = factor(df$Rating.Class)
levels(df$Rating.Class)=c("Yes","No")

df$Trip.Type = factor(df$Trip.Type)
class(df$ac)
class(df$Trip.Type)
class(df$Rating.Class)
summary(df$ac)
summary(df$Rating.Class)
summary(df$Trip.Type)

df[2:251] <- lapply(df[2:251], as.numeric)

library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(skimr)

v <- sort(colSums(df[2:251]),decreasing=TRUE)
head(v,20)

words <- names(v)
d <- data.frame(word=words, freq=v)
wordcloud(d$word,d$freq,min.freq=1000,colors=brewer.pal(8, "Dark2"))

barplot(d[1:20,]$freq, las = 2, names.arg = d[1:20,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

df[2:251] <- lapply(df[2:251], as.factor)
skim(df)

library(randomForest)
library(mlbench)
library(caret)

set.seed(123)
train_index <- createDataPartition(df$Rating.Class, p = 0.8, list = FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

tune_grid <- expand.grid(mtry = seq(2,10, by=2))

# Define cross-validation method and performance metric
ctrl <- trainControl(
  method = "cv", # 10-fold cross-validation
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = TRUE
)

# Train the model with Random Search
rf_model <- train(Rating.Class ~ ., data = train_data, method = "rf",
                  tuneGrid = tune_grid, trControl = trainControl(method = "cv", number = 10),
                  ntree = 500, importance = TRUE)

# Train the model using the tuning grid and the defined trainControl
rf_model <- train(
  Rating.Class~.,
  data = train_data,
  method = "rf",
  trControl = ctrl,
  tuneGrid = tune_grid,
  metric = "Accuracy",
  ntree = 1000
)

best_params <- rf_model$bestTune
best_params

# Refit the model using the best parameter values
final_model <- randomForest(
  Rating.Class ~ .,
  data = train_data,
  ntree = best_params$ntree,
  mtry = best_params$mtry
)

# Predict on the test set
predictions <- predict(final_model, newdata = test_data)

# Evaluate performance
confusionMatrix(predictions, test_data$Rating.Class)


control <- trainControl(method="cv", number=5)
tunegrid <- expand.grid(.mtry = 6:12)
set.seed(123)
custom <- train(Rating.Class~., 
                data=train_data, method="rf", 
                metric="rmse", 
                tuneGrid=tunegrid, 
                ntree = 1000,
                trControl=control)
