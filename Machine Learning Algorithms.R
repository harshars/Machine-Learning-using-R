
missing <- as.data.frame(apply(data,2,function(x){sum(is.na(x))/nrow(data)}))

write.csv(missing,"missing_new.csv")

#Linear Regression

model <- lm(revised_net_salary~.,data = data_final1)

summary(model)

library(car)
vif_values <- as.data.frame(vif(model))

vif_values <- vif_values[order(vif_values$GVIF,decreasing = T),]

vif_values


#Step wise Regression
library(MASS)
null <- lm(revised_net_salary~1,data = data_final1)
full <- lm(revised_net_salary~.,data = data_final1)

step(null,scope=list(lower=null,upper=full),direction="forward")


summary(model)


# Decision tree
library(rpart)
tree.train = rpart(revised_net_salary ~.,data=data_final1)


summary(tree.train)

library(rattle)
fancyRpartPlot(tree.train)


names(tree.train)

temp <- as.data.frame(tree.train$variable.importance)


#Random Forest
library(randomForest)
# First cut
tree.random = randomForest(revised_net_salary ~ .,data=data_final1,ntree=10,na.action = na.omit)


mportance(tree.random)

library(caret)
varImp(tree.random)


# GBM
library(caret)


fit <- train(revised_net_salary ~ ., data = data_final1, method = "gbm",verbose = FALSE)


## Xg Boost

fit_xg <- train(revised_net_salary ~ ., data = data_final1,method="xgbTree")


## SVM

library(e1071)


fit <-svm(revised_net_salary ~ ., data = data_final1)


## XGB with hyper parameter tuning

set.seed(1234)

# set up the cross-validated hyper-parameter search
xgb_grid_1 = expand.grid(
  nrounds = 500,
  eta = c(0.01),
  max_depth = c(6, 8),
  gamma = 1,
  colsample_bytree = 1,
  min_child_weight = 100,
  subsample = 1
)

# pack the training control parameters
xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  classProbs = FALSE,                                                           # set to TRUE for AUC to be computed
  allowParallel = TRUE
)

xgb_train_1 = train(
  x = as.matrix(final_train_data %>%
                  select(-Quantity,-qty_lag5)),
  y = final_train_data$Quantity,
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  method = "xgbTree"
)




## XGB with Parallel Computation

library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)


start_time <- Sys.time()
set.seed(1234)

# set up the cross-validated hyper-parameter search
xgb_grid_1 = expand.grid(
  nrounds = c(500,1000),
  eta = c(0.01,0.05),
  max_depth = c(6, 8),
  gamma = c(0.6,0.8,1),
  colsample_bytree = c(0.6,0.8,1),
  min_child_weight = 100,
  subsample = c(0.6,0.8,1)
)

# pack the training control parameters
xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  classProbs = FALSE,                                                           # set to TRUE for AUC to be computed
  allowParallel = TRUE
)

# train the model for each parameter combination in the grid,
#   using CV to evaluate
xgb_train_1 = train(
  x = as.matrix(final_train_data %>%
                  select(-Quantity,-qty_lag5)),
  y = final_train_data$Quantity,
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  method = "xgbTree"
)

# Predict

p <- predict(object = xgb_train_1,final_test_data)
final_output <- cbind(test_data[,"ModelCode"],final_test_data,p)
write.csv(final_output,"maharashtra_parallel.csv")
end_time <- Sys.time()

end_time - start_time

stopCluster(cluster)
registerDoSEQ()


