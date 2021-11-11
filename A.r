library(caret)
library(pROC)
library(AppliedPredictiveModeling)
data(hepatic)
##remove problematic variables for 'bio' dataframe
bio_zerovar <- nearZeroVar(bio)
bio_collinear <- findLinearCombos(cov(bio))$remove
bio <- bio[, -c(bio_zerovar, bio_collinear)]
##remove problematic variables for 'chem' dataframe
chem_zerovar <- nearZeroVar(chem)
chem_collinear <- findLinearCombos(chem)$remove
chem <- chem[, -c(chem_zerovar, chem_collinear)]

##Collapse response into "Yes" or "No" values
lut <- c("None" = "No", "Mild" = "No", "Severe" = "Yes")
injury2 <- factor(unname(lut[injury]))


set.seed(1234)

##Partition the data with stratified sampling
training <- createDataPartition(injury2, p = .8, list = FALSE)
##Partition train and test sets for bio data
bio_train <- bio[training, ]
bio_test <- bio[-training, ]
##Partition for train and test sets for chemical data
chem_train <- chem[training, ]
chem_test <- chem[-training, ]
##Partition for the train and test sets for the response variable
injury_train <- injury2[training]
injury_test <- injury2[-training]
## Set training control for model building
ctrl <- trainControl(method = "repeatedcv", 10, repeats = 10, summaryFunction = twoClassSummary, classProbs = TRUE, savePredictions = TRUE)


bio_lda <- train(bio_train, injury_train, method = "lda", trControl = ctrl, metric = "Spec")
chem_lda <- train(chem_train, injury_train, method = "lda", trControl = ctrl, metric = "Spec")
##Generate LDA model predictions for bio indicator test set
bio_lda_pred <- predict(bio_lda, bio_test)
##Generate confusion matrix to show results
confusionMatrix(bio_lda_pred, injury_test, positive = "No")


##Chem predictor LDA model
chem_lda_pred <- predict(chem_lda, chem_test)
##confusion matrix
confusionMatrix(chem_lda_pred, injury_test, positive = "No")




bio_plr <- train(bio_train, injury_train, method = "glmnet", family = "binomial", metric = "Spec", tuneLength = 20,
                 trControl = ctrl)
chem_plr <- train(chem_train, injury_train, method = "glmnet", family = "binomial", metric = "Spec", tuneLength = 20, trControl = ctrl)
bio_plr_pred <- predict(bio_plr, bio_test)

confusionMatrix(bio_plr_pred, injury_test, positive = "No")



chem_plr_pred <- predict(chem_plr, chem_test)
confusionMatrix(chem_plr_pred, injury_test, positive = "No")



bio_pls <- train(bio_train, injury_train, method = "pls", trControl = ctrl, metric = "Spec", tuneLength = 20)

chem_pls <- train(chem_train, injury_train, method = "pls", trControl = ctrl, metric = "Spec", tuneLength = 20)
bio_pls_pred <- predict(bio_pls, bio_test)

confusionMatrix(bio_pls_pred, injury_test, positive = "No")


chem_pls_pred <- predict(chem_pls, chem_test, probability = TRUE)

confusionMatrix(chem_pls_pred, injury_test, positive = "No")

bio_centroid <- train(bio_train, injury_train, method = "pam",
                      
                      trControl = ctrl, preProcess = c("center", "scale"), metric = "Spec", tuneLength = 20)

chem_centroid <- train(chem_train, injury_train, method = "pam",
                       
                       trControl = ctrl, preProcess = c("center", "scale"), metric = "Spec", tuneLength = 20)

bio_centroid_pred <- predict(bio_centroid, bio_test)

chem_centroid_pred <- predict(chem_centroid, chem_test)

confusionMatrix(bio_centroid_pred, injury_test, positive = "No")


confusionMatrix(chem_centroid_pred, injury_test, positive = "No")



predictions_bio_lda <- predict(bio_lda, bio_test, type = "prob")

pROC::plot.roc(injury_test, predictions_bio_lda$Yes)

