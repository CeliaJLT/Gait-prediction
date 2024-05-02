

# Loading packages --------------------------------------------------------

require(readxl)
require(tidyverse)
require(tidylog)

require(recipes)

require(caret)
require(doParallel)


load("C:/Users/gabri/OneDrive - University of Manitoba/PostDoc/Collab projects/Celia - Kinematic/nrs_vs_attributes/2023_11_02_nrs_vs_attributes.RData")

# Loading data ------------------------------------------------------------

# Data prep done on excel before loading in R:
# Created a column position2 indicating only left and right body position
# Identified code used for missing values ("", "N/A", and "#N/A")
# Replaced "-" on attribute names to "_"

d1 <- read_excel("data/2023_09_27_CLEANED DATA.xlsx",
                 sheet = "Data_prep_1",
                 na = c("", "N/A", "#N/A"))

summary(d1)
str(d1)


# Not all empty cells are missing data. In some cases, data is just not available
# I used the code 99999 on excel Sheet2 to indicate that. 
# Using that sheet to calculate the number of observations (total and per passage)

d1.counting <- read_excel("data/2023_09_27_CLEANED DATA.xlsx",
                          sheet = "Data_prep_2",
                          na = c("N/A", "#N/A"))


# Missing data exploration
DataExplorer::plot_missing(d1.counting %>% 
                             select(-c(position, position2, ID)))

d1.counting %>%
  select(-position, -position2) %>% 
  summarise_all(~sum(is.na(.)))


d1.counting %>%
  select(-position, -position2) %>% 
  group_by(ID) %>%
  summarise_all(~sum(is.na(.))) %>%
  select(-ID) %>% 
  summary()



# Exploring the distribution of the NRS values


d1 %>% 
  distinct(ID, nrs_score) %>% 
  pull(nrs_score) %>% 
  epiDisplay::tab1()




# Splitting and Imputing Missing ------------------------------------------


## Imputation -------------------------------------------------------------

# Variables are not collected on the same level. Some of them are collected on
# passage level, others on side level in each step, and lastly per leg in each 
# step. The imputation should take that into consideration!

# One of the easiest way would be to re-shape the data to a shape that represents
# the level of variable collection!


### Variables collected on side step level ####

d.ssl <- d1.counting %>% # Using the sheet that differentiate NA and non-existence data
  
  # Selecting variables measured with the same unit (side per step)
  select("ID", "position2", "track_up_disXY",	"track_up_disX") %>% 
  
  # Removing rows with non-existent data
  # Using Excel, I added the code "99999" to cells that had non-existent data
  # If is.na is not used, it will remove those rows automatically!!
  filter(is.na(track_up_disXY) | track_up_disXY < 99999) %>%
  
  # Generating step order variable (Assuming that data is ordered as such)
  # Even if that is not the case, it does not really matter because 
  # the most important thing (hypothesized) is the variation between steps
  # and not the step order in itself
  group_by(ID, position2) %>% 
  mutate(step = as.factor(1:n())) %>% 
  ungroup() %>% 
  
  
  # Replacing missing values using the average of the passage
  # Because the training/validation split will be made per passage, 
  # the imputation can be done before the splitting without the risk of 
  # information leakage
  group_by(ID, position2) %>%
  mutate(track_up_disXY = ifelse(is.na(track_up_disXY),
                                 mean(track_up_disXY, na.rm=TRUE),
                                 track_up_disXY),
         
         track_up_disX = ifelse(is.na(track_up_disX),
                                mean(track_up_disX, na.rm=TRUE),
                                track_up_disX)) %>% 
  ungroup() %>%
  
  
  # Reshape the data to a wider format (each row will the from each step)
  mutate(ID = paste(ID, step, sep = "_")) %>% 
  pivot_wider(id_cols = ID, names_from = position2,
              values_from = c("track_up_disXY",	"track_up_disX")) %>% 
  
  mutate(step = as.factor(substr(ID, start = nchar(ID)-1+1, stop = nchar(ID))),
         ID = substr(ID, start = 1, stop = nchar(ID)-2))


summary(d.ssl)

# The passage "1-20-21-penelope-clip2-copy" does not have data on 
# track_up_disX_left and track_up_disXY_left. I will be removing this 
# observation when splitting in training and validation!!


### Variables collected on leg step level ####
# These are the only variables that have NA!!


d.lsl <- d1.counting %>% # Using the sheet that differentiate NA and non-existence data
  
  # Selecting variables measured with the same unit (hoof per step)
  select(ID, position, stride_length,	stride_time, velocity,	stance_time) %>%
  
  # Generating step order variable (Assuming that data is ordered as such)
  # Even if that is not the case, it does not really matter because 
  # the most important thing (hypothesized) is the variation between steps
  # and not the step order in itself
  group_by(ID, position) %>% 
  mutate(step = as.factor(1:n())) %>% 
  ungroup() %>%
  
  
  # Replacing missing values using the average of the passage
  # Because the training/validation split will be made per passage, 
  # the imputation can be done before the splitting without the risk of 
  # information leakage
  group_by(ID, position) %>%
  mutate(stride_length = ifelse(is.na(stride_length),
                                mean(stride_length, na.rm=TRUE),
                                stride_length),
         
         stride_time = ifelse(is.na(stride_time),
                              mean(stride_time, na.rm=TRUE),
                              stride_time),
         
         velocity = ifelse(is.na(velocity),
                           mean(velocity, na.rm=TRUE),
                           velocity),
         
         stance_time = ifelse(is.na(stance_time),
                              mean(stance_time, na.rm=TRUE),
                              stance_time)) %>% 
  ungroup() %>% 
  
  # There was a problem with the observation from 2_2_21_loza_clip2 
  # (one extra row was added when creating the excel file). Removing that row
  filter(step != 4) %>% 
  
  # Also, the passage "1-28-21-alice-clip1" does not have any value for stride_length in
  # the left-rear and the right-front levels as well as velocity in the left-rear
  # level. Because of that, I will be removing this observation when splitting in
  # training and validation!!
  
  # Reshape the data to a wider format (each row will the from each step)
  mutate(ID = paste(ID, step, sep = "_")) %>% 
  pivot_wider(id_cols = ID, names_from = position,
              values_from = c("stride_length", "stride_time", "velocity",
                              "stance_time")) %>% 
  
  mutate(step = as.factor(substr(ID, start = nchar(ID)-1+1, stop = nchar(ID))),
         ID = substr(ID, start = 1, stop = nchar(ID)-2)) %>% 
  
  # some variable still have NA becasue they originally only had two steps
  # imputing the missing data with the average of the both steps
  group_by(ID) %>% 
  mutate(stride_length_left_rear = ifelse(is.na(stride_length_left_rear),
                                          mean(stride_length_left_rear, na.rm=TRUE),
                                          stride_length_left_rear),
         
         stride_time_left_rear = ifelse(is.na(stride_time_left_rear),
                                        mean(stride_time_left_rear, na.rm=TRUE),
                                        stride_time_left_rear),
         
         velocity_left_rear = ifelse(is.na(velocity_left_rear),
                                     mean(velocity_left_rear, na.rm=TRUE),
                                     velocity_left_rear),
         
         stance_time_left_rear = ifelse(is.na(stance_time_left_rear),
                                        mean(stance_time_left_rear, na.rm=TRUE),
                                        stance_time_left_rear)) %>% 
  
  ungroup()





summary(d.lsl)


### Variables collected on side passage level ####

d.spl <- d1.counting %>% # Using the sheet that differentiate NA and non-existence data
  
  # Selecting variables measured with the same unit (side per passage)
  select("ID", "position", "average_angle") %>% 
  
  # Removing rows with non-existent data
  # Using Excel, I added the code "99999" to cells that had non-existent data
  filter(average_angle != 99999) %>%
  distinct(ID, position, average_angle) %>% 
  
  # Reshape the data to a wider format (each row will the from each step)
  pivot_wider(id_cols = ID, names_from = position,
              values_from = average_angle,
              names_prefix = 'average_angle_')

summary(d.spl)



# Final data set!

d.final <- d.ssl %>% 
  left_join(d.lsl, by = c("ID", "step")) %>% 
  left_join(d.spl, by = "ID") %>% 
  left_join(d1 %>% 
              select(ID, nrs_score) %>% 
              distinct(ID, .keep_all = TRUE), by = "ID") %>% 
  
  # The passages "1-28-21-alice-clip1" does not have any value for stride_length in
  # the left-rear and the right-front levels as well as velocity in the left-rear
  # level. The passage "1-20-21-penelope-clip2-copy" does not have data on 
  # track_up_disX_left and track_up_disXY_left. Removing it too!  
  filter(ID != "1_28_21_alice_clip1") %>% 
  filter(ID != "1_20_21_penelope_clip2_copy") %>% 
  
  # There only one NRS = 1.5 and two NRS = 4. Dropping those observations
  filter(!(nrs_score %in% c(1.5, 4)))



# Splitting the data ####

d1_split_reference <- d.final %>% 
  select(ID, nrs_score) %>% 
  distinct(ID, .keep_all = TRUE)

splitIndex.d1 <- splitTools::partition(d1_split_reference$nrs_score,
                                       p = c(train = 0.70, valid = 0.30),
                                       seed = 1801)



d.train <- d.final %>% 
  filter(ID %in% d1_split_reference[splitIndex.d1$train,]$ID) %>%
  mutate(nrs_score = factor(paste("nrs_score", nrs_score, sep = "_")))


d.train %>% 
  pull(nrs_score) %>% 
  epiDisplay::tab1(main = "Distribution NRS - Training set")



d.valid <- d.final %>% 
  filter(ID %in% d1_split_reference[splitIndex.d1$valid,]$ID) %>% 
  mutate(nrs_score = factor(paste("nrs_score", nrs_score, sep = "_")))

         
d.valid %>% 
  pull(nrs_score) %>% 
  epiDisplay::tab1(main = "Distribution NRS - Validation set")       
         
         


# Creating F1 score function to train model!


f1_score <- function(predicted, expected, positive.class="1") {
  predicted <- as.factor(predicted)
  expected  <- as.factor(expected)
  cm = as.matrix(table(expected, predicted))

  precision <- diag(cm) / colSums(cm)
  recall <- diag(cm) / rowSums(cm)
  f1 <-  ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))

  #Assuming that F1 is zero when it's not possible compute it
  f1[is.na(f1)] <- 0

  #Binary F1 or Multi-class macro-averaged F1
  ifelse(nlevels(expected) == 2, f1[positive.class], mean(f1))
}



f1 <- function(data, lev = NULL, model = NULL) {

  f1_score <- function(predicted, expected, positive.class="1") {
    predicted <- as.factor(predicted)
    expected  <- as.factor(expected)
    cm = as.matrix(table(expected, predicted))

    precision <- diag(cm) / colSums(cm)
    recall <- diag(cm) / rowSums(cm)
    f1 <-  ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))

    #Assuming that F1 is zero when it's not possible compute it
    f1[is.na(f1)] <- 0

    #Binary F1 or Multi-class macro-averaged F1
    ifelse(nlevels(expected) == 2, f1[positive.class], mean(f1))
  }

  f1_val <- f1_score(data$pred,data$obs)
  names(f1_val) <- c("F1")
  f1_val
}



# Setting training 
# Leave one out cross validation


set.seed(1801)
fit_control <- trainControl(method = "LOOCV",
                            search = "grid",
                            summaryFunction = f1,
                            allowParallel = TRUE,
                            verboseIter = TRUE,
                            classProbs = TRUE)



cl <- makePSOCKcluster(16)
registerDoParallel(cl, cores = 16)

getDoParWorkers()


## RPART: Recursive partitioning tree
set.seed(1801)
rpart <- train(nrs_score ~ .,
               data = d.train %>%
                 select(-ID, -step),
               method = "rpart2",
               metric = "F1",
               trControl = fit_control,
               tuneLength = 5)


## Gradient boosting machine
set.seed(1801)
gbm <- train(nrs_score ~ .,
             data = d.train %>%
               select(-ID, -step),
             method = "gbm",
             metric = "F1",
             trControl = fit_control,
             tuneLength = 5)


## Extreme gradient boosting machine
set.seed(1801)
xgbm <- train(nrs_score ~ .,
              data = d.train %>%
                select(-ID, -step),
              method = "xgbTree",
              metric = "F1",
              trControl = fit_control,
              tuneLength = 5)



## Random Forest
set.seed(1801)
rf <- train(nrs_score ~ .,
            data = d.train %>%
              select(-ID, -step),
            method = "rf",
            metric = "F1",
            trControl = fit_control,
            tuneLength = 5)



## Support vector machine
set.seed(1801)
svm.radial <- train(nrs_score ~ .,
            data = d.train %>%
              select(-ID, -step),
            method = "svmRadial",
            metric = "F1",
            trControl = fit_control,
            tuneLength = 5)



stopCluster(cl)



# Evaluating the models ####

## Training ####

### Decision tree ####

confusionMatrix(data = predict(rpart, d.train),
                reference = d.train$nrs_score,
                mode = "everything")$overall[c("Accuracy",
                                               "AccuracyLower",
                                               "AccuracyUpper")] %>%
  round(2)

f1_score(predicted = predict(rpart, d.train),
         expected = d.train$nrs_score)

confusionMatrix(data = predict(rpart, d.train),
                reference = d.train$nrs_score,
                mode = "everything")$byClass[,c("Sensitivity",
                                                "Specificity",
                                                "F1",
                                                "Balanced Accuracy")] %>%
  round(2)



### Gradient boosting machine ####

confusionMatrix(data = predict(gbm, d.train),
                reference = d.train$nrs_score,
                mode = "everything")$overall[c("Accuracy",
                                               "AccuracyLower",
                                               "AccuracyUpper")] %>%
  round(2)

f1_score(predicted = predict(gbm, d.train),
         expected = d.train$nrs_score)

confusionMatrix(data = predict(gbm, d.train),
                reference = d.train$nrs_score,
                mode = "everything")$byClass[,c("Sensitivity",
                                                "Specificity",
                                                "F1",
                                                "Balanced Accuracy")] %>%
  round(2)



### Extreme gradient boosting machine ####

confusionMatrix(data = predict(xgbm, d.train),
                reference = d.train$nrs_score,
                mode = "everything")$overall[c("Accuracy",
                                               "AccuracyLower",
                                               "AccuracyUpper")] %>%
  round(2)

f1_score(predicted = predict(xgbm, d.train),
         expected = d.train$nrs_score)

confusionMatrix(data = predict(xgbm, d.train),
                reference = d.train$nrs_score,
                mode = "everything")$byClass[,c("Sensitivity",
                                                "Specificity",
                                                "F1",
                                                "Balanced Accuracy")] %>%
  round(2)



### Random forest ####

confusionMatrix(data = predict(rf, d.train),
                reference = d.train$nrs_score,
                mode = "everything")$overall[c("Accuracy",
                                               "AccuracyLower",
                                               "AccuracyUpper")] %>%
  round(2)

f1_score(predicted = predict(rf, d.train),
         expected = d.train$nrs_score)

confusionMatrix(data = predict(rf, d.train),
                reference = d.train$nrs_score,
                mode = "everything")$byClass[,c("Sensitivity",
                                                "Specificity",
                                                "F1",
                                                "Balanced Accuracy")] %>%
  round(2)



### Support vector machine - Radial ####

confusionMatrix(data = predict(svm.radial, d.train),
                reference = d.train$nrs_score,
                mode = "everything")$overall[c("Accuracy",
                                               "AccuracyLower",
                                               "AccuracyUpper")] %>%
  round(2)

f1_score(predicted = predict(svm.radial, d.train),
         expected = d.train$nrs_score)

confusionMatrix(data = predict(svm.radial, d.train),
                reference = d.train$nrs_score,
                mode = "everything")$byClass[,c("Sensitivity",
                                                "Specificity",
                                                "F1",
                                                "Balanced Accuracy")] %>%
  round(2)






## Validation ####

### Decision tree ####

confusionMatrix(data = predict(rpart, d.valid),
                reference = d.valid$nrs_score,
                mode = "everything")$overall[c("Accuracy",
                                               "AccuracyLower",
                                               "AccuracyUpper")] %>%
  round(2)

f1_score(predicted = predict(rpart, d.valid),
         expected = d.valid$nrs_score)

confusionMatrix(data = predict(rpart, d.valid),
                reference = d.valid$nrs_score,
                mode = "everything")$byClass[,c("Sensitivity",
                                                "Specificity",
                                                "F1",
                                                "Balanced Accuracy")] %>%
  round(2)


require(rattle)

# jpeg("RPART_MODEL.jpg", width = 10, height = 10,
#      units = "cm", quality = 100, res = 600)

fancyRpartPlot(rpart$finalModel, 
               type = 0,
               # fallen.leaves=FALSE,
               clip.right.labs=FALSE)

# dev.off()



### Gradient boosting machine ####

confusionMatrix(data = predict(gbm, d.valid),
                reference = d.valid$nrs_score,
                mode = "everything")$overall[c("Accuracy",
                                               "AccuracyLower",
                                               "AccuracyUpper")] %>%
  round(2)

f1_score(predicted = predict(gbm, d.valid),
         expected = d.valid$nrs_score)

confusionMatrix(data = predict(gbm, d.valid),
                reference = d.valid$nrs_score,
                mode = "everything")$byClass[,c("Sensitivity",
                                                "Specificity",
                                                "F1",
                                                "Balanced Accuracy")] %>%
  round(2)



### Extreme gradient boosting machine ####

confusionMatrix(data = predict(xgbm, d.valid),
                reference = d.valid$nrs_score,
                mode = "everything")$overall[c("Accuracy",
                                               "AccuracyLower",
                                               "AccuracyUpper")] %>%
  round(2)

f1_score(predicted = predict(xgbm, d.valid),
         expected = d.valid$nrs_score)

confusionMatrix(data = predict(xgbm, d.valid),
                reference = d.valid$nrs_score,
                mode = "everything")$byClass[,c("Sensitivity",
                                                "Specificity",
                                                "F1",
                                                "Balanced Accuracy")] %>%
  round(2)



### Random forest ####

confusionMatrix(data = predict(rf, d.valid),
                reference = d.valid$nrs_score,
                mode = "everything")$overall[c("Accuracy",
                                               "AccuracyLower",
                                               "AccuracyUpper")] %>%
  round(2)

f1_score(predicted = predict(rf, d.valid),
         expected = d.valid$nrs_score)

confusionMatrix(data = predict(rf, d.valid),
                reference = d.valid$nrs_score,
                mode = "everything")$byClass[,c("Sensitivity",
                                                "Specificity",
                                                "F1",
                                                "Balanced Accuracy")] %>%
  round(2)



### Support vector machine - Radial ####

confusionMatrix(data = predict(svm.radial, d.valid),
                reference = d.valid$nrs_score,
                mode = "everything")$overall[c("Accuracy",
                                               "AccuracyLower",
                                               "AccuracyUpper")] %>%
  round(2)

f1_score(predicted = predict(svm.radial, d.valid),
         expected = d.valid$nrs_score)

confusionMatrix(data = predict(svm.radial, d.valid),
                reference = d.valid$nrs_score,
                mode = "everything")$byClass[,c("Sensitivity",
                                                "Specificity",
                                                "F1",
                                                "Balanced Accuracy")] %>%
  round(2)




# Weighted regression models ----------------------------------------------


weights <- d.train %>% 
  left_join(d.train %>%
              group_by(nrs_score) %>% 
              summarise(n = n()) %>% 
              mutate(weights = 1/n) %>% 
              select(-n),
            by = "nrs_score") %>% 
  pull(weights)




cl <- makePSOCKcluster(16)
registerDoParallel(cl, cores = 16)

getDoParWorkers()


## RPART: Recursive partitioning tree
set.seed(1801)
rpart.wgt <- train(nrs_score ~ .,
               data = d.train %>%
                 select(-ID, -step),
               method = "rpart2",
               metric = "F1",
               weights = weights,
               trControl = fit_control,
               tuneLength = 5)


## Gradient boosting machine
set.seed(1801)
gbm.wgt <- train(nrs_score ~ .,
             data = d.train %>%
               select(-ID, -step),
             method = "gbm",
             metric = "F1",
             weights = weights,
             trControl = fit_control,
             tuneLength = 5)


## Extreme gradient boosting machine
set.seed(1801)
xgbm.wgt <- train(nrs_score ~ .,
              data = d.train %>%
                select(-ID, -step),
              method = "xgbTree",
              metric = "F1",
              weights = weights,
              trControl = fit_control,
              tuneLength = 5)



## Random Forest
set.seed(1801)
rf.wgt <- train(nrs_score ~ .,
            data = d.train %>%
              select(-ID, -step),
            method = "rf",
            metric = "F1",
            weights = weights,
            trControl = fit_control,
            tuneLength = 5)



## Support vector machine
set.seed(1801)
svm.radial.wgt <- train(nrs_score ~ .,
                    data = d.train %>%
                      select(-ID, -step),
                    method = "svmRadial",
                    metric = "F1",
                    weights = weights,
                    trControl = fit_control,
                    tuneLength = 5)



stopCluster(cl)



# Evaluating the models ####

## Training ####

### Decision tree ####

confusionMatrix(data = predict(rpart.wgt, d.train),
                reference = d.train$nrs_score,
                mode = "everything")$overall[c("Accuracy",
                                               "AccuracyLower",
                                               "AccuracyUpper")] %>%
  round(2)

f1_score(predicted = predict(rpart.wgt, d.train),
         expected = d.train$nrs_score)

confusionMatrix(data = predict(rpart.wgt, d.train),
                reference = d.train$nrs_score,
                mode = "everything")$byClass[,c("Sensitivity",
                                                "Specificity",
                                                "F1",
                                                "Balanced Accuracy")] %>%
  round(2)



### Gradient boosting machine ####

confusionMatrix(data = predict(gbm.wgt, d.train),
                reference = d.train$nrs_score,
                mode = "everything")$overall[c("Accuracy",
                                               "AccuracyLower",
                                               "AccuracyUpper")] %>%
  round(2)

f1_score(predicted = predict(gbm.wgt, d.train),
         expected = d.train$nrs_score)

confusionMatrix(data = predict(gbm.wgt, d.train),
                reference = d.train$nrs_score,
                mode = "everything")$byClass[,c("Sensitivity",
                                                "Specificity",
                                                "F1",
                                                "Balanced Accuracy")] %>%
  round(2)



### Extreme gradient boosting machine ####

confusionMatrix(data = predict(xgbm.wgt, d.train),
                reference = d.train$nrs_score,
                mode = "everything")$overall[c("Accuracy",
                                               "AccuracyLower",
                                               "AccuracyUpper")] %>%
  round(2)

f1_score(predicted = predict(xgbm.wgt, d.train),
         expected = d.train$nrs_score)

confusionMatrix(data = predict(xgbm.wgt, d.train),
                reference = d.train$nrs_score,
                mode = "everything")$byClass[,c("Sensitivity",
                                                "Specificity",
                                                "F1",
                                                "Balanced Accuracy")] %>%
  round(2)



### Random forest ####

confusionMatrix(data = predict(rf.wgt, d.train),
                reference = d.train$nrs_score,
                mode = "everything")$overall[c("Accuracy",
                                               "AccuracyLower",
                                               "AccuracyUpper")] %>%
  round(2)

f1_score(predicted = predict(rf.wgt, d.train),
         expected = d.train$nrs_score)

confusionMatrix(data = predict(rf.wgt, d.train),
                reference = d.train$nrs_score,
                mode = "everything")$byClass[,c("Sensitivity",
                                                "Specificity",
                                                "F1",
                                                "Balanced Accuracy")] %>%
  round(2)



### Support vector machine - Radial ####

confusionMatrix(data = predict(svm.radial.wgt, d.train),
                reference = d.train$nrs_score,
                mode = "everything")$overall[c("Accuracy",
                                               "AccuracyLower",
                                               "AccuracyUpper")] %>%
  round(2)

f1_score(predicted = predict(svm.radial.wgt, d.train),
         expected = d.train$nrs_score)

confusionMatrix(data = predict(svm.radial.wgt, d.train),
                reference = d.train$nrs_score,
                mode = "everything")$byClass[,c("Sensitivity",
                                                "Specificity",
                                                "F1",
                                                "Balanced Accuracy")] %>%
  round(2)






## Validation ####

### Decision tree ####

confusionMatrix(data = predict(rpart.wgt, d.valid),
                reference = d.valid$nrs_score,
                mode = "everything")$overall[c("Accuracy",
                                               "AccuracyLower",
                                               "AccuracyUpper")] %>%
  round(2)

f1_score(predicted = predict(rpart.wgt, d.valid),
         expected = d.valid$nrs_score)

require(rattle)

# jpeg("RPART_MODEL.jpg", width = 10, height = 10,
#      units = "cm", quality = 100, res = 600)

fancyRpartPlot(rpart.wgt$finalModel, 
               type = 0,
               # fallen.leaves=FALSE,
               clip.right.labs=FALSE)

# dev.off()



confusionMatrix(data = predict(rpart.wgt, d.valid),
                reference = d.valid$nrs_score,
                mode = "everything")$byClass[,c("Sensitivity",
                                                "Specificity",
                                                "F1",
                                                "Balanced Accuracy")] %>%
  round(2)



### Gradient boosting machine ####

confusionMatrix(data = predict(gbm.wgt, d.valid),
                reference = d.valid$nrs_score,
                mode = "everything")$overall[c("Accuracy",
                                               "AccuracyLower",
                                               "AccuracyUpper")] %>%
  round(2)

f1_score(predicted = predict(gbm.wgt, d.valid),
         expected = d.valid$nrs_score)

confusionMatrix(data = predict(gbm.wgt, d.valid),
                reference = d.valid$nrs_score,
                mode = "everything")$byClass[,c("Sensitivity",
                                                "Specificity",
                                                "F1",
                                                "Balanced Accuracy")] %>%
  round(2)



### Extreme gradient boosting machine ####

confusionMatrix(data = predict(xgbm.wgt, d.valid),
                reference = d.valid$nrs_score,
                mode = "everything")$overall[c("Accuracy",
                                               "AccuracyLower",
                                               "AccuracyUpper")] %>%
  round(2)

f1_score(predicted = predict(xgbm.wgt, d.valid),
         expected = d.valid$nrs_score)

confusionMatrix(data = predict(xgbm.wgt, d.valid),
                reference = d.valid$nrs_score,
                mode = "everything")$byClass[,c("Sensitivity",
                                                "Specificity",
                                                "F1",
                                                "Balanced Accuracy")] %>%
  round(2)



### Random forest ####

confusionMatrix(data = predict(rf.wgt, d.valid),
                reference = d.valid$nrs_score,
                mode = "everything")$overall[c("Accuracy",
                                               "AccuracyLower",
                                               "AccuracyUpper")] %>%
  round(2)

f1_score(predicted = predict(rf.wgt, d.valid),
         expected = d.valid$nrs_score)

confusionMatrix(data = predict(rf.wgt, d.valid),
                reference = d.valid$nrs_score,
                mode = "everything")$byClass[,c("Sensitivity",
                                                "Specificity",
                                                "F1",
                                                "Balanced Accuracy")] %>%
  round(2)



### Support vector machine - Radial ####

# Does not have a weighted version!



save.image("C:/Users/gabri/OneDrive - University of Manitoba/PostDoc/Collab projects/Celia - Kinematic/nrs_vs_attributes/2023_11_02_nrs_vs_attributes.RData")
