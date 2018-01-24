library(plotly)
library(data.table)
library(tidyverse)
library(randomForest)
library(caret)
library(FactoMineR)
library(pbapply)

source("src/process_coord.R")
source("src/process_feat_engi.R")
source("src/process_xgb.R")

#### params ####
soumission <- 0

#### read data ####
input_train <- read_csv("data/train/train.csv")
coord_train <- process_coord("data/train/spatial")

#### feature engineering ####
data_train <- feat_engi(input_train, coord_train)

#### train ####
if (soumission) {
  train <- data_train
  input_test <- read_csv("data/test/test.csv")
  coord_test <- process_coord("data/test/spatial")
  
  test <- feat_engi(input_test, coord_test)
} else {
  train_idx <- sample(nrow(data_train), 0.75 * nrow(data_train))
  train <- data_train[train_idx,]
  
  test <- data_train[-train_idx,]
}

#### randomForest formation ####
train_formation <- train %>%
  select(-id, -bandgap_energy_ev) 

set.seed(546)
randomForest_formation <- randomForest(formation_energy_ev_natom ~ ., data = train_formation, ntree = 500)
#svm_formation <- svm(formation_energy_ev_natom ~ ., data = train_formation)

pred_formation <- predict(randomForest_formation, test)
#pred_formation <- predict(svm_formation, test)

#### randomForest bandgap ####
train_bandgap <- train %>%
  select(-id, -formation_energy_ev_natom) 
# %>% 
#   bind_cols(data.frame(pred_formation = randomForest_formation$predicted))
# 
# test_bandgap <- test %>%
#   bind_cols(data.frame(pred_formation = pred_formation))

set.seed(546)
randomForest_bandgap <- randomForest(bandgap_energy_ev ~ ., data = train_bandgap, ntree = 500)
#svm_bandgap <- svm(bandgap_energy_ev ~ ., data = train_bandgap)

pred_bandgap <- predict(randomForest_bandgap, test)
#pred_bandgap <- predict(svm_bandgap, test)

#### results ####
pred <- data.frame(pred_formation, pred_bandgap)

RMSLE_formation <- sqrt(sum((log(pred$pred_formation+1)-log(test$formation_energy_ev_natom+1))**2)/length(pred$pred_formation))
RMSLE_bandgap <- sqrt(sum((log(pred$pred_bandgap+1)-log(test$bandgap_energy_ev+1))**2)/length(pred$pred_bandgap))

RMSLE = (RMSLE_formation + RMSLE_bandgap) / 2

imp_formation <- data.frame(feat = row.names(randomForest_formation$importance), importance = randomForest_formation$importance, row.names = 1:length(randomForest_formation$importance))
imp_bandgap <- data.frame(feat = row.names(randomForest_bandgap$importance), importance = randomForest_bandgap$importance, row.names = 1:length(randomForest_bandgap$importance))

if (soumission) {
  output <- pred %>% 
    mutate(id = row.names(.)) %>% 
    select(id, pred_formation, pred_bandgap) %>% 
    rename(formation_energy_ev_natom = pred_formation, bandgap_energy_ev = pred_bandgap)
  
  write_csv(output, path = "output/pred.csv")
}
