if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, mice, e1071, Metrics, skimr, pracma, knitr, ggplot2, plyr, dplyr, corrplot, caret, gridExtra, scales, Rmisc, ggrepel, randomForest, psych, xgboost)

train <- read.csv("https://www.dropbox.com/s/pniwqrjk5nmhupp/train.csv?dl=1", stringsAsFactors = F)
test <- read.csv("https://www.dropbox.com/s/bxj4sgves9f5g4y/test.csv?dl=1", stringsAsFactors = F)
full <- bind_rows(train,test)

full %>% skim()
SalePrice <- train$SalePrice
Id <- test$Id

full[,c('Id','SalePrice')] <- NULL
rm(train,test)

chr <- full[,sapply(full,is.character)]
int <- full[,sapply(full,is.integer)]
chr[is.na(chr)] <- "Not Available"
fac <- chr %>% lapply(as.factor) %>% as.data.frame()

full <- bind_cols(fac,int)
micemod <- full %>% mice(method='rf')
full <- complete(micemod)
rm(chr,fac,int,fill_chr,micemod)
train <- full[1:length(SalePrice),]
test<-full[(length(SalePrice)+1):nrow(full),]

set.seed(841995)
my_control <-trainControl(method="cv", number=5)
lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))
lasso_mod <- train(x=train, y=SalePrice, method='glmnet', trControl= my_control, tuneGrid=lassoGrid) 


solution <- data.frame(Id=Id,SalePrice=lasso_pred)
write.csv(solution,"/Users/karmapatel/Developer/Github/house_prices_advanced_regression_technique/lasso.csv",row.names = F)
