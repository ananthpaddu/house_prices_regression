if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, mice, e1071, Metrics, skimr, pracma)

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

svm_model<-svm(SalePrice~., data=train, cost = 3)
svm_pred <- predict(svm_model,newdata = test)

solution <- data.frame(Id=Id,SalePrice=nthroot(other1$SalePrice*other2$SalePrice*svm_pred,3))
write.csv(solution,"hybrid_solution.csv",row.names = F)
