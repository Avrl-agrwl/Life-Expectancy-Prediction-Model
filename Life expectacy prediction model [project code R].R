#import dataset
library(pls)
library(forecast)
library(DAAG)
library(readr)
library(gains)
library(ggplot2)
library(psych)
library(rpart.plot)
library(rpart)
library(staTools)
library(tidyverse)
library(FNN)
library(caret)
library(usmap)
#Creating functions
rmse <- function(error)
{
  sqrt(mean(error^2))
}

standarize <- function(x)
{
  z <- (x-mean(x))/sd(x)
  return(z)
}
############################################load the data 
Life_exp <- read_csv("E:/northeastern university/courses/Data mining/Project/Life Expectancy Data.csv")

######### Remove unwanted columns and null values
Life_exp1 <-  Life_exp[,-c(1:3,17,18)] %>%
  na.omit()

q1 <- Life_exp1
q1 <- as.data.frame(lapply(Life_exp1, standarize))
################################## scatter plot for the complete filtered dataset

pairs.panels(q1)
ggsave("scatter plot.jpeg", plot = pairs.panels(q1), dpi = 900) ## to save the scatter plot

####################################Regression################################

################################################ Sampling
set.seed(213)
index <- sample(nrow(q1), size = nrow(q1)*0.6,)
training <- q1[index,]
validation <- q1[-index,]

##############################Multiple linear regression######################
training.lm <- lm(`Life.expectancy`~.,data = training)
options(scipen = 999)
summary(training.lm)

predicted <- predict(training.lm, validation) 
residual <- validation$`Life.expectancy`-predicted
residual_df <- data.frame("Predicted" = predicted, "Actual" = validation$`Life.expectancy`,
           "Residual" = residual)
head(residual_df,20)

accuracy(predicted, validation$`Life.expectancy`)

############################################# Multiple linear regression with PCA

fa.parallel(q1[,-c(1)], fa="pc", n.iter = 100, show.legend = TRUE,
            main = "Scree plot with parallel analysis")  ##########number of components=4


training2.lm <- pcr(`Life.expectancy`~.,data = training, scale= TRUE, validation='CV')
prediction2 <- predict(training2.lm, validation, ncomp = 4)

residual2 <- validation$`Life.expectancy`-prediction2

residual2_df <- data.frame("Predicted" = prediction2, "Actual" = validation$`Life.expectancy`,
           "Residual" = residual2)
head(residual2_df,20)

################################lift chart#################################
########################################################################### lift chart for MLR 

gain <- gains(validation$`Life.expectancy`[!is.na(predicted)], predicted[!is.na(predicted)])
options(scipen=999)
expectancy <- validation$`Life.expectancy`[!is.na(validation$`Life.expectancy`)]

par(pty="s")
plot(c(0,gain$cume.pct.of.total*sum(expectancy))~c(0,gain$cume.obs), 
     xlab = "# cases", ylab = "Cumulative Life Expectancy", main = "Lift Chart", type = "l")
#baseline
lines(c(0,sum(expectancy))~c(0,dim(validation)[1]), col = "gray", lty = 2)

########################################################################### lift chart MLR with PCA

gain <- gains(validation$`Life.expectancy`[!is.na(prediction2)], prediction2[!is.na(prediction2)])
options(scipen=999)
expectancy <- validation$`Life.expectancy`[!is.na(validation$`Life.expectancy`)]

par(pty="s")
plot(c(0,gain$cume.pct.of.total*sum(expectancy))~c(0,gain$cume.obs), 
     xlab = "# cases", ylab = "Cumulative Life Expectancy", main = "Lift Chart", type = "l")
#baseline
lines(c(0,sum(expectancy))~c(0,dim(validation)[1]), col = "gray", lty = 2)

################################################## Random forest RT

rpart_model <- rpart(`Life.expectancy`~., data = training, method = "anova")
rpart.plot(rpart_model) #to plot regression tree
summary(rpart_model) #to identify variable of importance

###boxplot of error values in training and validatioin dataset
pred_train <- predict(rpart_model,training)
pred_valid <- predict(rpart_model,validation)
error_train <- pred_train-training$`Life.expectancy`
error_valid <- pred_valid-validation$`Life.expectancy`
boxplot(error_train, error_valid,names = c("Training", "Validation"))

Cp <-  rpart_model$cptable[which.min(rpart_model$cptable[,"xerror"]), "CP"]

#pruning the tree
prune_model <- prune.rpart(rpart_model, cp = Cp)
rpart.plot(prune_model)

####################################################################comparison of regression models
####RMSE values
RMSE1 <- rmse(residual) #for MLR
RMSE2 <- rmse(residual2) #for MLR with PCA
RMSE3 <- RMSE(pred_valid, validation$`Life.expectancy`) #For regression tree
RMSE <- c(RMSE1, RMSE2,RMSE3)

###correlation values
cor_rt <- cor(pred_valid, validation$`Life.expectancy`) #for MLR
cor_mlr <- cor(validation$`Life.expectancy`, predicted) # for MLR with PCA
cor_pca <- cor(validation$`Life.expectancy`,prediction2) # For regression tree
Cor <- c(cor_mlr, cor_pca, cor_rt)
comparison <- as.data.frame(cbind(RMSE,Cor), row.names = c("MLR", "MLR with PCA", "Regression Tree"))
colnames(comparison) <- c("RMSE", "Correlation_Value")
comparison

#################################### Classification ##############################################

#############visualization
#barplot representing change in category count in year 2000 vs 2015

viz <- Life_exp %>%
  mutate(Category= ifelse(`Life expectancy`>=76, "High",
                          ifelse(`Life expectancy`<=60, "Low", "Average")))
category2000 <- viz %>%
  filter(Year=="2000") %>%
  dplyr::select(Country, Category) %>%
  group_by(Category) %>%
  summarise(Count=n())
category2000$Category <- factor(category2000$Category, levels = c("Low","Average","High"))

ggplot(category2000 ) +geom_bar(aes(Category,Count),stat = "identity", fill="steelblue") +
  theme_classic() +ggtitle("Category Count in 2000")

category2015 <- viz %>%
  filter(Year=="2015") %>%
  dplyr::select(Country, Category) %>%
  group_by(Category) %>%
  summarise(Count=n())
category2015$Category <- factor(category2015$Category, levels = c("Low","Average","High"))

ggplot(category2015) +geom_bar(aes(Category,Count),stat = "identity", fill="steelblue") +
  theme_classic() +ggtitle("Category Count in 2015")

############ Sampling
set.seed(123)
Class_df <- Life_exp1 %>%
  mutate(Category= ifelse(`Life expectancy`>=76, "High",
                          ifelse(`Life expectancy`<=60, "Low", "Average")))

Class_df<-Class_df[,-1] 

index <- sample(nrow(Class_df), size = nrow(Class_df)*0.6,)

train <- Class_df[index,] 
valid <- Class_df[-index,]
pred <- train$Category

train2 <- train[,-17]
valid2 <- valid[,-17]

################################################ knn

accuracy.df <- data.frame(k = seq(1, 20, 1), accuracy = rep(0, 20))

for(i in 1:20) {
  kpred <- knn(train2, valid2,pred, k= i)
  accuracy.df[i, 2] <- confusionMatrix(as.factor(kpred), 
                                       as.factor(valid$Category))$overall[1]
}
accuracy.df
kbest <- knn(train2,valid2,pred,k=5)

confusionMatrix(as.factor(kbest), as.factor(valid$Category))

################################################################################## Classification tree

rpart_model2 <- rpart(Category~.,data = train, method = "class")
summary(rpart_model2)
rpart.plot(rpart_model2, box.palette = list("Red", "Green","Grey"))

predict_ct <- predict(rpart_model2,valid2, type = "class")
confusionMatrix(as.factor(predict_ct), as.factor(valid$Category))

#pruning the tree
Cp <-  rpart_model2$cptable[which.min(rpart_model2$cptable[,"xerror"]), "CP"]

prune_model2 <- prune.rpart(rpart_model2, cp = Cp)
rpart.plot(prune_model2, box.palette = list("Red", "Green","Grey"))
predict_ct_prune <- predict(prune_model2, valid2, type = "class")
confusionMatrix(as.factor(predict_ct_prune), as.factor(valid$Category))



