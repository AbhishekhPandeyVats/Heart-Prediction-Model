# ==============================================================================================
# ---------- Import libraries ------------------------------------------------
# ==============================================================================================
library(data.table)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(car)
library(caTools)
library(tidyverse)
library(corrplot)
library(ggcorrplot)
library(moments)
library(scales)
library(datasets)
library(caret)
library(randomForest)
library(ROCR)

#read.table(file='/Users/keithheng/Documents/NTU/BC2406/Project/Cleveland.data', header = FALSE, sep ="")
#read.table(file='/Users/keithheng/Documents/NTU/BC2406/Project/Switzerland.data', header = FALSE, sep ="")
#read.table(file='/Users/keithheng/Documents/NTU/BC2406/Project/Hungarian.data', header = FALSE, sep ="")
#read.table(file='/Users/keithheng/Documents/NTU/BC2406/Project/VA.data', header = FALSE, sep ="")

# ==============================================================================================
# ---------- Import Dataset --------------------------------------------------------------------
# ==============================================================================================
setwd('C:/Users/iansi/Documents/NTU/3. Modules/Year 2/Y2S1/BC2406/2. Projects/Code')
heart.dt <- fread('heart.csv',  na.strings = c("", "missing", "N/A", -99, "m", "na", "."))

# ==============================================================================================
# ---------- Data Exploration ---------------------------------------------------------------
# ==============================================================================================
# Check for missing values
sapply(heart.dt, function(x) sum(is.na(x)))
# Check datatype and convert chr to categorical
str(heart.dt)
heart.dt <- fread('heart.csv', stringsAsFactors = TRUE, na.strings = c("", "missing", "N/A", -99, "m", "na", "."))
# Convert HeartDisease to categorical variable
heart.dt$HeartDisease <- factor(heart.dt$HeartDisease)
# Convert FastingBS to categorical variable
heart.dt$FastingBS <- factor(heart.dt$FastingBS)
# Final datatype
str(heart.dt)

# Summary - Cholesterol, RestingBP erroneous values
summary(heart.dt)

# Stacked Bar Plot - HeartDisease vs Sex
ggplot(heart.dt) + aes(Sex, fill = HeartDisease) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "Sex", y = "HeartDisease",
        title ="Relationship Between HeartDisease & Sex")

# Stacked Bar Plot - HeartDisease vs ChestPainType
ggplot(heart.dt) + aes(ChestPainType, fill = HeartDisease) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "ChestPainType", y = "HeartDisease",
        title ="Relationship Between HeartDisease & ChestPainType")

# Stacked Bar Plot - HeartDisease vs FastingBS
ggplot(heart.dt) + aes(FastingBS, fill = HeartDisease) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "FastingBS", y = "HeartDisease",
        title ="Relationship Between HeartDisease & FastingBS")

# Stacked Bar Plot - HeartDisease vs RestingECG
ggplot(heart.dt) + aes(RestingECG, fill = HeartDisease) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "RestingECG", y = "HeartDisease",
        title ="Relationship Between HeartDisease & RestingECG")

# Stacked Bar Plot - HeartDisease vs ExerciseAngina
ggplot(heart.dt) + aes(ExerciseAngina, fill = HeartDisease) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "ExerciseAngina", y = "HeartDisease",
        title ="Relationship Between HeartDisease & ExerciseAngina")


# Stacked Bar Plot - HeartDisease vs ST_Slope
ggplot(heart.dt) + aes(ST_Slope, fill = HeartDisease) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "ST_Slope", y = "HeartDisease",
        title ="Relationship Between HeartDisease & ST_Slope")

# Boxplot of all Continuous variables
# Age
ggplot(data=heart.dt, aes(x=Age, y=HeartDisease, fill=HeartDisease)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "light blue", "1" = "orange")) +
  labs(title = "Distribution of HeartDisease vs. Age")
# RestingBP
ggplot(data=heart.dt, aes(x=RestingBP, y=HeartDisease, fill=HeartDisease)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "light blue", "1" = "orange")) +
  labs(title = "Distribution of HeartDisease vs. RestingBP")
# Cholesterol
ggplot(data=heart.dt, aes(x=Cholesterol, y=HeartDisease, fill=HeartDisease)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "light blue", "1" = "orange")) +
  labs(title = "Distribution of HeartDisease vs. Cholesterol")
# MaxHR
ggplot(data=heart.dt, aes(x=MaxHR, y=HeartDisease, fill=HeartDisease)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "light blue", "1" = "orange")) +
  labs(title = "Distribution of HeartDisease vs. MaxHR")
# Oldpeak
ggplot(data=heart.dt, aes(x=Oldpeak, y=HeartDisease, fill=HeartDisease)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "light blue", "1" = "orange")) +
  labs(title = "Distribution of HeartDisease vs. Oldpeak")

# ---------- Clean erroneous values for RestingBP & Cholesterol ----------------------------------------
# Creating cleaned CART dataset----------------------------------------------------------------------
cart.dt <- copy(heart.dt)

# Replace RestingBP = 0 with NA
cart.dt <- cart.dt[RestingBP == 0, RestingBP:=NA]
# Replace Cholesterol = 0 with NA
cart.dt <- cart.dt[Cholesterol == 0, Cholesterol:=NA]
summary(cart.dt)

#Export cart.dt
write.csv(cart.dt, 'cartdt.csv')

# Creating cleaned RF & LG dataset----------------------------------------------------------------------
model.dt <- copy(heart.dt)
# Count erroneous RestingBP values
count(model.dt[RestingBP == 0,])
# Drop RestingBP == 0 data point
model.dt <- model.dt[RestingBP!=0]
summary(model.dt)

# Count erroneous Cholesterol values
count(model.dt[Cholesterol == 0,])
# Create train set & test set
set.seed(789)
imputeoriginal.dt <- copy(model.dt)
imputeoriginal.dt <- imputeoriginal.dt[Cholesterol!=0]
train <- sample.split(Y = imputeoriginal.dt$Cholesterol, SplitRatio = 0.7)
trainset <- subset(imputeoriginal.dt, train == T)
testset <- subset(imputeoriginal.dt, train == F)

# Make a copy of testset to mediantest
mediantest <- copy(testset)
# Impute via Median -----------------------------------------------------------------
# Replace Cholesterol values with the median of Cholesterol values in trainset
mediantest$Cholesterol <- median(trainset$Cholesterol)

medianerror <- mediantest$Cholesterol - testset$Cholesterol
RMSE <- sqrt(mean(medianerror^2))
RMSE

# Impute via Linear Regression  -----------------------------------------------------------------
lr.m1 <- lm(Cholesterol~., data=trainset)
#Use backward elimination to identify the best model
lr.m2 <- step(lr.m1)
summary(lr.m2)

RMSE.lr.m2.train <- sqrt(mean(residuals(lr.m2)^2)) #RMSE on trainset
RMSE.lr.m2.train
summary(abs(residuals(lr.m2)))  # Check Min Abs Error and Max Abs

predict.lr.m2.test <- predict(lr.m2, newdata = testset)

#Find errors of testset
testset.error <- testset$Cholesterol - predict.lr.m2.test
RMSE.lr.m2.test <- sqrt(mean(testset.error^2))
summary(abs(testset.error))

RMSE.lr.m2.train
RMSE.lr.m2.test

# Impute via KNN  ---------------------------------------------------------------------------
knn.dt <- copy(imputeoriginal.dt)

knn.dt$Sex <- as.numeric(knn.dt$Sex)
knn.dt$ChestPainType  <- as.numeric(knn.dt$ChestPainType)
knn.dt$RestingECG <- as.numeric(knn.dt$RestingECG)
knn.dt$ExerciseAngina <- as.numeric(knn.dt$ExerciseAngina)
knn.dt$ST_Slope <- as.numeric(knn.dt$ST_Slope)
knn.dt$HeartDisease <- as.numeric(knn.dt$HeartDisease)
knn.dt$FastingBS <- as.numeric(knn.dt$FastingBS)

set.seed(789)
train <- sample.split(Y = knn.dt$Cholesterol, SplitRatio = 0.7)
knntrain <- subset(knn.dt, train == T)
knntest <- subset(knn.dt, train == F)

train_x <- subset(knntrain, select = -c(Cholesterol))
train_y <- knntrain[, Cholesterol]
test_x <- subset(knntest, select = -c(Cholesterol))
test_y <- testset[, Cholesterol]

# Run kNN imputation
knnmodel <- knnreg(train_x, train_y)
str(knnmodel)

pred_y <- predict(knnmodel, test_x)
print(data.frame(test_y, pred_y))
mse <- mean((test_y - pred_y)^2)
mae <- caret::MAE(test_y, pred_y)
rmse <- caret::RMSE(test_y, pred_y)
cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)

# Drop erroneous cholesterol rows as imputation is highly inaccurate
model.dt <- model.dt[Cholesterol!=0,]
summary(model.dt)
nrow(model.dt)

#Export model.dt
write.csv(model.dt, 'modeldt.csv')

# ==============================================================================================
# ---------- Post-Cleaning Data Exploration ----------------------------------------------------
# ==============================================================================================

# Plot variables after erroneous values cleaned
# RestingBP
ggplot(data=model.dt, aes(x=RestingBP, y=HeartDisease, fill=HeartDisease)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "light blue", "1" = "orange")) +
  labs(title = "Distribution of HeartDisease vs. RestingBP")
# Cholesterol
ggplot(data=model.dt, aes(x=Cholesterol, y=HeartDisease, fill=HeartDisease)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "light blue", "1" = "orange")) +
  labs(title = "Distribution of HeartDisease vs. Cholesterol")

# Plot heatmap between continuous variables
subset1 <- model.dt[, c("Age", "RestingBP", "Cholesterol", "MaxHR", "Oldpeak")]
cor1 <- round(cor(subset1), 5)
cor1 <- melt(cor1)
head(cor1)
ggplot(data = cor1, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  ggtitle("Heatmap of Continuous Variables") +
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank())

# Scatterplot of RestingBP vs Age
ggplot(model.dt, aes(x = Age, y = RestingBP)) +
  geom_point(color= "steelblue") + geom_smooth(formula = y ~ x,method = "lm") +
  ggtitle("Scatterplot of RestingBP against Age")
# Scatterplot of MaxHR vs Age
ggplot(model.dt, aes(x = Age, y = MaxHR)) +
  geom_point(color= "steelblue") + geom_smooth(formula = y ~ x,method = "lm") +
  ggtitle("Scatterplot of MaxHR against Age")
# Scatterplot of Oldpeak vs Age
ggplot(model.dt, aes(x = Age, y = Oldpeak)) +
  geom_point(color= "steelblue") + geom_smooth(formula = y ~ x,method = "lm") +
  ggtitle("Scatterplot of Oldpeak against Age")
# Scatterplot of MaxHR vs Oldpeak
ggplot(model.dt, aes(x = MaxHR, y = Oldpeak)) +
  geom_point(color= "steelblue") + geom_smooth(formula = y ~ x,method = "lm") +
  ggtitle("Scatterplot of Oldpeak against MaxHR")

# Chi-square test for categorical variables
chisqmatrix <- function(x) {
  names = colnames(x);  num = length(names)
  m = matrix(nrow=num, ncol=num, dimnames=list(names,names))
  for (i in 1:(num-1)) {
    for (j in (i+1):num) {
      m[i,j] = chisq.test(x[[i]], x[[j]],)$p.value
    }
  }
  return (m)
}
subset2 <- model.dt[, c("Sex", "RestingECG", "FastingBS", "ExerciseAngina", "ST_Slope", "HeartDisease", "ChestPainType")]
mat <- chisqmatrix(subset2)
mat

# Barplot of ExerciseAngina vs ST_Slope
ggplot(model.dt) + aes(ExerciseAngina, fill = ST_Slope) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "ExerciseAngina", y = "ST_Slope",
        title ="Relationship Between ST_Slope & ExerciseAngina")
# Barplot of ExerciseAngina vs ChestPainType
ggplot(model.dt) + aes(ExerciseAngina, fill = ChestPainType) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "ExerciseAngina", y = "ChestPainType",
        title ="Relationship Between ChestPainType & ExerciseAngina")
# Barplot of ST_Slope vs ChestPainType
ggplot(model.dt) + aes(ST_Slope, fill = ChestPainType) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "ST_Slope", y = "ChestPainType",
        title ="Relationship Between ST_Slope & ChestPainType")
# Barplot of Sex vs ExerciseAngina
ggplot(model.dt) + aes(Sex, fill = ExerciseAngina) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "Sex", y = "ExerciseAngina",
        title ="Relationship Between Sex & ExerciseAngina")
# Barplot of Sex vs ST_Slope
ggplot(model.dt) + aes(Sex, fill = ST_Slope) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "Sex", y = "ST_Slope",
        title ="Relationship Between Sex & ST_Slope")

# Further analysis
# Boxplot of Age vs ExerciseAngina
ggplot(data=model.dt, aes(x=Age, y=ExerciseAngina, fill=Age)) + 
  geom_boxplot() +
  labs(title = "Distribution of Age vs. ExerciseAngina")
# Boxplot of Age vs ST_Slope
ggplot(data=model.dt, aes(x=Age, y=ST_Slope, fill=Age)) + 
  geom_boxplot() +
  labs(title = "Distribution of Age vs. ST_Slope")
# Boxplot of Age vs ChestPainType
ggplot(data=model.dt, aes(x=Age, y=RestingECG, fill=Age)) + 
  geom_boxplot() +
  labs(title = "Distribution of Age vs. RestingECG")
# Boxplot of Age vs FastingBS
ggplot(data=model.dt, aes(x=Age, y=FastingBS, fill=Age)) + 
  geom_boxplot() +
  labs(title = "Distribution of Age vs. FastingBS")

# Bar Plot of HeartDisease and ExerciseAngina faceted by ST_Slope
ggplot(model.dt) + aes(x = ExerciseAngina, fill = HeartDisease) + 
  geom_bar(position = 'fill') + 
  facet_wrap(.~ST_Slope  ) +
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "ExerciseAngina", y = "Percentage",
        title ="Percentage Between ExerciseAngina & HeartDisease split by ST_Slope") 

# Bar Plot of HeartDisease and Sex faceted by ExerciseAngina,ST_Slope,ChestPainType
ggplot(model.dt) + aes(x = Sex, fill = HeartDisease) + 
  geom_bar(position = 'fill') + 
  facet_grid(ChestPainType ~ ST_Slope + ExerciseAngina , labeller=label_context) +
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "Sex", y = "Percentage",
        title ="Percentage Between Sex & HeartDisease split by ExerciseAngina, ST_Slope & ChestPainType") 

#==========================================================================================
#----------- Model 1: Logistic Regression Model-----------------------------
#==========================================================================================
# Train-test Split
set.seed(789)
train <- sample.split(Y = model.dt$HeartDisease, SplitRatio = 0.7)
trainset <- subset(model.dt, train == T)
summary(trainset)
testset <- subset(model.dt, train == F)
summary(testset)
str(trainset)

# -----<<<<<Logistic Regression>>>>>------
#Building model first time
m1 <- glm(HeartDisease ~ ., family = binomial, data = trainset)

#Building model second time, remove insignificant variables
m1 = step(m1)
summary(m1)

#Measure the strength correlation between the independent variables 
vif(m1) #VIF of 1 indicates variables are not correlated

#odd ratio
OR <- exp(coef(m1))
OR

#Find the confidence interval
OR.CI <- exp(confint(m1))
OR.CI

threshold1 <- 0.5 #Did to decide on what threshold to set, default is 0.5

summary(m1)

prob.train <- predict(m1, type = 'response')
m1.predict.train <- ifelse(prob.train > threshold1, 1, 0)


# Predicting on Testset
prob.test <- predict(m1, newdata = testset, type = 'response')
m4.predict.test <- ifelse(prob.test > threshold1, 1, 0)


##-----Evaluation of the model-----##
# Quick overview of model
confusionMatrix(as.factor(m4.predict.test), as.factor(testset$HeartDisease), positive="1", mode="everything")

#create an ROC curve (receiver operating characteristic curve) which will sweep 
#AUC-ROC
m1.predict2 <- predict(m1, newdata=testset, type="response")
m1.perf = prediction(m1.predict2, testset$HeartDisease)
m1.perf
# 1. Area under curve
m1.auc <- performance(m1.perf, "auc")
m1.auc
#Calculate AUC Value
m1.auc.value <- m1.auc@y.values[[1]]
# 2. True Positive and Negative Rate
m1.pred2 <- performance(m1.perf, "tpr","fpr")
# 3. Plot the ROC curve
plot(m1.pred2,main="ROC Curve for Logistic Regression",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
text(0.2,0.8, round(m1.auc.value,4))  

#==========================================================================================
#-------------- Model 2: CART Model-----------------------------
#==========================================================================================
#Train-Test Split
set.seed(789)
cart.train <- sample.split(Y = cart.dt$HeartDisease, SplitRatio = 0.7)
cart.trainset <- subset(cart.dt, cart.train == T)
cart.testset <- subset(cart.dt, cart.train == F)

#Create model and store in object
cart.m1 <- rpart(HeartDisease~., data=cart.trainset, method="class", control=rpart.control(minsplit=2, cp=0))

#Print pruning sequence and 10-fold CV errors as table
printcp(cart.m1)

#Find Optimal Tree
plotcp(cart.m1)

#Calculate CP of optimal tree using geometric mean
cp1 <- sqrt(0.0209059*0.0331010)

#Prune Max Tree using CP Value of Geomtric mean
cart.m2 <- prune(cart.m1, cp=cp1)

#Find CP Table for the optimal tree
printcp(cart.m2)

#Plot pruned optimal tree
rpart.plot(cart.m2, nn=T, main="Optimal Tree in heart.csv")

#Find Variable Importance
cart.m2$variable.importance
scaledVarImpt.heart <- round(100*cart.m2$variable.importance/sum(cart.m2$variable.importance),2)
scaledVarImpt.heart

#View summary of optimal tree
summary(cart.m2)

#Test pruned tree on test set
cart.m2.predict <- predict(cart.m2, newdata=cart.testset, type="class")

#All In One Confusion Matrix
confusionMatrix(cart.m2.predict, reference = cart.testset$HeartDisease, positive="1", mode="everything")

#AUC-ROC
cart.m2.predict2 <- predict(cart.m2, newdata=cart.testset, type="prob")
cart.perf = prediction(cart.m2.predict2[,2], cart.testset$HeartDisease)
cart.perf
# 1. Area under curve
cart.auc = performance(cart.perf, "auc")
cart.auc
#Calculate AUC Value
cart.auc.value <- cart.auc@y.values[[1]]
# 2. True Positive and Negative Rate
cart.pred2 = performance(cart.perf, "tpr","fpr")
# 3. Plot the ROC curve
plot(cart.pred2,main="ROC Curve for CART",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
text(0.1,0.8, round(cart.auc.value,4))

#==========================================================================================
#------------------ Model 3: Random Forest Model-----------------------------
#==========================================================================================
#Train-Test Split
set.seed(789)
rf.train <- sample.split(Y = model.dt$HeartDisease, SplitRatio = 0.7)
rf.trainset <- subset(model.dt, rf.train == T)
rf.testset <- subset(model.dt, rf.train == F)

rf <-randomForest(HeartDisease~.,data=rf.trainset, ntree=500)
print(rf)

#Find optimal mtry value
#Step Factor: To test for different mtry values by scaling by this value
#Improve: Required improvement to continue testing for other mtry
#Trace: To print the progress of search
#Plot: To plot the OOB error as function of mtry
mtry <- tuneRF(rf.trainset[,1:(length(rf.trainset)-1)],rf.trainset$HeartDisease, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)

#Find mtry with lowest OOBError
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m) #mtry 3 is the best mtry

#Build model again with best mtry value
rf2 <- randomForest(HeartDisease~.,data=rf.trainset, mtry=best.m, importance=TRUE, ntree=500)
print(rf2)

#Evaluate variable importance
importance(rf2)
varImpPlot(rf2)

#Predict on testset
rf.pred1 = predict(rf2, newdata=rf.testset, type="class")

#All in one confusion matrix
confusionMatrix(rf.pred1, reference = rf.testset$HeartDisease, positive="1", mode="everything")

#ROC-UC
rf.pred2=predict(rf,newdata=rf.testset, type = "prob")
rf.pred2
library(ROCR)
rf.perf = prediction(rf.pred2[,2], rf.testset$HeartDisease)
# 1. Area under curve
rf.auc = performance(rf.perf, "auc")
rf.auc
rf.auc.value <- rf.auc@y.values[[1]]
# 2. True Positive and Negative Rate
rf.pred3 = performance(rf.perf, "tpr","fpr")
# 3. Plot the ROC curve
plot(rf.pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
text(0.2,0.8, round(rf.auc.value,4))
