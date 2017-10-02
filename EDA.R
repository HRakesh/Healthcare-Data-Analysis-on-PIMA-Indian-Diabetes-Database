
#This R script provides an EDA of the Pima Indians Diabetes Database - a small dataset 
#with missing values. We have used imputation techniques using MICE and tried analyzing using 
#some explanatory models (linear regression).

rm(list = ls())
gc()

#loading the dataset and familiarizing the data
data <-read.csv(file="diabetes.csv",
                stringsAsFactors=FALSE,header=TRUE)
head (data)
tail (data)
str(data)
summary(data)

#there are missing values as observed

#imputation using MICE
install.packages('mice')
library(mice)

#missing values replaced by 'NA'
data[, 2:6][data[, 2:6] == 0] <- NA
View(data)

#a function that computes the percentage of missing values
missingdata <- function(x){sum(is.na(x))/length(x)*100}

#percentage of missing data column and row wise is explained below
apply(data,2,missingdata)

apply(data,1,missingdata)

#understand the missing data pattern
md.pattern(data)

#visual representation of above pattern can be obtained using the VIM
install.packages("VIM")
library(VIM)

vim_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), 
                 cex.axis=0.7, gap=1, ylab=c("Histogram of missing data","Pattern"))

#visually checking the imputed/mising values 
marginplot(data[c(1,5)])
marginplot(data[c(1,3)])
marginplot(data[c(1,4)])
marginplot(data[c(1,6)])
marginplot(data[c(1,2)])

methods(mice)

#imputing the missing values
#remove the outcome column from the operation

tempData <- mice(data[, !names(data) %in% "Outcome"],m=5,maxit=5,meth='norm.predict',seed=500)
summary(tempData)


tempData$imp$Pregnancies
tempData$imp$Glucose
tempData$imp$BloodPressure
tempData$imp$SkinThickness
tempData$imp$Insulin
tempData$imp$BMI

ImputedData <- complete(tempData,2)
View(ImputedData)

#add back the outcome column to the table
ImputedData$Outcome <- data$Outcome
View(ImputedData)

UnSupData <-ImputedData

#analyzing the imputed data with the earlier observed data
library(lattice)
densityplot(tempData)
#the density of the imputed data for each imputed dataset -> RED
#the density of the observed data -> BLUE

#check the Imputed data - observe the min, max, mean values
summary(ImputedData)
str(ImputedData)
View(ImputedData)

graphics.off()

#check the spread between patients with diabetes and patients with no diabetes

install.packages("plotrix")
library("plotrix")
pie3D(table(ImputedData$Outcome), explode = 0.1, labels = c("Diabetes Negative","Diabetes Positive"), 
      main= "Pie Chart of Outcome", col = c("darkseagreen1","red"))

#check the spread between patients with diabetes and patients with no diabetes in percentage
prop.table(table(data$Outcome))

install.packages("corrplot")
library(corrplot)

#corelation between the attributes
correlat <- cor(ImputedData[, setdiff(names(ImputedData), 'Outcome')])
correlat
corrplot(correlat)

#Evaluating the quality of data by checking distribution and outliers
aggregate(Pregnancies ~ Age, summary, data=ImputedData)
aggregate(Insulin ~ Age, summary, data=ImputedData)
aggregate(BloodPressure ~ Age, summary, data=ImputedData)

boxplot(Pregnancies ~ Age, data=ImputedData, outline = TRUE, names, plot = TRUE, 
        col= 'light blue',   xlab = "Age", ylab = "Pregnancies")

plot(ImputedData$Age, ImputedData$Pregnancies, col=ifelse(ImputedData$Outcome==1, "red", "black"))
plot(ImputedData$Age, ImputedData$Glucose, col=ifelse(ImputedData$Outcome==1, "red", "black"))
plot(ImputedData$Age, ImputedData$Insulin, col=ifelse(ImputedData$Outcome==1, "red", "black"))
plot(ImputedData$Age, ImputedData$BMI, col=ifelse(ImputedData$Outcome==1, "red", "black"))
plot(ImputedData$Age, ImputedData$DiabetesPedigreeFunction, col=ifelse(ImputedData$Outcome==1, "red", "black"))

install.packages("ggplot2")
library(ggplot2)
qplot(Age, DiabetesPedigreeFunction, data=ImputedData, facets=Outcome ~.)

#multiple regression
fit0 <- lm(ImputedData$Age ~ ImputedData$DiabetesPedigreeFunction)
summary(fit0)

fit <- lm(ImputedData$Outcome ~ ImputedData$Age + ImputedData$Pregnancies)
fit1 <- lm(ImputedData$Outcome ~ ImputedData$Glucose + ImputedData$Insulin)
fit2 <- lm(ImputedData$Outcome ~ ImputedData$BMI + ImputedData$SkinThickness)

summary(fit)
summary(fit1)
summary(fit2)

# partition data
NegativeOutcome <- subset(ImputedData,ImputedData$Outcome == 0)
PositiveOutcome <- subset(ImputedData,ImputedData$Outcome == 1)

nrow(NegativeOutcome)

nrow(PositiveOutcome)


summary(PositiveOutcome$Age)
summary(NegativeOutcome$Age)

par(mfrow = c(1, 2))
boxplot(PositiveOutcome$Age)
boxplot(NegativeOutcome$Age)

hist(PositiveOutcome$Age)
hist(NegativeOutcome$Age)

par(mfrow = c(1, 1))
plot(PositiveOutcome$Age, PositiveOutcome$Pregnancies)
plot(NegativeOutcome$Age, NegativeOutcome$Pregnancies)

install.packages('dplyr')
library(dplyr)

nrow(NegativeOutcome)

nrow(PositiveOutcome)

#Percentage of diabetic women with pregnecies > 5
(nrow(filter(NegativeOutcome,NegativeOutcome$Pregnancies > 5))/nrow(NegativeOutcome))*100
(nrow(filter(PositiveOutcome,PositiveOutcome$Pregnancies > 5))/nrow(PositiveOutcome))*100

#Percentage of diabetic women with age > 40
(nrow(filter(NegativeOutcome,NegativeOutcome$Age > 35))/nrow(NegativeOutcome))*100
(nrow(filter(PositiveOutcome,PositiveOutcome$Age > 35))/nrow(PositiveOutcome))*100

#Percentage of diabetic women age>35 AND pregnancies>5
(nrow(filter(NegativeOutcome,NegativeOutcome$Pregnancies > 5, NegativeOutcome$Age > 35))/
    nrow(NegativeOutcome))*100
(nrow(filter(PositiveOutcome,PositiveOutcome$Pregnancies > 5, PositiveOutcome$Age > 35))
  /nrow(PositiveOutcome))*100


nrow(filter(NegativeOutcome,NegativeOutcome$Pregnancies > 5, NegativeOutcome$Age > 35))
nrow(filter(PositiveOutcome,PositiveOutcome$Pregnancies > 5, PositiveOutcome$Age > 35))

#Exploring glucose
summary(PositiveOutcome$Glucose)
summary(NegativeOutcome$Glucose)

par(mfrow = c(1, 2))
boxplot(PositiveOutcome$Glucose)
boxplot(NegativeOutcome$Glucose)
par(mfrow = c(1, 1))
hist(PositiveOutcome$Glucose)
hist(NegativeOutcome$Glucose)

#Percentage of diabetic women with glucose > 110


ImputedData = within(ImputedData, {
  GlucoseBinning = ifelse(Glucose >= 110, 1, 0)
})

counts <- table(ImputedData$Outcome, ImputedData$GlucoseBinning, 
                dnn=c("Outcome", "Glucose"))
counts

nrow(filter(NegativeOutcome,NegativeOutcome$Glucose >= 110))

nrow(filter(PositiveOutcome,PositiveOutcome$Glucose >= 110))

nrow(filter(ImputedData,ImputedData$Glucose >= 110))

nrow(filter(PositiveOutcome,PositiveOutcome$Glucose > 110))

nrow(filter(ImputedData,ImputedData$Glucose > 110))
(nrow(filter(NegativeOutcome,NegativeOutcome$Glucose > 110))/nrow(NegativeOutcome))*100
(nrow(filter(PositiveOutcome,PositiveOutcome$Glucose > 110))/nrow(PositiveOutcome))*100

nrow(filter(ImputedData,ImputedData$Glucose > 110))
(nrow(filter(ImputedData,ImputedData$Glucose > 110))/nrow(ImputedData))*100
(nrow(filter(ImputedData,ImputedData$Glucose > 110))/nrow(ImputedData))*100

#Exploring insulin

nrow(filter(NegativeOutcome,NegativeOutcome$Insulin < 166))

nrow(filter(PositiveOutcome,PositiveOutcome$Insulin < 166))

nrow(filter(ImputedData,ImputedData$Insulin < 166))

nrow(filter(PositiveOutcome,PositiveOutcome$Insulin >= 166))

summary(PositiveOutcome$Insulin)
summary(NegativeOutcome$Insulin)

par(mfrow = c(1, 2))
boxplot(PositiveOutcome$Insulin)
boxplot(NegativeOutcome$Insulin)
par(mfrow = c(1, 1))
hist(PositiveOutcome$Insulin)
hist(NegativeOutcome$Insulin)

nrow(filter(NegativeOutcome,NegativeOutcome$Insulin > 166))
nrow(filter(PositiveOutcome,PositiveOutcome$Insulin > 166))

(nrow(filter(NegativeOutcome,NegativeOutcome$Insulin > 166))/nrow(NegativeOutcome))*100
(nrow(filter(PositiveOutcome,PositiveOutcome$Insulin > 166))
  /nrow(PositiveOutcome))*100

#Exploring BMI

summary(PositiveOutcome$BMI)
summary(NegativeOutcome$BMI)

nrow(filter(NegativeOutcome,NegativeOutcome$BMI > 26))
nrow(filter(PositiveOutcome,PositiveOutcome$BMI > 26))

(nrow(filter(NegativeOutcome,NegativeOutcome$BMI > 30))/nrow(NegativeOutcome))*100
(nrow(filter(PositiveOutcome,PositiveOutcome$BMI > 30))
  /nrow(PositiveOutcome))*100


#Exploring SkinThickness

summary(PositiveOutcome$SkinThickness)
summary(NegativeOutcome$SkinThickness)

nrow(filter(NegativeOutcome,NegativeOutcome$SkinThickness > 26))
nrow(filter(PositiveOutcome,PositiveOutcome$SkinThickness > 26))

(nrow(filter(NegativeOutcome,NegativeOutcome$SkinThickness > 26))/nrow(NegativeOutcome))*100
(nrow(filter(PositiveOutcome,PositiveOutcome$SkinThickness > 26))
  /nrow(PositiveOutcome))*100


cor(PositiveOutcome)
cor(NegativeOutcome)

#########################################################################################
#Data understanding phase

#Checking the Imputed data for Outliers and applying zscore transformation for normalization

hist(ImputedData$Pregnancies,col = 'light blue', border = 'black', prob = T)
curve(dnorm(x, mean = mean(ImputedData$Pregnancies), sd = sd(ImputedData$Pregnancies)), add = T, col="red")
box(which = "plot", lty="solid", col="black")

hist(ImputedData$Glucose,col = 'light blue', border = 'black', prob = T)
curve(dnorm(x, mean = mean(ImputedData$Glucose), sd = sd(ImputedData$Glucose)), add = T, col="red")
box(which = "plot", lty="solid", col="black")

hist(ImputedData$BloodPressure,col = 'light blue', border = 'black', prob = T)
curve(dnorm(x, mean = mean(ImputedData$BloodPressure), sd = sd(ImputedData$BloodPressure)), add = T, col="red")
box(which = "plot", lty="solid", col="black")

hist(ImputedData$SkinThickness,col = 'light blue', border = 'black', prob = T)
curve(dnorm(x, mean = mean(ImputedData$SkinThickness), sd = sd(ImputedData$SkinThickness)), add = T, col="red")
box(which = "plot", lty="solid", col="black")

hist(ImputedData$Insulin,col = 'light blue', border = 'black', prob = T)
curve(dnorm(x, mean = mean(ImputedData$Insulin), sd = sd(ImputedData$Insulin)), add = T, col="red")
box(which = "plot", lty="solid", col="black")

hist(ImputedData$BMI,col = 'light blue', border = 'black', prob = T)
curve(dnorm(x, mean = mean(ImputedData$BMI), sd = sd(ImputedData$BMI)), add = T, col="red")
box(which = "plot", lty="solid", col="black")

hist(ImputedData$Age, col = 'light blue', border = 'black', prob = T)
curve(dnorm(x, mean=mean(ImputedData$Age), sd=sd(ImputedData$Age)), add = T, col="red")
box(which = "plot", lty="solid", col="black", prob = T)

hist(ImputedData$DiabetesPedigreeFunction, col = 'light blue', border = 'black', prob = T)
curve(dnorm(x, mean=mean(ImputedData$DiabetesPedigreeFunction), sd=sd(ImputedData$DiabetesPedigreeFunction)), add = T, col="red")
box(which = "plot", lty="solid", col="black", prob = T)

boxplot(ImputedData$Pregnancies,col= 'light blue',   xlab = "Pregnancies")
boxplot(ImputedData$SkinThickness, col= 'light blue',   xlab = "Skin Thickness")
boxplot(ImputedData$BMI, col= 'light blue',   xlab = "BMI")
boxplot(ImputedData$Glucose,col= 'light blue',   xlab = "Glucose")


#Detecting and Removing Outliers 

#Function to remove the outlier

outliers<-function(df, x){
  library(dplyr)
  i<-IQR(x)
  y<-quantile(x, .25)
  z<-quantile(x, .75)
  
  lower<-filter(df, x < (y - (1.5 * i)))
  greater<-filter(df, x > (z + (1.5 * i)))
  
  return(rbind.data.frame(lower,greater))
}


df_bmi_outliers<-outliers(ImputedData, ImputedData$BMI)
df_glucose_outliers<-outliers(ImputedData, ImputedData$Glucose)
df_preg_outliers<-outliers(ImputedData, ImputedData$Pregnancies)
df_dpf_outliers<-outliers(ImputedData, ImputedData$DiabetesPedigreeFunction)
df_age_outliers<-outliers(ImputedData, ImputedData$Age)
df_st_outliers<-outliers(ImputedData, ImputedData$SkinThickness)
df_insulin_outliers<-outliers(ImputedData, ImputedData$Insulin)
df_bp_outliers<-outliers(ImputedData, ImputedData$BloodPressure)

df_insulin_outliers$Outcome

nrow(df_bmi_outliers)
nrow(df_glucose_outliers)
nrow(df_preg_outliers)
nrow(df_dpf_outliers)
nrow(df_age_outliers)
nrow(df_st_outliers)
nrow(df_insulin_outliers)
nrow(df_bp_outliers)

View(ImputedData)

(a <- which(ImputedData$BMI %in% boxplot.stats(ImputedData$BMI)$out))
(b <- which(ImputedData$Glucose %in% boxplot.stats(ImputedData$Glucose)$out))
(c <- which(ImputedData$Pregnancies %in% boxplot.stats(ImputedData$Pregnancies)$out))
(d <- which(ImputedData$SkinThickness %in% boxplot.stats(ImputedData$SkinThickness)$out))
(e <- which(ImputedData$Age %in% boxplot.stats(ImputedData$Age)$out))
(f <- which(ImputedData$DiabetesPedigreeFunction %in% boxplot.stats(ImputedData$DiabetesPedigreeFunction)$out))
(g <- which(ImputedData$Insulin %in% boxplot.stats(ImputedData$Insulin)$out))
(h <- which(ImputedData$BloodPressure %in% boxplot.stats(ImputedData$BloodPressure)$out))
a
b
c
d
all_outliers <- Reduce(union, list(a,b,c,d))
all_outliers

#Removing Outlier from PREG ST GLU BMI 

removeOutliers <- function(data, indicesOfAllOutliers) {
  for (x in 1:nrow(indicesOfAllOutliers)){
    i = indicesOfAllOutliers[x,]
    data <- data[-c(i), ]
  }
  return(data)
}

transformedData<-removeOutliers(ImputedData, as.data.frame(all_outliers))

nrow(transformedData)

View(transformedData)

#Checking for Normalization

#There are variables with high right skew (Pregnancies, Insulin, DiabetesPedigreeFunction, Age) 
#and other with high left skew like BloodPressure.


hist(transformedData$Pregnancies,col = 'light blue', border = 'black', prob = T)
curve(dnorm(x, mean = mean(transformedData$Pregnancies), sd = sd(transformedData$Pregnancies)), add = T, col="red")
box(which = "plot", lty="solid", col="black")
zscore.Pregnancies <- (transformedData$Pregnancies -
                         mean(transformedData$Pregnancies))/sd(transformedData$Pregnancies)
zscore.Pregnancies
hist(zscore.Pregnancies,col = 'light blue', border = 'black', prob = T)
curve(dnorm(x, mean = mean(zscore.Pregnancies), sd = sd(zscore.Pregnancies)), add = T, col="red")
box(which = "plot", lty="solid", col="black")

boxplot(zscore.Pregnancies)

hist(transformedData$Glucose,col = 'light blue', border = 'black', prob = T)
curve(dnorm(x, mean = mean(transformedData$Glucose), sd = sd(transformedData$Glucose)), add = T, col="red")
box(which = "plot", lty="solid", col="black")


hist(transformedData$BloodPressure,col = 'light blue', border = 'black', prob = T)
curve(dnorm(x, mean = mean(transformedData$BloodPressure), sd = sd(transformedData$BloodPressure)), add = T, col="red")
box(which = "plot", lty="solid", col="black")

hist(transformedData$SkinThickness,col = 'light blue', border = 'black', prob = T)
curve(dnorm(x, mean = mean(transformedData$SkinThickness), sd = sd(transformedData$SkinThickness)), add = T, col="red")
box(which = "plot", lty="solid", col="black")

hist(transformedData$Insulin,col = 'light blue', border = 'black', prob = T)
curve(dnorm(x, mean = mean(transformedData$Insulin), sd = sd(transformedData$Insulin)), add = T, col="red")
box(which = "plot", lty="solid", col="black")

hist(transformedData$BMI,col = 'light blue', border = 'black', prob = T)
curve(dnorm(x, mean = mean(transformedData$BMI), sd = sd(transformedData$BMI)), add = T, col="red")
box(which = "plot", lty="solid", col="black")

hist(transformedData$Age, col = 'light blue', border = 'black', prob = T)
curve(dnorm(x, mean=mean(transformedData$Age), sd=sd(transformedData$Age)), add = T, col="red")
box(which = "plot", lty="solid", col="black", prob = T)
zscore.Age <- (transformedData$Age - mean(transformedData$Age))/sd(transformedData$Age)
zscore.Age
hist(zscore.Age,col = 'light blue', border = 'black', prob = T)
curve(dnorm(x, mean = mean(zscore.Age), sd = sd(zscore.Age)), add = T, col="red")
box(which = "plot", lty="solid", col="black")


hist(transformedData$DiabetesPedigreeFunction, col = 'light blue', border = 'black', prob = T)
curve(dnorm(x, mean=mean(transformedData$DiabetesPedigreeFunction), sd=sd(transformedData$DiabetesPedigreeFunction)), add = T, col="red")
box(which = "plot", lty="solid", col="black", prob = T)
zscore.DiabetesPedigreeFunction <- (transformedData$DiabetesPedigreeFunction -
                                      mean(transformedData$DiabetesPedigreeFunction))/sd(transformedData$DiabetesPedigreeFunction)
zscore.DiabetesPedigreeFunction
hist(zscore.DiabetesPedigreeFunction,col = 'light blue', border = 'black', prob = T)
curve(dnorm(x, mean = mean(zscore.DiabetesPedigreeFunction), sd = sd(zscore.DiabetesPedigreeFunction)), add = T, col="red")
box(which = "plot", lty="solid", col="black")

boxplot(zscore.DiabetesPedigreeFunction)

transformedData$Pregnancies<-zscore.Pregnancies
transformedData$Age<-zscore.Age
transformedData$DiabetesPedigreeFunction<-zscore.DiabetesPedigreeFunction

View(transformedData)

#Transform glucose to K flag variable, K = 3, Safe, Prediabetic and Diabetic


transformedData$PrediabeticGlucose = ifelse(transformedData$Glucose > 140 & transformedData$Glucose <= 199, 1, 0)
transformedData$DiabeticGlucose = ifelse(transformedData$Glucose > 199, 1, 0)

#Binning for Age variable

transformedData$AgeBinning <- ifelse(transformedData$Age < 31, "Lower", 
                                     ifelse((transformedData$Age >= 31 & 
                                               transformedData$Age <= 60),"Medium", "High" ))

View(transformedData)

#Interesting Subsets

#Partitioning 

set.seed(1234)

install.packages("caTools")
library(caTools)

ImputedData$Outcome <- factor(make.names(ImputedData$Outcome))

split <- sample.split(transformedData$Outcome, SplitRatio = 0.70)

# train_data <- ImputedData[split,]
# test_data <- ImputedData[-split,]

train_data <- subset(ImputedData,split == T)
test_data <- subset(ImputedData,split == F)

nrow(train_data)
nrow(test_data)

#Evaluating the train and test data
t.test(train_data$Pregnancies,test_data$Pregnancies)

t.test(train_data$BMI,test_data$BMI)

t.test(train_data$SkinThickness,test_data$SkinThickness)

t.test(train_data$Glucose,test_data$Glucose)

#################################################################################

# Thanks to http://machinelearningmastery.com/ and https://onlinecourses.science.psu.edu

# Modelling phase

# Unsupervised Learning


# K-means clustering

responseY <- UnSupData[,dim(UnSupData)[2]]
predictorX <- UnSupData[,1:(dim(UnSupData)[2]-1)] #Remove OUTCOME column

View(responseY)
View(predictorX)

#principal component analysis
pca <- princomp(predictorX, cor=T)  #cor is correlation matrix
pca
pc.comp <- pca$scores

pc.comp1 <- -1*pc.comp[,1] # principal component 1 scores (negated for convenience)
pc.comp2 <- -1*pc.comp[,2] # principal component 2 scores (negated for convenience)

X <- cbind(pc.comp1, pc.comp2)
cl <- kmeans(X,13)
cl$cluster
plot(pc.comp1, pc.comp2,col=cl$cluster, main='k-means Clustering analysis')
points(cl$centers, pch=16)
# Fig shows the resulting scatter plot with different clusters in different colors. 
# The solid black circles are the centers of the clusters.

# Gives a pretty good result with good clusters formed.
# Within cluster sum of squares by cluster:
# [1] 20.05304 15.94767 27.45816 29.47695 24.78921 20.86744 19.09157 28.97672 24.17836 22.60736 19.07573
# [12] 18.67121 25.20264
# (between_SS / total_SS =  90.7 %)

################

#Self-Organising Maps (SOMs) are an unsupervised data visualisation technique that can be used to visualise 
#high-dimensional data sets in lower (typically 2) dimensional representations.

# install the kohonen package
install.packages("kohonen")

# load the kohonen package
library("kohonen")

View(UnSupData)
str(UnSupData)

UnSupData$Outcome

split <- sample.split(UnSupData$Outcome, SplitRatio = 0.70)

U_train_data <- subset(UnSupData,split == T)
U_test_data <- subset(UnSupData,split == F)

nrow(U_train_data)
nrow(U_test_data)

# scale data
scale_data = scale(ImputedData[, 1:8])

# build grid
grid = somgrid(xdim = 5, ydim=5, topo="hexagonal")

# build model
som = som(scale_data, grid=grid, rlen=100, alpha=c(0.05,0.01))

data_train_matrix <- as.matrix(scale(U_train_data),center = TRUE, scale = TRUE)

som_model <- som(data_train_matrix, 
                 grid=grid, 
                 rlen=100, 
                 alpha=c(0.05,0.01))

plot(som_model, type="changes")

# As the SOM training iterations progress, the distance from each node's weights to the samples represented by that node is reduced. 
# Ideally, this distance should reach a minimum plateau. This plot option shows the progress over time. 
# If the curve is continually decreasing, more iterations are required.


plot(som_model, type="count")

# visualise the count of how many samples are mapped to each node on the map. 
# This metric can be used as a measure of map quality - ideally the sample distribution is relatively uniform. 
# Large values in some map areas suggests that a larger map would be benificial. 
# Empty nodes indicate that your map size is too big for the number of samples.
#Baseline Model - Logistic Regression
#K-Fold Cross Validation

#Let's use the most common variation of cross validation is 10-fold cross-validation.

#################

# Supervised Learning Techniques

#Baseline Model - Logistic Regression
#K-Fold Cross Validation

#Let's use the most common variation of cross validation is 10-fold cross-validation.

set.seed(1234)

table(train_data$Outcome) 

library(caret)
fitControl <- trainControl(method = "cv",
                           number = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)

glmmodel <- train(Outcome~.,
                   train_data,
                   method="glm",
                   metric="ROC",
                   tuneLength=10,
                   preProcess = c('center', 'scale'),
                   trControl=fitControl)

p_glm <- predict(glmmodel, test_data)
cmat_glm <- confusionMatrix(p_glm, test_data$Outcome, positive="X1")
cmat_glm

install.packages('pROC')
library(pROC)

#Column-wise Area Under ROC Curve (AUC)
prob_p_glm <- predict(glmmodel, test_data, type="prob")
roc_glm <- roc(test_data$Outcome, prob_p_glm$X1)
colAUC(prob_p_glm$X1, test_data$Outcome, plotROC = TRUE)

#Result from Logistic Regression
# AUC = 0.835292 (not bad)

# Accuracy : 0.7719
# 95% CI : (0.7119, 0.8247)
# No Information Rate : 0.6842          
# P-Value [Acc > NIR] : 0.00219         
# 
# Kappa : 0.4431          
# Mcnemar's Test P-Value : 0.07142         
# 
# Sensitivity : 0.5417          
# Specificity : 0.8782          
# Pos Pred Value : 0.6724          
# Neg Pred Value : 0.8059          
# Prevalence : 0.3158          
# Detection Rate : 0.1711          
# Detection Prevalence : 0.2544          
# Balanced Accuracy : 0.7099
# 'Positive' Class : X1 


# Lets try one more model and compare both of the accuracy.

knnmodel <- train(Outcome~.,
                   train_data,
                   method="knn",
                   metric="ROC",
                   trControl=fitControl)
p_knn <- predict(knnmodel, test_data)
cmat_knn <- confusionMatrix(p_knn, test_data$Outcome, positive="X1")
cmat_knn

prob_p_knn <- predict(knnmodel, test_data, type="prob")
roc_knn <- roc(test_data$Outcome, prob_p_knn$X1)

colAUC(prob_p_knn$X1, test_data$Outcome, plotROC = TRUE)

# Accuracy : 0.7193          
# 95% CI : (0.6562, 0.7766)
# No Information Rate : 0.6842          
# P-Value [Acc > NIR] : 0.1423          
# 
# Kappa : 0.3693          
# Mcnemar's Test P-Value : 0.3816          
# 
# Sensitivity : 0.6111          
# Specificity : 0.7692          
# Pos Pred Value : 0.5500          
# Neg Pred Value : 0.8108          
# Prevalence : 0.3158          
# Detection Rate : 0.1930          
# Detection Prevalence : 0.3509          
# Balanced Accuracy : 0.6902          
# 'Positive' Class : X1  

?resamples

models <- list(LOGREG=glmmodel, KNN=knnmodel)
resamples <- resamples(models)
summary(resamples)
bwplot(resamples, metric="ROC")

# logistic regression looks like to be the best model here: best sensitivity, F1 score and AUC.


###################################
