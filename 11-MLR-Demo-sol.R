##################################################
### PROG8430                                    ##
### Multiple Linear Regression - Demo           ## 
##################################################
#                                               ##
##################################################
# Written by Peiyuan
# ID: 123456
#
##################################################
### Basic Set Up                                ##
##################################################
# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())
options(scipen=9)

##################################################
### Install Libraries                           ##
##################################################
#If the library is not already downloaded, download it
if(!require(lattice)){install.packages("lattice")}
library("lattice")
if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")
if(!require(corrgram)){install.packages("corrgram")}
library("corrgram")
if(!require(cowplot)){install.packages("cowplot")}
library("cowplot")

##################################################
### Read data and do preliminary data checks    ##
##################################################
# Read text file (".txt")
# Training Data
train <- read.table("C:/Users/Geedhu/Documents/Maths _ Data Analysys/Week 11/MLR-Train.txt",
                    header = TRUE, sep = ",")
train[5:15,] #Prints data 5 to 15 to make sure it looks correct
str(train)     

###################################################
##Descriptive Analysis                           ##
###################################################

summary(train)
stat.desc(train)
#Boxplot 
par(mfrow=c(3,2))
for (i in 1:ncol(train)) {
  if (is.numeric(train[,i])) {
    boxplot(train[,i], main=names(train)[i],xlab="", horizontal=TRUE)
  }
}
#loop over column names - more advanced solution
#sapply(names(train), function(cname){# restrict to numeric columns
#  if (is.numeric(train[[cname]])) # Use the cname for the chart title
#    print(boxplot(train[[cname]], main=cname, xlab="", horizontal=TRUE))
#})


#Histogram Plot
round(stat.desc(train),2)
par(mfrow=c(3,2))    
for (i in 1:ncol(train)) {
  if (is.numeric(train[,i])) {
    hist(train[,i], main=names(train)[i],xlab="")
  }
}
## loop over column names - more advanced solution
#sapply(names(train), function(cname){ #restrict to numeric columns
#  if (is.numeric(train[[cname]])) #Use the cname for the chart title
#    print(hist(train[[cname]], main=cname, xlab=""))
#})

#Density Plot
round(stat.desc(train),2)
par(mfrow=c(3,2))    
den <- list()
for (i in 1:ncol(train)) {
  if (is.numeric(train[,i])) {
    den[[i]]<-densityplot(train[,i], main=names(train)[i])
    #[i]:a list, [[i]]: the object
  }
}
#plot_grid(den[[1]],den[[2]], den[[3]],den[[4]],den[[5]],den[[6]], ncol=2, nrow=3)
#do.call(plot_grid, c(den, list(ncol = 2)))
#plot_grid
#########################################
## Checking Correlations               ##
#########################################
trn_cr <- cor(train)
round(trn_cr,2)
trn_cr <- cor(train,method="spearman")
round(trn_cr,2)


corrgram(train, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Correlations")

#########################################
## Creating Baseline/Full Model        ##
#########################################
full.model = lm(Saving ~ ., data=train, na.action=na.omit)
summary(full.model)

###RMSE Evaluation
pred <- predict(full.model, newdata=train)
RMSE_trn_full <- sqrt(mean((train$Saving - pred)^2))
round(RMSE_trn_full,2)

test <- read.table("C:/Users/Geedhu/Documents/Maths _ Data Analysys/Week 11/MLR-Test.txt",
                    header = TRUE, sep = ",")
pred <- predict(full.model, newdata=test)
RMSE_trn_full <- sqrt(mean((test$Saving - pred)^2))
round(RMSE_trn_full,2)
#########################################
## Feature Selection                   ##
#########################################
##Backward Feature Selection
back.model = step(full.model, direction="backward", details=TRUE)
summary(back.model)
pred <- predict(back.model, newdata=train)
RMSE_trn_back <- sqrt(mean((train$Saving - pred)^2))
round(RMSE_trn_back,2)

##Forward Feature Selection
empty.model = lm(Saving ~ 1, data=train)
fwd.model = step(empty.model, direction="forward",scope=formula(full.model), details=TRUE)
summary(fwd.model)
pred <- predict(fwd.model, newdata=train)
RMSE_trn_step <- sqrt(mean((train$Saving - pred)^2))
round(RMSE_trn_step,2)

##Stepwise Feature Selection
step.model = step(empty.model, direction="both",scope=formula(full.model), details=TRUE)
summary(step.model)
pred <- predict(step.model, newdata=train)
RMSE_trn_step <- sqrt(mean((train$Saving - pred)^2))
round(RMSE_trn_step,2)

#Stepwise Regression using the forward/backward selection method based on the initial linear regression model
#AIC is a statistical criterion that quantifies the trade-off between model fit and complexity. 
#lower AIC values indicating better-fitting models

#########################################
## Regression Diagnostics              ##
#########################################
#Graphically
par(mfrow = c(2, 2))  
plot(full.model)  

par(mfrow = c(2, 2))  
plot(back.model)   

par(mfrow = c(2, 2))  
plot(step.model)   

###########################################
## Comparing to the Test Set              #
###########################################
test <- read.table("C:/Users/Geedhu/Documents/Maths _ Data Analysys/Week 11/MLR-Test.txt",
                   header = TRUE, sep = ",")
pred <- predict(full.model, newdata=test)
RMSE_tst_full <- sqrt(mean((test$Saving - pred)^2))

pred <- predict(back.model, newdata=test)
RMSE_tst_back <- sqrt(mean((test$Saving - pred)^2))

pred <- predict(step.model, newdata=test)
RMSE_tst_step <- sqrt(mean((test$Saving - pred)^2))

## Print and Compare all RMSE
RMSE_full <- c(RMSE_trn_full,RMSE_tst_full)
round(RMSE_full,2)

RMSE_back <- c(RMSE_trn_back,RMSE_tst_back)
round(RMSE_back,2)

RMSE_step <- c(RMSE_trn_step,RMSE_tst_step)
round(RMSE_step,2)
