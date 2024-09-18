##################################################
### PROG8430                                    ##
### Multiple Linear Regression - Demo           ## 
##################################################
#                                               ##
##################################################
# Written by Peiyuan
# ID: 123456789
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
library("cowplot")#for grid plot

##################################################
### Read in Data                                ##
##################################################
# Read "comma separated value" files (".csv")
# Thunder Basin Dataset
Thunder <- read.table("ThunderBasin.csv", header = TRUE, sep = ",")

##################################################
### Rename and Clean Variables                  ##
##################################################
#Rename Variables to something meaningful
str(Thunder)
names(Thunder) <- c("Fwn", "Adt", "Prc", "Sev")
str(Thunder)

##################################################
### Description of Data                         ##
##################################################
#Covered Previously
#Thunder Basin
summary(Thunder)
TdrSum <-stat.desc(Thunder)
format(TdrSum,digits=2)

den1=densityplot( ~Fwn, dat=Thunder, main="Dist of Fawns")
den2=densityplot( ~Adt, dat=Thunder,  main="Dist of Adults")
den3=densityplot( ~Prc, dat=Thunder, main="Dist of precipitation")
den4=densityplot( ~Sev, dat=Thunder, main="Dist of Winter Sev.")
plot_grid(den1, den2, den3, den4, ncol=2, nrow=2)

#Scatter plots of Thunder (and correlation)
par(mfrow = c(2, 2)) # Create a 2 x 2 plotting matrix
plot(Fwn ~ Adt, data=Thunder,
     main = 'Fawns vs Adult',
     pch = 20)
plot(Fwn ~ Prc, data=Thunder,
     main = 'Fawns vs. Precipitation',
     pch = 20)
plot(Fwn ~ Sev, data=Thunder,
     main = 'Fawns vs. severity',
     pch = 20)

#Correlation: Statistics and Graphics
pairs(Thunder)
Corr <- cor(Thunder)
round(Corr,2)

##################################################
### Create a Model                              ##
##################################################
#Simple Linear Regression
Tdr_Adt <- lm (Fwn ~ Adt,data = Thunder)
summary (Tdr_Adt)

#consider more features

#Considering all features
Tdr_APS <- lm(Fwn ~ Adt + Prc + Sev, data=Thunder )
summary(Tdr_APS)
Tdr_all <- lm(Fwn ~ ., data=Thunder )
summary(Tdr_all)





