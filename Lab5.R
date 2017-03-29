#################################
# LAB 4: Influence and Outliers #
#################################


################################################
# "The human brain is protected from bacteria and 
#  toxins, which course through the blood???stream, 
#  by a single layer of cells called the blood???brain 
#  barrier. These data come from an experiment 
#  (on rats, which process a similar barrier) to 
#  study a method of disrupting the barrier by 
#  infusing a solution of concentrated sugars."
#                  - R-documentation
################################################

library(Sleuth2)
head(case1102)

#Store the data 
data <- case1102

#Set up response variable
data$Antibody <- log(data$Brain/data$Liver)

#Change sacrifice time  and days from continuous to categorical.
data$Sac <- as.factor(data$Time)
data$Days <- as.factor(data$Days)

#Fit model
mod <- lm(Antibody ~ Sac*Treat + Days + Sex + Weight + Loss + Tumor, data=data)

#Check the studentized residuals
res <- residuals(mod)
fit <- fitted(mod)
plot(fit,res);abline(0,0,lty=2)

#Overlay the row number of observations
plot(fit,res,cex=0);abline(0,0,lty=2);text(fit,res)

#Notice: 31 and 34 looks like unusual observations
#It is necessary to check other measures influence



#One "easy" way to do this is to use the following command.
####IMPORTANT: click your mouse in the console below after
####           running this and then press [Enter] to go
####           through each plot.
plot(mod) 




#Here is how we can get our own plots of our own of three 
#important measures.

#1. Studentized Residuals
rstud <- rstudent(mod)
plot(1:34, rstud,cex=0);text(1:34,rstud);abline(0,0,lty=2);abline(2,0);abline(-2,0)

#2. Cook's Distances
cooks <- cooks.distance(mod)
plot(1:34, cooks,cex=0);text(1:34,cooks)

#3. Leverage ("hat" values)
lever <- hatvalues(mod)
plot(1:34, lever,cex=0);text(1:34,lever)

#The Studentized residuals and Cook's Distances indicate that
#the 31st and 34th observation are quite influential. The 
#leverage of 31 and 34 looks OK.

#Let's say we decided to drop points 31 and 34 from the model.
data2 <- data[-c(31,34),]

#Fit model
mod2 <- lm(Antibody ~ Sac*Treat + Days + Sex + Weight + Loss + Tumor, data=data2)

#Check the studentized residuals
res2 <- residuals(mod2)
fit2 <- fitted(mod2)
plot(fit2,res2);abline(0,0,lty=2)

#Looks much better.

#Now look at the model:
summary(mod2)

#Examine the p-values of the coefficents and notice that many
#of the covariates are not significant.

#Fit a new model
mod3 <- lm(Antibody ~ Sac + Treat + Loss, data=data2)

#Examine the output
summary(mod3)

#Perform F-test
anova(mod3,mod2)

#The p-value of 0.2515 gives us no evidence that the more 
#complicated model is signficantly better than the simpler model.
#Conclude mod3 fits the data better than mod2.


### This is just one iteration of a process than can take quite awhile.
### At this point it is important to examine the residuals again and
### make sure there are no more outliers or overly influential points.


