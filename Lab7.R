##Goals:
#
#Calculating AIC, BIC and Cp from a fitted model
#Using the leaps package for model selection
#Measuring the prediction accuracy of a model
#AIC, BIC and Cp
#
#The problem with calculating AIC, BIC and Cp, 
#is that different people drop different constants
#from the formulas. Since they are constant given 
#a particular set of data dropping them doesn't 
#change the ranking of the models, but it does 
#change the absolute values of the AIC, BIC and 
#Cp statistics. So, let's calculate the AIC, BIC
#and Cp following the formulas in Sleuth (and in
#the lecture slides).

#Let's use the example from last weeks Lab with state SAT scores,
#install.packages("Sleuth2")
library(Sleuth2)
head(case1201)

#Recall from last week the model that was selected using stepwise AIC included logtakers,Years, Expend and Rank
case1201$logtakers <- log(case1201$Takers)
attach(case1201)

fit1 <- lm(SAT ~ logtakers + Years + Expend + Rank,data = subset(case1201, State != "Alaska"))

#AIC and BIC only depend on Sum of Square Residual SSres from 
#this model. Mallow's Cp also requires sigma^2 from the
#full model, so we'll fit that too.

fit_full <- lm(SAT ~ logtakers + Income + Years + Public + Expend + Rank,data = subset(case1201, State != "Alaska"))

#We can either calculate SSres by summing the squared residuals

sum(residuals(fit1)^2)

#Or we can obtain the from the ANOVA table, 
anova(fit1)

#Let's the quantities we need from both models both models and store them

SSres <- anova(fit1)$"Sum Sq"[5] #The 5 is becuase it is the 5th value in the "Sum Sq" column

#remember the estimate of sigma^2 from the full model is the MSE of the full model
anova(fit_full)
sigma_sq_full <- anova(fit_full)$"Mean Sq"[7]
#Now we can plug directly into the formulas from Sleuth on page 364

n <- nrow(case1201)-1
p <- length(coef(fit1))

SSres/sigma_sq_full - n + 2*p  # Cp
n * log(SSres/n) + log(n)*(p+1)  # BIC
n * log(SSres/n) + 2*(p+1)  # AIC


#let's do this for one other model so we can compare them.
#Consider taking logtakers out of the model

fit2 <- lm(SAT ~ Years + Expend + Rank,data = subset(case1201, State != "Alaska"))
SSres2 <- anova(fit2)$"Sum Sq"[4]
n <- nrow(case1201)-1
p <- length(coef(fit2))
SSres2/sigma_sq_full - n + 2*p  # Cp
n * log(SSres2/n) + log(n)*(p+1)  # BIC
n * log(SSres2/n) + 2*(p+1)  # AIC

#Aside: I'm used to seeing the AIC and
#BIC defined in terms of the residual
#sum of squares divided by n - p (rather 
#than n). This does actually change
#the ordering. To be consistent with the
#Sleuth we are using that definition, but
#be aware other people's definitions may vary.
#
#Using the leaps package
#First you'll need to install the leaps package

#install.packages("leaps")
#The function that does the work finding the best
#models is regsubsets, as an example of it's use,
#here's how I found the best models in the SAT case study

library(leaps)
all <- regsubsets(SAT ~ Expend + logtakers + Income + Years + Public + Rank, 
data = subset(case1201, State != "Alaska"), nbest = 7, method = "exhaustive")

#The first argument specifies the largest model you are 
#willing to consider, here I specify my response is SAT,
#and the possible terms are all six explanatories. data
#should be pretty familiar by now, here I'm excluding
#Alaska. nbest specifies how many models should be returned 
#for each size, I want the best seven models. method 
#states the methods for finding the best models, exhaustive
#considers all models, other options are forward and 
#backward. Another useful argument is nvmax the largest
#size model you want to consider (by default it is set at
#8, so here we don't need to change it).
#
#The object that is returned is a bit complicated. 
#You can have a look at it with summary and plot

summary(all)
plot(all)

#Both of these outputs tell use which parameters are 
#included in each model, but not much else. 
#I've written a little function to pull out some other useful information
#install.packages("ggplot2")
library(ggplot2)
source(url("http://stat512.cwick.co.nz/code/fortify_leaps.r"))
models <- fortify(all)

head(models)

#It adds in rss, BIC and Cp (according to leaps definitions)
#and size as well as a short description of the model that 
#can be used to fit it later. The columns that match our 
#explanatories contain the parameter estimates for the model. 
#We can use this to make a Cp plot

plot(models$size, models$cp);abline(0,1)
plot(models$size, models$cp,ylim=c(0,10));abline(0,1)


#Or a BIC plot

plot(models$size, models$bic)

#If we like one model we can pick it out and fit it with lm. 
#For example the model with the lowest Cp,

models[models$cp == min(models$cp), ]

#includes Expend+logtakers+Years+Rank, we can fit that with lm by,

fit_1 <- lm(SAT ~ Expend + logtakers + Years + Rank, data = case1201)
summary(fit_1)


