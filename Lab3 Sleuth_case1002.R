###################################
###------ECHOLOCATION PROBLEM------
###################################
# The data are on in–flight energy expenditure and body mass from 20 energy studies on three types
# of flying vertebrates: echolocating bats, non–echolocating bats and non–echolocating birds.
# The data set consists of 20 observations on the following 4 variables.
# Mass: mass (in grams)
# Type: a factor with 3 levels indicating the type of flying vertebrate: non-echolocating bats, 
#       nonecholocating birds, echolocating bats.
# Energy: in–flight energy expenditure (in W)

# install.packages("Sleuth3")
# write.csv(case1002,file="case1002.csv", row.names=FALSE) # Create .csv file with the data
# data <- read.csv("case1002.csv",header=TRUE)
library(Sleuth3)
head(case1002)
dim(case1002)
attach(case1002)

## Look at the scatterplot
plot(Mass,Energy,pch = ifelse(Type=="echolocating bats", 1,
ifelse(Type=="non-echolocating birds", 20, 2)), col = ifelse(Type=="echolocating bats", "blue",
ifelse(Type=="non-echolocating birds", "red", "darkgreen")))

legend(7, 45, pch=c(1, 20, 2), col=c("blue","red","darkgreen"),
c("Non-echolocating bats", "Non-echolocating birds","Echolocating bats"))

## Log-transform the variables

logEnergy <- log(Energy)
logMass <- log(Mass)

plot(logMass,logEnergy, xlab = "Body Mass (log-scale)",
ylab = "Energy Expenditure (log-scale)",
pch = ifelse(Type=="echolocating bats", 19,
ifelse(Type=="non-echolocating birds", 21, 24)))

legend("topleft", pch=c(24, 21, 19),
c("Non-echolocating bats", "Non-echolocating birds","Echolocating bats"))

## Define indicator ariables and fit the parallel lines model

bird <- as.numeric(Type=="non-echolocating birds")
ebat <- as.numeric(Type=="echolocating bats")

fit.par <- lm(logEnergy ~ logMass + bird + ebat)
# fit.par2 <- lm(logEnergy ~ logMass + Type) # Can use this command instead, but need to figure the refrence line

summary(fit.par)  # Get the output with t-tests and P-values
anova(fit.par)  # Get the ANOVA table to obtain estimate of variance and  degrees of freedom

## Get the P-value for the "ebat" by hand and confidence intervals by hand

pt(0.3881190,16,lower.tail=FALSE) # Multiply this number by 2 to get the P-value

qt(0.025,16,lower.tail=FALSE) # Multiplicative-factor for a 95% CI based on the t-distribution

coef(summary(fit.par))["ebat","Estimate"]  # Coefficient estimated value
coef(summary(fit.par))["ebat","Std. Error"]  # Coeficient standard error

low_lim <- coef(summary(fit.par))["ebat","Estimate"] - qt(0.025,16,lower.tail=FALSE)*coef(summary(fit.par))["ebat","Std. Error"]

upp_lim <- coef(summary(fit.par))["ebat","Estimate"] + qt(0.025,16,lower.tail=FALSE)*coef(summary(fit.par))["ebat","Std. Error"]

confint(fit.par, level=0.95)

## Compare "birds" with "echolocating bats"
bat <- as.numeric(Type=="non-echolocating bats")

bird_vs_ebat <- lm(logEnergy ~ logMass + bird + bat)  # Now ebat is reference line, so you can look at bird
summary(bird_vs_ebat)

## Fit the reduced model that does not include indicator variables
fit.red <- lm(logEnergy ~ logMass)

summary(fit.red)  # Get the output with t-tests and P-values
anova(fit.red)  # Get the ANOVA table for the reduced model

## Obtain F-statistic for "by hand" for H0: beta2=beta3=0

ESS <- anova(fit.red)["Residuals","Sum Sq"] - anova(fit.par)["Residuals","Sum Sq"] # Extra sum of squares
sigma2 <- anova(fit.par)["Residuals","Mean Sq"] # Estimate of the variance from the full model

F_stat <- (ESS/2)/sigma2  # F-statistic to compare full vs reduced model

pf(F_stat,2,16,lower.tail=FALSE) # DON'T MULTIPLY THIS BY 2!!

## You can also get F-statistic for model comparison using the anova function

anova(fit.par,fit.red)

detach(case1002)

######