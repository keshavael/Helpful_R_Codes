# ===============================
# Lab 3 - Topics 
# ===============================

# 1) Exploratory Analysis
#   a) Visualization
#   b) Modelling Considerations (Log transforms, Interactions)
# 2) Indicator Data Analysis
#   a) Point estimates, p-values (one- and two-sided), CI's by hand
#   b) Type I Error, Power tradeoff
#   c) Make all pairwise comparisons
#   d) Introduce Sums of Squares F tests


# ===============================
# Part 1 - Exploratory Analysis 
# ===============================

# load data
library(Sleuth3)
?case1002
head(case1002)
str(case1002)

# plot data
library(ggplot2)
qplot(y = Energy, x = Mass, data = case1002, color = Type, shape = Type)

# Should we consider log transforms? Variance in energy tends to increase
# as mass increases - let's see if log transforing energy and mass helps.

qplot(y = log(Energy), x = log(Mass), data = case1002, color = Type, shape = Type)

# Now that assumptions are better, does a seperate lines model make sense?
# Do you want to explore that model? (Does the effect of log(mass) differ
# depending on animal type? Do the slopes differ between the animal types?)

# Seperate Lines Model:
# mu{log(Energy) | X} = B0 + B1*x1 + B2*x2 + B3*log(Mass) + B4*x1*log(Mass) + B5*x2*log(Mass) + epsilon
# where x1 = 1(non-echo bat), and x2 = 1(non-echo bird)

# Parallel Lines Model (different intercepts):
# mu{log(Energy) | X} = B0 + B1*x1 + B2*x2 + B3*log(Mass) + epsilon
# where x1 = 1(non-echo bat), and x2 = 1(non-echo bird)

# What is the predicted response for each animal type (using model equation above)?
# For echo-bats: mu-hat{log(Energy) | X} = B0hat + B3hat*log(Mass)
# For non-echo bats: mu{log(Energy) | X} = B0hat + B1hat + B3hat*log(Mass)
# For non-echo birds: mu{log(Energy) | X} = B0hat + B2hat + B3hat*log(Mass)


# ===============================
# Part 2 - Indicator Data Analysis 
# ===============================

# Fit parallel lines model
fit.par <- lm(log(Energy) ~ Type + log(Mass), data = case1002)
summary(fit.par) # error variance?
anova(fit.par)

# Some questions we can answer:


## 1) Do non-echo bats have a different mean log(Energy) than echo-bats, after
##    accounting for log(Mass)? Estimate this difference.

# Non-echolocating bats have an estimated 0.079 units lower log(Energy) expenditure
# compared to echolocating bats after accounting for log(mass).
# t-test with 16 degrees of freedom, p = 0.703, 95% CI: (-0.508,  0.351).

2*pt(-.388, 16, lower.tail=TRUE) #-.388 = -0.079 / 0.203
summary(fit.par)$coeff
summary(fit.par)$coeff[2,1] + c(-1,1) * qt(.975,16) * summary(fit.par)$coeff[2,2]
confint(fit.par)

# The p-value is the probability of Type I error, rejecting a true null.
# (If we were to reject Ho with p = p*, then we have p* prob of type I error.)
# Since p = .7, the probability of a Type I error is very large if we reject.


## 2) Do non-echo bats have a lower mean log(Energy) than echo-bats, after
##   accounting for log(Mass)? Estimate this difference. How does the answer here
##    differ from the previous question?

# No evidence to suggest that non-echo bats have a lower mean log(energy) than
# echo-bats, after accounting for log(mass). t-test with 16 degrees of freedom, p = 0.352.
# Produce the confidence interval.
pt(-.388, 16, lower.tail=TRUE)

# Notice that the two-sided p-value is double the size of the one-sided p-value.
# Two-sided tests are more "conservative" meaning that it is harder to reject Ho
# than the one-sided test, even though the test statistic (the observed effect)
# is the same. That is, probability of Type I error is smaller, but power (the
# ability to declare an effect significant) decreases.


## 3) Order the mean responses for each of the groups, accounting for log(mass).
summary(fit.par)$coeff[,"Estimate"]

# Notice that non-echo bats have lowest estimated mean response, then echo-bats,
# then non-echo birds. We know that non-echo bats are not statistically signifcantly
# different compared to echo-bats, and that echo-bats are not statistically
# significantly different compared to non-echo birds. Follow up question:


## 4) Do non-echo bats have a different mean log(Energy) than non-echo birds, after
##    accounting for log(Mass)? Estimate this difference.

# Notice we need to refit the model, since we only have the pairwise comparisons
# with echolocating bats as the reference group.

case1002$TYPE <- relevel(case1002$Type, ref = "non-echolocating bats")
fit.par2 <- lm(log(Energy) ~ TYPE + log(Mass), data = case1002)
summary(fit.par2) # error variance?
anova(fit.par2)

# There is no evidence to suggest that non-echo bats have a different mean response
# than non-echo birds (t test with 16 df, p = .384).


## 5) Confirm that under both parameterizations that the predicted responses
##    for each of the animal types is the same (i.e., the models are not different).

# Construct the fitted equations for each (or maybe 1 or 2) animal types under
# both parameterzations
summary(fit.par)
summary(fit.par2)
# We still have the same ordering under second model: non-echo bat, echo bat, bird.

# Fitted model 1 for bird: yhat = (-1.49770 + 0.02360) + 0.81496*log(Mass)
# Fitted model 2 for bird: yhat = (-1.57636 + 0.10226) + 0.81496*log(Mass)

# Notice sums of squares are the same for both models
anova(fit.par)
anova(fit.par2)


## 6) Should the indicator variables even be included? That is, can we just use...
fit.red <- lm(log(Energy) ~ log(Mass), data = case1002)

summary(fit.red)  # Get the output with t-tests and P-values
anova(fit.red)  # Get the ANOVA table for the reduced model

# We can answer this question with a hypothesis test (sums of squares F test).
# We want to know if we can just use the simpler model, or if considering
# the more complex model explains variability in response better.

# mu{log(Energy) | X} = B0 + B1*x1 + B2*x2 + B3*log(Mass) + epsilon
# mu{log(Energy) | X} = B0 + B3*log(Mass) + epsilon

# That is, we assume the simpler model through Ho: beta1=beta2=0.
# The alternative hypothesis is "not the null" or at least one of the betas is non-zero.

# Conduct the F test:

# Extra sum of squares
ESS <- anova(fit.red)["Residuals","Sum Sq"] - anova(fit.par)["Residuals","Sum Sq"]
# Estimate of the variance from the full model
sigma2 <- anova(fit.par)["Residuals","Mean Sq"]

# F-statistic to compare full vs reduced model
F_stat <- (ESS/2)/sigma2

# p-value, DON'T MULTIPLY THIS BY 2!!
pf(F_stat,2,16,lower.tail=FALSE)

# Or obtain automatically:
anova(fit.par,fit.red)