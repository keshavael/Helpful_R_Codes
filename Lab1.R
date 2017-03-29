# Lab One

###############################################
#### Regression diagnostic plots ####
###############################################

# The results of a simple linear regression are only meaningful when the simple linear
# regression model is an appropriate description of the data of interest. I.e. the data
# satisfies all of the assumptions of simple linear regression. We can never prove the
# assumptions are satisfied but we can look for evidence assumptions are violated. We
# generally do this by looking at residual plots. (Remember a residual is the difference
# between the observed response and the response the regression model predicts).
# 
# We have three types of residuals plots:
# 
# Residuals against fitted values
# Residuals against explanatory values
# A normal probability plot for residuals
# 
# The first two look very similar with simple linear regression, but we'll see later in
# the quarter they can be quite different in multiple regression.
# 
# The dataset ex0824 from Sleuth2 contains the age and respiratory rate for 618 children.

library(Sleuth2)
library(ggplot2)
head(ex0824)

# Fit a linear regression model of Rate on Age and examine the three diagnostic plots.

D = ex0824
m1 = lm(Rate ~ Age, data = D)
summary(m1)

# Is there evidence any of the assumptions are violated?

D$res1 = residuals(m1)
D$fit1 = predict(m1)

# Diagnostic Plot 1: Residuals versus fitted values
plot(D$fit1, D$res1, xlab = "Fitted Values", ylab = "Residuals", cex = 0.8)
abline(h = 0)

# Or in ggplot2
qplot(fit1, res1, data = D) + geom_hline(aes(yintercept = 0))

# Diagnostic Plot 2: Residuals versus age
plot(D$Age, D$res1, xlab = "Age", ylab = "Residuals", cex = 0.8)
abline(h = 0)

# Or in ggplot2
qplot(Age, res1, data = D) + geom_hline(aes(yintercept = 0))

# Diagnostic Plot 3: Normal probability plot of residuals
qqnorm(D$res1); qqline(D$res1)

# Now try fitting a linear regression of log(Rate) on Age.

D$log.rate = log(D$Rate)
D$Age2 = (D$Age)^2 # Create quadratic term for practice
m2 = lm(log.rate ~ Age, data = D)
summary(m2)

# Do the residual plots look better?

D$res2 = residuals(m2)
D$fit2 = predict(m2)

# Diagnostic Plot 1: Residuals versus fitted values
plot(D$fit2, D$res2, xlab = "Fitted Values", ylab = "Residuals", cex = 0.8)
abline(h = 0)

# Or in ggplot2
qplot(fit2, res2, data = D) + geom_hline(aes(yintercept = 0))

# Diagnostic Plot 2: Residuals versus age
plot(D$Age, D$res2, xlab = "Age", ylab = "Residuals", cex = 0.8)
abline(h = 0)

# Or in ggplot2
qplot(Age, res2, data = D) + geom_hline(aes(yintercept = 0))

# Diagnostic Plot 3: Normal probability plot of residuals
qqnorm(D$res2); qqline(D$res2)

# How do you interpret the slope parameter now the response is log transformed?
# (Read Section 8.4 in Sleuth)

###############################
#### Calibrating your eyes ####
###############################

# You might find it useful to look at examples of these plots from data known to satisfy
# the assumptions. Have a look at: http://glimmer.rstudio.com/cwick/regression-plots/.

# I've generated data known to satisfy the assumptions of regression and made all the
# residual plots. Every few seconds a new dataset is simulated and plotted. You'll
# notice that even though the data satisfy the assumptions you still get residual plots
# that don't look "perfect".

# Change the sample size, n, (you'll have to hit the update button for the change to
# take effect) and reexamine the plots. You can also change whether the explanatory
# variable is discrete (x can only be an integer between 0 and 4) or continuous (x can
# be any value between 0 and 4).
                            