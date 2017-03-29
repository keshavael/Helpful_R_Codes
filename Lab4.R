# Lab Four

# GOALS
# Estimated means, confidence intervals and prediction intervals

# Partial residual plots

###################
#### Fit Model ####
###################

library(Sleuth3)
?ex1030
head(ex1030)

# Let's say I've decided on the following model:
ex1030$EducationCategory <- reorder(ex1030$EducationCategory, ex1030$EducationCode)
fit_2.1 <- lm(log(WeeklyEarnings) ~ Region + Race:Region + EducationCode + I(EducationCode^2) + 
                Age + I(Age^2) + MetropolitanStatus, data = ex1030, na.action = na.exclude)

# The argument na.action = na.exclude is new for you. R automatically ignores observations
# with missing values (NAs) but this additionally tells R to return NA for residuals or
# predictions for these observation with missing values. There is no harm in always having it
# in your call to lm.

# The parameters on EducationCode and EducationCode^2 explain the relationship between mean
# log weekly earnings and education as a curved line, holding all the other variables constant.
# The easiest way to visualise the fitted relationship is to hold all the other variables
# constant, and plot the fitted mean log weekly earnings. The first step is to set up a
# new data.frame that describes all the explanatory values we are interested in.

newdf <- expand.grid(EducationCode = unique(ex1030$EducationCode), Race = "White", 
                     Region = "Northeast", MetropolitanStatus = "Metropolitan", Age = 40)
newdf

# expand.grid creates a data.frame from all combinations of the supplied variables.
# Alternatively, can use the data.frame() command:

uniq_edu_codes = unique(ex1030$EducationCode)
num = length(uniq_edu_codes)

newdf_alt <- data.frame(EducationCode = uniq_edu_codes, Race = rep("White", num),
                        Region = rep("Northeast", num),
                        MetropolitanStatus = rep("Metropolitan", num),
                        Age = rep(40, num))

# If we wanted the mean response for both white and black men,
# age 40 in a Metropolis in the Northeast, then we could do

newdf2 <- expand.grid(EducationCode = unique(ex1030$EducationCode),
                      Race = c("Black","White"), Region = "Northeast",
                      MetropolitanStatus = "Metropolitan", Age = 40)
newdf2

# We'll come back to this example later, but now let's stick with newdf.
# We can get the estimated mean log weekly earnings using the predict function

predict(fit_2.1, newdata = newdf)

##     1     2     3     4     5     6     7     8     9    10    11    12 
## 6.817 7.039 6.387 7.289 7.160 6.459 6.717 6.924 7.567 6.624 6.170 6.538 
##    13    14    15    16 
## 7.425 6.264 6.322 6.214 

# Remember the estimated mean response is the same as the predicted response.
# This just returns 16 numbers, so we could store them in a new column called pred
# in newdf then plot,

newdf$pred <- predict(fit_2.1, newdata = newdf)
newdf = newdf[order(newdf$EducationCode),] #sorts newdf by increasing order according to EducationCode

attach(newdf)
plot(EducationCode, pred, ylab = "Log Weekly Earnings",type='l')
detach(newdf)

# Or in ggplot2:
qplot(EducationCode, pred, data = newdf, geom = "line") + ylab("log Weekly earnings")

#####################################
#### Adding confidence intervals ####
#####################################

# Let's add some confidence intervals on the mean response. Compare the structure of these
# two commands (i.e. use str)

predict(fit_2.1, newdata = newdf)
predict(fit_2.1, newdata = newdf, interval = "confidence")

# In the second line, the interval = "confidence" argument asks for confidence intervals on
# the mean response to be returned as well as fitted values.
# The first command returns a vector of 16 numbers and that's why it can be saved driectly
# into a new column in newdf above. The second command returns a list of data.frame with 3
# columns. It doesn't make sense to store three columns in one column so instead we join it
# to our newdf data.frame with cbind (short for column bind)

cis <- predict(fit_2.1, newdata = newdf, interval = "confidence")
cbind(cis, newdf)
newdf <- cbind(cis, newdf)
head(newdf)

# Now we can use this newdf to plot our confidence interval on the mean response

newdf = newdf[order(newdf$EducationCode),]
attach(newdf)
plot(EducationCode, fit, type = "l")
lines(EducationCode, lwr, col = "red", lty = 2)
lines(EducationCode, upr, col = "red", lty = 2)
legend(x = "topleft", lty = c(1, 2), col = c("black", "red"),
       legend = c("Mean Resp", "95% C.I."))
detach(newdf)


# Or in ggplot2
qplot(EducationCode, fit, data = newdf, geom = "line") +
  geom_line(aes(y = upr), linetype = "dashed") +
  geom_line(aes(y = lwr), linetype = "dashed")

# Challenge:

# Try plotting the estimated mean response for white and black men, age 40 in a Metropolis
# in the Northeast by repeating the above steps with newdf2.

# If you want to see the estimated median response on the original scale just
# back-transform the mean and interval endpoints

attach(newdf)
plot(EducationCode, exp(fit), type = "l")
lines(EducationCode, exp(lwr), col = "red", lty = 2)
lines(EducationCode, exp(upr), col = "red", lty = 2)
legend(x = "topleft", lty = c(1, 2), col = c("black", "red"),
       legend = c("Mean Resp", "95% C.I."))
detach(newdf)


# Or in ggplot2
qplot(EducationCode, exp(fit), data = newdf, geom = "line") +
  geom_ribbon(aes(ymin = exp(lwr), ymax = exp(upr)), linetype = "dotted", alpha = I(0.2))

#####################################
#### Adding prediction intervals ####
#####################################

# Prediction intervals can be produced by using the interval = "prediction" argument in
# predict().

pis <- predict(fit_2.1, newdata = newdf, interval = "prediction")
head(pis)

# If you try to add the results of predict to newdf you'll run into a problem where you
# have multiple columns called fit, lwr and upr.
# One way around this is to rename the columns before cbinding them to newdf.

colnames(pis) <- c("pi_fit", "pi_lwr", "pi_upr")
newdf <- cbind(pis, newdf)
head(newdf)

# Then we can add them to our plot
attach(newdf)
plot(EducationCode, fit, type = "l", ylim = c(5,9))
lines(EducationCode, lwr, col = "red", lty = 2)
lines(EducationCode, upr, col = "red", lty = 2)
lines(EducationCode, pi_lwr, col = "blue", lty = 3)
lines(EducationCode, pi_upr, col = "blue", lty = 3)
legend(x = "topleft", lty = c(1, 2, 3), col = c("black", "red", "blue"),
       legend = c("Mean Resp", "95% C.I.", "95% P.I."))
detach(newdf)

# Or in ggplot2
qplot(EducationCode, fit, data = newdf, geom = "line") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), linetype = "dotted", alpha = I(0.2)) +
  geom_ribbon(aes(ymin = pi_lwr, ymax = pi_upr), alpha = I(0.2))

# Challenge:

# Plot the relationship between median weekly earnings and age for all four regions.

################################
#### Partial residual plots ####
################################

# Partial residuals for a variable are obtained by adding the estimated term for the
# variable to the residuals. We will first get all the terms in the model,
# these are the estimated coefficient multipled by the observed explanatory variable for
# all the observations,

m <- model.matrix(fit_2.1)
betas <- coef(fit_2.1)
terms <- m * matrix(betas, nrow = nrow(m), ncol = length(betas), byrow = TRUE)
head(terms)

# terms is a matrix with one row for every observation and one column for every term in our
# model. These correspond to the estimate for the parameter times the explanatory value ??^X.
# We can add these to the residuals to get partial residuals. Let's get partial residuals for
# EducationCode. Since both EducationCode and EducationCode2 are in the model we should add
# both terms to the residuals

ex1030$partial_educationCode <- residuals(fit_2.1) + terms[, "EducationCode"] + 
  terms[, "I(EducationCode^2)"]

attach(ex1030)
plot(EducationCode, partial_educationCode)
detach(ex1030)

# Or in ggplot2
qplot(EducationCode, partial_educationCode, data = ex1030, geom = "jitter") + 
  geom_smooth(method = "loess")