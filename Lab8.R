###################################
###--------Pygmalion Effect--------
###################################
# The data set consists of 29 observations for the following 3 variables.
#	Company:  a factor indicating company identification, with levels "C1", "C2",..., "C10"
#	Treat:  a factor indicating treatment with two levels, "Pygmalion"  and "Control"
#	Score:  average score on practical specialty test of all soldiers in the platoon

#install.packages("Sleuth")
# write.csv(case1302,file="case1302.csv", row.names=FALSE) # Create .csv file with the data
# data <- read.csv("case1302.csv",header=TRUE)

#install.packages("Sleuth3")

# Call data set
library(Sleuth3)
head(case1302)
case1302$Company <- as.numeric(case1302$Company)
case1302 <- case1302[order(case1302$Company),]
dim(case1302)
attach(case1302)

# Plot the data 
plot(as.numeric(Company),Score, pch=ifelse(Treat=="Pygmalion",17,16), col=ifelse(Treat=="Pygmalion","red","blue"))

# Boxplots
par(mfrow=c(1,2))

boxplot(Score~Company,main="Scores vs Company")
boxplot(Score~Treat, main="Scores vs Treatment")

# Interaction Plots
par(mfrow=c(1,1))
interaction.plot(Company,Treat,Score,col=ifelse(Treat=="Pygmalion","red","blue"))
interaction.plot(Treat,Company,Score)


#These plots are fine for your Homework but, I (Joe) think they are a little ugly. Here is how I would do it.
plot(1:10,Score[Treat=="Pygmalion"],type="l",col="forest green",lwd=2,ylim=c(63,90),xlab="Company",ylab="Score",at=c(1:10,seq(60,90,5)))
points(1:10,Score[Treat=="Pygmalion"],col="forest green",pch=8,cex=2)
means <- tapply(Score[Treat=="Control"],Company[Treat=="Control"],mean)
points(1:10,means,col="darkmagenta",type="l",lwd=2,lty=2)
points(1:10,means,col="darkmagenta",pch=11,cex=2)
legend("topright",c("Pygmalion","Control"),lty=1:2, pch=c(8,11), col=c("forest green","darkmagenta"))


# Non-additive model
non_add <- lm(Score ~ as.factor(Company) + Treat + as.factor(Company):Treat) # Fit with interaction.
summary(non_add)
anova(non_add)

plot(non_add, which=1) # Residual plot

# Additive model

add <- lm(Score ~ as.factor(Company) + Treat) # Fit without interaction
summary(add)
anova(add)

plot(non_add, which=1) # Residual plot
# Compare models
anova(add, non_add) # Show extra-ss-F-test p-value (for interaction effect).

# Only Treat effect (1-way ANOVA)
one <- lm(Score ~ Treat)
summary(one)
anova(one)

# Compare models
anova(one, add) # Test for Company effect.
summary(one) # Show estimate and p-value for Pygmalion effect.
anova(one)

# Confidence intervals for particular effect
confint(add,11) # Show 95% CI for Pygmalion effect after accounting for company effect.

detach(case1302)
################
