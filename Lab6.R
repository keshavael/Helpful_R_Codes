#######################################
###----State Averagre SAT Scores----###
#######################################
# The data set consists of 50 observations measured on the following variables:
#	State: US state
#	SAT: state averages of the total SAT (verbal + quantitative) scores
#	Takers: the percentage of the total eligible students (high school seniors) in the state who took the exam
#	Income: the median income of families of test–takers (in hundreds of dollars)
#	Years: the average number of years that the test–takers had formal studies in social sciences, natural sciences	#	and humanities
#	Public: the percentage of the test–takers who attended public secondary schools
#	Expend: the total state expenditure on secondary schools (in hundreds of dollars per student)
#	Rank: the median percentile ranking of the test–takers within their secondary school classes

# install.packages("Sleuth")
# install.packages("leaps")
# write.csv(case1201,file="case1201.csv", row.names=FALSE) # Create .csv file with the data
# data <- read.csv("case1201.csv",header=TRUE)

# Call required librraies
library(Sleuth3)
library(MASS)
library(leaps)

# Load data set
head(case1201)
dim(case1201)
attach(case1201)

# Matrix of scatter-plots
pairs(~SAT+Takers+Income+Years+Public+Expend+Rank,pch=20, col="blue")
#Looking at this plot it appears there are several variables related to SAT. It also looks like 
#there may be some curvature in the relationship between Takers and SAT and there are potential 
#outliers in the variables Public and Expend

# Check possible curvature in data
plot(Takers,SAT,pch=20, col="blue")
case1201$logTakers <- log(Takers)
detach(case1201)
attach(case1201)
plot(logTakers,SAT,pch=20, col="blue")

# Check possible influential points
State[Public < 50] # Identify state with low Public 
State[Expend > 40] # Identify state with high Expend 

# Start with tentative model (all variables)
mod1 <- lm(SAT ~ logTakers + Income+ Years + Public + Expend + Rank)
summary(mod1)

# Check residuals
plot(mod1,which=1) # Residuals
plot(mod1,which=4) # Cook’s Distance
State[29] # Identify State number 29

std_res <- studres(mod1) # studentozed residuals
plot(1:50,std_res, pch=20, col="blue") 
identify(1:50,std_res,labels=case1201$State)


# Re-fit the model removing "Alaska"
mod2 <- update(mod1, . ~ . ,subset=(State != "Alaska"))
summary(mod2)

# Check new residuals
plot(mod2,which=1) # Classic residual plot
plot(mod2,which=4) # Cook’s Distance

std_res <- studres(mod2) # studentozed residuals
plot(1:49,std_res, pch=20, col="blue") 
identify(1:49,std_res,labels=case1201$State[c(1:28,30:50)])

# Stepwise regression
seq_fit <- lm(SAT ~ logTakers + Income+ Years + Public + Expend + Rank, data=subset(case1201,State != "Alaska"))
step <- stepAIC(seq_fit, direction="both")
step$anova # display results

# Explore all possible models
all <- regsubsets(SAT ~ logTakers + Income+ Years + Public + Expend + Rank, nvmax=7, nbest=3, data=subset(case1201,State != "Alaska"))
summary(all)
allresults <- summary(all)
p <- apply(allresults$which, 1, sum)
plot(p, allresults$cp, pch=20, col="blue")
abline(a=0,b=1,col="blue")
cbind(p,allresults$cp)
allresults$which[which.min(allresults$cp),]	

	
final_fit <- lm(SAT ~ logTakers + Years + Expend + Rank, data=subset(case1201,State != "Alaska"))
summary(final_fit)

detach(case1201)
###



