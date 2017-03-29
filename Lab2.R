###### Multiple regression models in R

#Goals for today's lab * Understand how factors work in R, and how to specify
#indicator variables * Learn how to specify multiple regression models in R *
#Practice matching shorthand, longhand and code for models.

### Factors in R

#Factors are a special type of variable in R used to hold categorical variables.
#We've already seen lots of them. In ST411/511 all of our grouping variables were
#stored in R as factors. Have a look at the meadowfoam case study:

library(Sleuth2)
str(case0901)
levels(case0901$Time)

#The Time variable is a factor. It has two levels, R lingo for two categories. 
#Why are factors so useful? When we tell R to put a factor into a regression, 
#it automatically generates the indicator variables required for us! In class I
#generated my own indicator variable for early and then added it to the regression

case0901$early <- as.numeric(case0901$Time == "Early")
case0901$early
summary(lm(Flowers ~ Intens + early, data = case0901))

#But I could have just added the Time column (which is a factor) to the 
#regression formula,

summary(lm(Flowers ~ Intens + Time, data = case0901))

#Check the output is exactly the same. The TimeEarly row corresponds to the indicator
#that Time == "Early".

#There are two useful things to know how to do with factors. One, is create a factor
#from a numeric variable, and the other is to reorder the levels of the factor.

#Creating a factor is easy, simply use the factor function. Let's make a new
#intensity_f variable that is a factor version of Intensity.

case0901$intensity_f <- factor(case0901$Intens)
str(case0901$intensity_f)
levels(case0901$intensity_f)

#By default, the levels are put in alphanumerical order, sometimes this is what
#you want, sometimes it isn't. You can completely specify the order, by using the
#levels argument when you create the variable or you can assign a new baseline
#level with the relevel function.

case0901$intensity_f2 <- factor(case0901$Intens, levels = c("750", "300", "450", 
                                                            "150", "600", "900"))
levels(case0901$intensity_f2)
case0901$intensity_f3 <- relevel(case0901$intensity_f, ref = "300")
levels(case0901$intensity_f3)

### Fitting a multiple linear regression

#lm is the workhorse in R for fitting multiple regression models. We've already
#seen it for fitting simple linear regression models. Take the previous code as
#an example:

lm(Flowers ~ Intens + Time, data = case0901)

#The Flowers ~ Intens + Time part is called the formula. The nice thing about
#the Sleuth shorthand is that it closely corresponds to the formula in lm. The
#only trick is with indicator variables. Sleuth uses capital letters to indicate
#an collection of indicator variables. In R you need to either use a column that
#is already a factor or wrap the column name in factor to guarantee R treats it 
#as a factor.

#In Sleuth shorthand, the model:  mu{Flowers|INTENSITY, TIME}=INTENSITY+TIME
#corresponds to a model with indicator variables for intensity and time, 
#in lm we could fit this with

fit_1 <- lm(Flowers ~ factor(Intens) + Time, data = case0901)
summary(fit_1)

#Examine the output. How many parameters are fit? Notice that there
#is no coefficient for factor(Intens)150, we only need 5 indicator 
#variables to represent all 6 levels of Intensity. By default lm uses 
#the first level as a baseline. If we wanted the 600 level to be the 
#baseline, we would relevel intensity, i.e.

fit_2 <- lm(Flowers ~ relevel(factor(Intens), ref = "600") + Time, data = case0901)
summary(fit_2)

#Now there is no coefficient for 600. To keep the output tidy you might 
#prefer to make the intensity factor first then include it in the model

case0901$INTENS <- relevel(factor(case0901$Intens), ref = "600")
fit_3 <- lm(Flowers ~ INTENS + Time, data = case0901)
summary(fit_3)

### Adding interactions

#There are a number of ways to add interactions. The simplest is to let
#R do all the work and use : in the model formula to indicate an interaction.
#For example
fit_int <- lm(Flowers ~ Intens + Time + Time:Intens, data = case0901)
summary(fit_int)
#adds the interaction between Intensity and Time (remember Time is a factor)
#so this is the separate lines model,
# mu{Flowers|intensity, TIME}=intensity+TIME+intensity*TIME
# mu{Flowers|intensity, TIME}=beta0+beta1intensity+beta2early+beta3intensity*early

### Other terms

#You can add squared terms or any other calculated explanatories two ways.
#Either compute the new variable beforehand and add it to the regression, 
#or compute them in the formula (but you'll have to wrap them in I())

ex0915  # the corn yield data
head(ex0915)
# add a square term on the fly
lm(Yield ~ Rainfall + I(Rainfall^2), data = ex0915)
# or precalculate rainfall squared
ex0915$rainfall_sq <- ex0915$Rainfall^2
lm(Yield ~ Rainfall + rainfall_sq, data = ex0915)

### Exercise

#The ex0923 dataset in Sleuth3 contains observations on income, intelligence
#scores and years of educations.

library(Sleuth3)
head(ex0923)

#Match up the code, shorthand and full model representations of the following 
#multiple regression models relating logarithm of income to education and 
#gender variables.

#Code:

lm(log(Income2005) ~ Educ + Gender, data = ex0923)
lm(log(Income2005) ~ Educ + relevel(Gender, ref = "male"), data = ex0923)
lm(log(Income2005) ~ factor(Educ) + Gender, data = ex0923)
lm(log(Income2005) ~ Educ + Gender + Educ:Gender, data = ex0923)
lm(log(Income2005) ~ factor(Educ) + Gender + factor(Educ):Gender, data = ex0923)

#Shorthand:
# mu{log(Income)|GENDER, EDUC}=EDUC+GENDER
# mu{log(Income)|GENDER, educ}=educ+GENDER+GENDER*educ
# mu{log(Income)|GENDER, educ}=educ+GENDER
# mu{log(Income)|GENDER, EDUC}=EDUC+GENDER+GENDER*EDUC

#Full model:
# mu{log(Income)|Gender, Education}=beta0+beta1*educ7+beta2*educ8+beta3*educ9+beta4*educ10+...+beta14*educ20+beta15*male+beta16educ7*male+beta17educ8*male +...+beta29educ20*male
# mu{log(Income)|Gender, Education}=beta0+beta1*educ7+beta2*educ8+beta3*educ9+beta4*educ10+...+beta14*educ20+beta15*male
# mu{log(Income)|Gender, Education}=beta0+beta1*educ+beta2*male
# mu{log(Income)|Gender, Education}=beta0+beta1*educ+beta2*male+beta3*male*educ
# mu{log(Income)|Gender, Education}=beta0+beta1*educ+beta2*female

#Try running the code if you get stuck.