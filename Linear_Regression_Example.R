#Linear Regression and Correlation looking at data from Student Survey Data downloaded from StatKey
#Relationships between Weight and SAT, Pulse, BirthOrder, GPA and Exercise
#Individual linear regression plots and summary data for each is below

#Clear Environment
rm(list=ls())

#Import data
data.survey <- read.table("ssurvey.csv",sep=",",header=T)


#Pulling just the data needed
surveydata <- data.survey[,c(1,9,6,11,14,15,16)]
print(surveydata)

#Weight and Exercise
lm_ex <- lm(Weight ~ Exercise, data = surveydata)
plot(Weight ~ Exercise, data = surveydata)
abline(lm_ex)
summary(lm_ex)

#Weight and Pulse
lm_pulse <- lm(Weight ~ Pulse, data = surveydata)
plot(Weight ~ Pulse, data = surveydata)
abline(lm_pulse)
summary(lm_pulse)

#Weight and SAT
lm_sat <- lm(Weight ~ SAT, data = surveydata)
plot(Weight ~ SAT, data = surveydata)
abline(lm_sat)
summary(lm_sat)

#Weight and BirthOrder
lm_birth <- lm(Weight ~ BirthOrder, data = surveydata)
plot(Weight ~ BirthOrder, data = surveydata)
abline(lm_birth)
summary(lm_birth)

#Weight and GPA
## Note that p-value is close to one and R-squared is the largest of all data
## This does not imply causation though
lm_gpa <- lm(Weight ~ GPA, data = surveydata)
plot(Weight ~ GPA, data = surveydata)
abline(lm_gpa)
summary(lm_gpa)

#Multiple Regression
lm_all <- lm(Weight ~ Exercise+SAT+Pulse+BirthOrder+GPA, data = surveydata)
lm_all
summary(lm_all)
