##get help for a function by typing help(nameofafunction) into the console e.g.

help(mean)

#into the console

#read in the data into R

dat_all<-read.csv("dat_synthetic.csv")

###look at the top rows of the datasheet
head(dat_all)
#View the whole data sheets
View(dat_all)

##merge the two datasets if necessary
###dat_all<-merge(dat1, dat2, all = T)

#summary stats
summary(dat_all)

#calculate the mean
mean(dat_all$age)

#calculate the median
median(dat_all$age)

#calculate the sd
sd(dat_all$age)

#calculate percentile
quantile(dat_all$age, c(.025, .975))

#plot histogram
hist(dat_all$age)

##add mean
abline(v = mean(dat_all$age), col = "blue", lwd = 2)

##add sd
abline(v = mean(dat_all$age)-sd(dat_all$age), col = "lightblue", lwd = 1)
abline(v = mean(dat_all$age)+sd(dat_all$age), col = "lightblue", lwd = 1)

##add median
abline(v = median(dat_all$age), col = "red", lwd = 2)

##add quantiles
abline(v = quantile(dat_all$age, c(.025, .975))[1], col = "orange", lwd = 1)
##add quantiles
abline(v = quantile(dat_all$age, c(.025, .975))[2], col = "orange", lwd = 1)


hist(dat_all$age, probability = T)
lines(density(dat_all$age))


##get dplyr library for  %>% operator aka "pipe"
library(dplyr)

##group data by variable sex in order to look for differences in age depending on sex
dat_all%>%group_by(sex)%>%summarise(mean_age=mean(age), n=n())

##look at the data with a basic plot function
plot(dat_all$age[dat_all$sex!="k"]~dat_all$sex[dat_all$sex!="k"])

##calculate t-test for age between sex, in order to formally test for differences
t.test(dat_all$age[dat_all$sex!="k"]~dat_all$sex[dat_all$sex!="k"],var.equal=T)

#calculate linear model with sex as predictor and age as dependent variable
model<-lm(dat_all$age[dat_all$sex!="k"]~dat_all$sex[dat_all$sex!="k"])

#look at summary of linear model (check the t and p-value, it is exactly the same like the values in the t-test above)
summary(model)


##look at relation between age and pdiSum by plotting the data
plot(dat_all$age[dat_all$sex!="k"], dat_all$X2backRTFAmean[dat_all$sex!="k"])

##test for correlation between age and pdiSum
cor.test(dat_all$age[dat_all$sex!="k"], dat_all$X2backRTFAmean[dat_all$sex!="k"])

##calculate linear model
model<-lm(dat_all$X2backRTFAmean[dat_all$sex!="k"]~dat_all$age[dat_all$sex!="k"])

##calculate linear model and again look at the t- and p-value, sounds familiar?
summary(model)

##draw a line according to the model
abline(model)


##calculate predicted and residual values
dat_all$predicted[dat_all$sex!="k"] <- predict(model)   # Save the predicted values
dat_all$residuals[dat_all$sex!="k"] <- residuals(model) # Save the residual values
#check the error

##solution:
model<-lm(X2backRTFAmean~age,dat_all, na.action = na.exclude)
dat_all$predicted <- predict(model,na.action = na.exclude) # Save the predicted values
dat_all$residuals <- residuals(model, na.action = na.exclude) # Save the residual values



##plot predicted and residuals=actual-predicted (aka E=y-y^) in order to wrap your head around linear regression and the error term
library(ggplot2)
dat_all%>%ggplot(aes(age, X2backRTFAmean))+geom_point()+geom_smooth(method = "lm", se = FALSE, color = "lightgrey")+
  geom_point(aes(y=predicted), shape=1)+geom_segment(aes(xend = age, yend = predicted))



