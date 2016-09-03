###################################################################################################################
#Studied a sample dataset (50000) of a cohort of new users activities since Jan 2014. Developed a survival model to understand and forecast how factors affect the user retain (active) rate, and to predict whether or not a user will be inactive in the 6th month on the system. Coded by R.
###################################################################################################################
library(KMsurv)
library(survival)
library(nlme)
library(crrSC)
data<-read.csv('/Users/Raines/Desktop/data.csv',header=TRUE) ## read Data
attach(data)
head(data[as.Date(data$last_trip_date,"%m/%d/%Y")>as.Date('6/1/14',"%m/%d/%Y"),]) # check the data
censor<-!(as.Date(data$last_trip_date,"%m/%d/%Y")>as.Date('6/1/14',"%m/%d/%Y")) # Define status (censor=1 inactive user if no activity after 6/1/14,indicate a failure occurred.)
time<-as.numeric(as.Date(data$last_trip_date,"%m/%d/%Y")-as.Date(data$signup_date,"%m/%d/%Y")) # Calculate user history usage length.
failure<-time*censor+as.numeric(as.Date("7/1/14","%m/%d/%Y")-as.Date(data$signup_date,"%m/%d/%Y"))*!censor # Modified Survival time, if a failure occurred, then failure=time, the history usage length ; otherwise failure=the last day of analysis(7/1/14)-signup_date.

raw.fit<-survfit(Surv(failure,censor) ~1, conf.type="none") # MH estimate. Nonparametric estimator for new user retain distribution
plot(raw.fit, xlab="Time", ylab="Survival Probability")

raw.fit1<-survfit(Surv(time,censor) ~1, conf.type="none")# MH estimate. Nonparametric estimator for new user retain distribution. This time use modified survival time
plot(raw.fit1, xlab="Time", ylab="Survival Probability",main="Survival Uber User")

mon<-floor(failure/30)# Recalculate the modified survival time in "month"
mon.fit<-survfit(Surv(mon,censor) ~1, conf.type="none")# MH estimate. Nonparametric estimator for new user retain distribution in "month".
plot(mon.fit, xlab="Time", ylab="Survival Probability",main="Survival Uber User")

### Simple t-test. Compare the "active" user group and "inactive" user group, see if any of these features play a role.
t.test(x=data[censor,]$surge_pct, y=data[!censor,]$surge_pct)
t.test(x=data[censor,]$weekday_pct, y=data[!censor,]$weekday_pct)
t.test(x=data[censor,]$avg_dist, y=data[!censor,]$avg_dist)
t.test(x=data[censor,]$avg_surge, y=data[!censor,]$avg_surge)
t.test(x=data[censor,]$avg_rating_of_driver, y=data[!censor,]$avg_rating_of_driver)
t.test(x=data[censor,]$avg_rating_by_driver, y=data[!censor,]$avg_rating_by_driver)
t.test(x=is.na(data[censor,]$avg_rating_of_driver), y=is.na(data[!censor,]$avg_rating_of_driver))
t.test(x=is.na(data[censor,]$avg_rating_by_driver), y=is.na(data[!censor,]$avg_rating_by_driver))
t.test(x=data[censor,]$trips_in_first_30_days, y=data[!censor,]$trips_in_first_30_days)
retain<-censor
# If feaures are category variables, using table summary. Compare the "active" user group and "inactive" user group.
table(data.frame(retain,data$city))
table(data.frame(retain,data$phone))
table(data.frame(retain,data$uber_black_user))
table(data.frame(censor,data$phone))


cau<-data[,-c(1,3,4,5,10)]
# Define missing parttern feature. From previous test, the missing parttern feature is a stronger factor for user retain time.
miss1<-is.na(data$avg_rating_of_driver)
miss2<-is.na(data$avg_rating_by_driver)
cau<-cbind(cau,miss1,miss2)
stra<-data[,c(1,5)]
# Use a simple weibull regression to explain the data and provide prediction of survival funtion (the user retain probability)
weibullfit<-survreg(Surv(time,censor)~cau$trips_in_first_30_days+cau$surge_pct+cau$uber_black_user+cau$weekday_pct+cau$avg_dist+cau$miss1+cau$miss2+strata(stra$city)+strata(stra$phone)+cau$trips_in_first_30_days*cau$miss2+cau$miss1*cau$miss2+cau$surge_pct*cau$uber_black_user+cau$miss1*cau$uber_black_user+cau$avg_dist*cau$miss1,dist = 'weibull')
summary(weibullfit)


-172774.