---
  title: "175 Project"
author: "Kevin Ayala"
date: "11/13/2018"
output: html_document
---
library(survival)
library(tidyverse)
library(tree)
library(KMsurv)
library(ggplot2)

#Exploratory Data Analysis 
data("burn")
burn.dataset<- data("burn")
?burn

sum(burn$Z10) #number of patients with burn in throat
mean(burn$Z4) #avg body burn percentage is 24.69481
summary(burn) #5 point summary for all variables 

females.in.study <- sum(burn$Z2)

#excision


#time 1 
excision.km <-survfit(Surv(burn$T3, burn$D3)~Z1*D1, data = burn) 
plot(excision.km, xlab= "Time for Excison", ylab="Probability of Needing Excision", conf.int=TRUE,
     mark.time=TRUE, col = c(1:2), lwd = 2) #excision km
legend("topright", legend = c("yes","no"), fill = c(1:2))
excsion.cph 

plot(excisoin.gender, xlab="Time in Days till Excision",
     ylab="Probability of Surviving",
     main="Excision Gender Survival Function Comparison",
     col=c("green","purple"))
legend("top",legend=c("Male","Female"), col=c("green","purple"), pch=rep(19,2))

excisoin.gender <- survfit(Surv(burn$T1, burn$D1)~D2, data=burn)
summary(excisoin.gender)
excision.coxph <- coxph(Surv(T1,D1)~Z11,data=burn)
excision.coxph2 <- coxph(Surv(T1,D1)~.,data=burn)
excision.coxph2
anova(excision.coxph2)
step(excision.coxph2, direction = "backward")
### log log plot
excisioncox <- coxph(Surv(T1,D1)~ Z1 + Z2 + Z3 + Z6 + D1 + D2 , data = burn)
excisioncox

plot(survfit(excisioncox),fun = "cloglog", main ="Log Log Graph" , xlab="Time until death(in days)", ylab = "Log(-Log(S))")


## Z11 ( Type of Burn)
Typeofburn.km <- survfit(Surv(burn$T1, burn$D1) ~ Z11, data = burn)
plot(Typeofburn.km, xlab = "Time till Deaht", ylab = "Probability of Survival", main = "Survival between Burn Type (Excision)", col = c("purple", "blue", "yellow", "red"))
legend("top", legend=c("chemical", "scald", "electricity", "flame"), col = c("purple", "blue", "yellow", "red"), pch=rep(19,2))
log.rank.test.typeofburn <- survdiff(Surv(T1, D1)~Z11, data = burn)
log.rank.test.typeofburn
plot(Typeofburn.km, mark="+",lwd=2,col = c("purple", "blue", "yellow", "red"), fun="cloglog", xlab="Days", ylab="Log-Log S", main = "log log plot on type of burn")
legend(3,1,c("chemical", "scald", "electricity", "flame"),fill=c("purple", "blue", "yellow", "red"))


#time 2
Prophylacti.km <- survfit(Surv(burn$T2, burn$D2)~1) 
plot(Prophylacti.km, xlab="Time to Prophlactic Treatment", ylab="Probablilty of Survival ", col="green")

Prophylacti.km.gender <- survfit(Surv(T2,D2)~Z2, data=burn)
plot(Prophylacti.km.gender, xlab="Time Till Prophylacti Treatement", ylab ="Probablility of Prophylacti Treatment",
     main="Prophylacti Gender Comparison", col=c("red","pink"))
legend("middle", legend=c("Male, Female"), col=c("red","pink"), phc=rep(19,2)) #dont work lol, tired. fix later

Prophylacti.coxph<- coxph(Surv(T2,D2)~Z11, data=burn)
Prophylacti.coxph
Prophylacti.coxph2<- coxph(Surv(T2,D2)~., data=burn)
anova(Prophylacti.coxph2)
step(Prophylacti.coxph2, direction = "backward")

#time 3
straphylocous.km <- survfit(Surv(burn$T3, burn$D3)~1)
plot(straphylocous.km, xlab="Time to Straphylocous", ylab ="Probability of Getting Sttraphylocous Infection", col="blue")

straphylocous.km.gender <- survfit(Surv(T3, D3)~Z2, data=burn)

plot(straphylocous.km.gender, xlab="Time in Days till Staphylocous",
     ylab="Probability of Straphylocous",
     main="Straphylocous Gender Survival Function Comparison",
     col=c("green","purple"))
legend("top",legend=c("Male","Female"), col=c("green","purple"), pch=rep(19,2))

straphylocous.coxph<- coxph(Surv(T3, D3)~., data=burn)
anova(straphylocous.coxph)
step(straphylocous.coxph, direction ="backward")

###residuals time 3
rsds <- residuals(straphylocous.coxph,type = "schoenfeld")
hist(rsds,col = 5, xlab = "Residuals", main = "Residual", breaks = 20)
qqnorm(rsds)
qqline(rsds, col = "steelblue")


#gender question analysis
?burn
excision.km.ethnicity<- survfit(Surv(T1, D1)~Z3, data=burn)
summary(excision.km.ethnicity)
excision.km.gender
plot(excision.km.gender, xlab="Time till Excision", ylab="Probability of Excicision",
     main="Ethnicity on Excision", col=c("blue", "red"))

log.rank.test.ethnicity <- survdiff(Surv(T1, D1)~Z3, data=burn)
log.rank.test.ethnicity
#extremely high p-value=1, probs something wrong. 
#otherwise fail to reject null, conclude survival rate the same for ethnicities. 



#question regarding survival rates for different burn
#null is no difference. 
log.rank.test.burntype <- survdiff(Surv(T1, D1)~Z11, data=burn)
log.rank.test.burntype

#p-value at .04, means no difference in type of burn assuming i did this right

#log log Variable
Z1cox <- coxph(Surv(T1,D3)~ Z1, data = burn)
Z2cox <- coxph(Surv(T1,D3)~ Z2 , data = burn)
Z3cox <- coxph(Surv(T1,D3)~ Z3 , data = burn)
Z6cox <- coxph(Surv(T1,D3)~ Z6 , data = burn)
D1cox <- coxph(Surv(T1,D3)~ D1 , data = burn)
D2cox <- coxph(Surv(T1,D3)~ D2 , data = burn)

exp(confint(Z1cox,level = .95))
exp(confint(Z2cox,level = .95))
exp(confint(Z3cox,level = .95))
exp(confint(Z6cox,level = .95))
exp(confint(D1cox,level = .95))
exp(confint(D2cox,level = .95))


#log log Z1
plot(survfit(Z1cox,newdata=data.frame(Z1=factor(c("0", "1")))),
     fun = "cloglog", main ="Log Log Graph of Bathing Treatment" , 
     xlab="Time until death(in days)", ylab = "Log(-Log(S))", lwd = 2, col = c(1:2))
legend("bottomright", legend = c("Routine Bath", "Body Cleansing"), fill = c(1:2))

#log log Z2
plot(survfit(Z2cox,newdata=data.frame(Z2=factor(c("0", "1")))),
     fun = "cloglog", main ="Log Log Graph of Gender" , 
     xlab="Time until death(in days)", ylab = "Log(-Log(S))", lwd = 2, col = c(1:2))
legend("bottomright", legend = c("Male", "Female"), fill = c(1:2))

#log log Z3
plot(survfit(Z3cox,newdata=data.frame(Z3=factor(c("0", "1")))),
     fun = "cloglog", main ="Log Log Graph of Race" , 
     xlab="Time until death(in days)", ylab = "Log(-Log(S))", lwd = 2, col = c(1:2))
legend("bottomright", legend = c("Non-White", "White"), fill = c(1:2))

#log log z6
plot(survfit(Z6cox,newdata=data.frame(Z6=factor(c("1", "0")))),
     fun = "cloglog", main ="Log Log Graph of Burn Site Indicator: Buttock" , 
     xlab="Time until death(in days)", ylab = "Log(-Log(S))", lwd = 2, col = c(1:2))
legend("bottomright", legend = c("Yes", "No"), fill = c(1:2))

#log log D1
plot(survfit(D1cox,newdata=data.frame(D1=factor(c("1", "0")))),
     fun = "cloglog", main ="Log Log Graph of Excision Indicator" , 
     xlab="Time until death(in days)", ylab = "Log(-Log(S))", lwd = 2, col = c(1:2))
legend("bottomright", legend = c("Yes", "No"), fill = c(1:2))

#log log D2
plot(survfit(D2cox,newdata=data.frame(D2=factor(c("1", "0")))),
     fun = "cloglog", main ="Log Log Graph of Prophylactic Antibiotic" , 
     xlab="Time until death(in days)", ylab = "Log(-Log(S))", lwd = 2, col = c(1:2))
legend("bottomright", legend = c("Yes", "No"), fill = c(1:2))

### stratified
model <- coxph(Surv(T1,D3)~Z1 + Z2 + Z3 + Z6 + D1 + D2, data = burn)
D3Strata <- coxph(Surv(T1,D3)~ Z1 + Z2 + Z3 + Z6 + D1 + strata(D2) +
                    Z1*D1 + Z2*Z3 + Z2:D1 + Z2*strata(D2) +
                    Z3*strata(D2) , data = burn)
D3Strata  
model


#### linear regression
attach(burn)
pairs(D3~Z1 + Z2 + Z3 + Z6 + D1 + D2 + Z1:D1 + Z2:D1 + Z2:D1 + Z2:D2 + Z3:D2, main = "Scatterplot")
fitburn <- lm(D3~Z1 + Z2 + Z3 + Z6 + D1 + D2 + Z1:D1 + Z2:D1 + Z2:D1 + Z2:D2 + Z3:D2)
summary(fitburn)
plot(fitburn)
aasdasdas

#### Recurrent Model

### Subset D1 = 0
plot(survfit(Surv( burn$T3, burn$D3) ~ D1,
             data = burn, subset = (burn$D1 == "0")), lwd = 2, xlab = "Days", 
     ylab = "Survival Probability", col = 2:4, main = " No Extension to Infection")

plot(survfit(Surv( burn$T3, burn$D3) ~ strata(burn$Z1) + D1 ,
             data = burn, subset = (burn$D1 == "0")), lwd = 2, xlab = "Days", 
     ylab = "Survival Probability", col = 2:4, main = " No Extension to Infection")
legend("topright", legend = c("No Bathing", "Bathing"), fill= c(2:4) )

######################################################################################################################
#### Subset D1 =0 & D2 = 0
plot(survfit(Surv( burn$T3, burn$D3) ~ D1 + D2,
             data = burn, subset = (burn$D1 == "0" & burn$D2 == "0")), lwd = 2, xlab = "Days", 
     ylab = "Survival Probability", col = 2:4, main = "Extension & Antibiotic to Infection")
# strata bathing 
plot(survfit(Surv( burn$T3, burn$D3) ~ strata(burn$Z1) + D1 + D2,
             data = burn, subset = (burn$D1 == "0" & burn$D2 == "0")), lwd = 2, xlab = "Days", 
     ylab = "Survival Probability", col = 2:4, main = "Extension & Antibiotic to Infection")
legend("topright", legend = c("No Bathing", "Bathing"), fill= c(2:4) )

coxph(Surv( burn$T3, burn$D3) ~ Z1 + Z2 + Z3 + Z6 + D1 + D2 + Z1:D1 + Z2:D1 + Z2:D1 + Z2:D2 + Z3:D2,
      data = burn, subset = (burn$D1 == "0" & burn$D2 == "0"))
#########################################################################################################################
#### Subset D1 = 1
plot(survfit(Surv( burn$T3, burn$D3) ~ D1,
             data = burn, subset = (burn$D1 == "1")), lwd = 2, xlab = "Days", 
     ylab = "Survival Probability", col = 2:4, main = "Extension to Infection")
# Strata bathing 
plot(survfit(Surv( burn$T3, burn$D3) ~ strata(burn$Z1) + D1,
             data = burn, subset = (burn$D1 == "1")), lwd = 2, xlab = "Days", 
     ylab = "Survival Probability", col = 2:4, main = "Extension to Infection")
legend("topright", legend = c("No Bathing", "Bathing"), fill= c(2:4) )

coxph(Surv( burn$T3, burn$D3) ~ strata(burn$Z1) + D1 ,
      data = burn, subset = (burn$D1 == "0" & burn$D2 == "0"))
#####################################################################################################################################
#### Subset D1 =1 & D2 = 1
plot(survfit(Surv( burn$T3, burn$D3) ~ D1 + D2,
             data = burn, subset = (burn$D1 == "1" & burn$D2 == "1")), lwd = 2, xlab = "Days", 
     ylab = "Survival Probability", col = 2:4, main = "Extension & Antibiotic to Infection")
# strata bathing 
plot(survfit(Surv( burn$T3, burn$D3) ~ strata(burn$Z1) + D1 + D2,
             data = burn, subset = (burn$D1 == "1" & burn$D2 == "1")), lwd = 2, xlab = "Days", 
     ylab = "Survival Probability", col = 2:4, main = "Extension & Antibiotic to Infection")
legend("topright", legend = c("No Bathing", "Bathing"), fill= c(2:4) )


#####################################################################################################################################

### Subset D2 = 1
plot(survfit(Surv( burn$T3, burn$D3) ~ D2 ,
             data = burn, subset = (burn$D2 == "1")), lwd = 2, xlab = "Days", 
     ylab = "Survival Probability", col = 2:4, main = "Antibiotic to Infection")

#Strata bathing
plot(survfit(Surv( burn$T3, burn$D3) ~ strata(burn$Z1) + D2 ,
             data = burn, subset = (burn$D2 == "1")), lwd = 2, xlab = "Days", 
     ylab = "Survival Probability", col = 2:4, main = "Antibiotic to Infection")

legend("topright", legend = c("No Bathing", "Bathing"), fill= c(2:4) )




###  Due to our burn data set have mutiple events that can occur before an infection happen. We decided to use a recurrent model to test if
# events happening prior to our D3 have any effect on the survival rate of getting an infection. When we look at our survival rate when no extension occurs 
# when both extension and antibiotic treatment don't occur,the surival rate drops faster in a shorter amount of time. It hits around 50% survival rate
# between the 10 - 15 day mark. WHhen we check if  events such as extension antibiotic treament happened prior, the survival rate drop slower than coparted
## to when no events occur prior to infection. The the survival rate events happened before the infection drop to it's lowest rate at around betwen the
# 40 and 50 day range. The reason for this occuring is because the the events happening prior to the infection are meant to help reduce the infection from
## happening and help heal the burn


## We also checked if bathing routine had an effect on survival rate. For infection where events did happened prior, the subjects bathing had a higher
# suvival rate compared to its counter part. However, it's interesting to point out that when events didn't happened prior to the infection, no bathing had
## a higher survival rate. This could be due to the face that the events such as extension and antiobiotic are meant to help prevent the infection. IF
# those event didn't occur, there's more variability if the subject will get the infection if there was no other medical prevention except for bathing. 



