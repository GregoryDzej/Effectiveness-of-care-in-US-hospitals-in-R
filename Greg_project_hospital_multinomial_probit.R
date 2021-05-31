setwd("C:\\Users\\Grzesiek\\Desktop\\kaggle")

library(MASS)

hospital = read.csv(file="HospInfo.csv", header=TRUE, sep=",")

head(hospital, n =0)


hospitalsub<- subset(hospital, Effectiveness.of.care.national.comparison != "Not Available" )

table(hospitalsub$Effectiveness.of.care.national.comparison)

comparisonlvl<-factor(hospitalsub$Effectiveness.of.care.national.comparison,
                levels<-c("Same as the national average","Below the national average","Above the national average"),
                labels=c(0,1,2))

comparisonlvl

hospitalsub$Effectiveness.of.care.national.comparison

hospitalolm <- polr(as.factor(comparisonlvl) ~ Hospital.Type + Hospital.Ownership + Emergency.Services + Meets.criteria.for.meaningful.use.of.EHRs + Hospital.overall.rating + Mortality.national.comparison + Safety.of.care.national.comparison +  Patient.experience.national.comparison + Timeliness.of.care.national.comparison + Efficient.use.of.medical.imaging.national.comparison, data=hospitalsub, Hess=TRUE)

?polr
summary(hospitalolm)

hospitalolm.coef <- data.frame(coef(summary(hospitalolm)))
hospitalolm.coef$pval = round((pnorm(abs(hospitalolm.coef$t.value), lower.tail = FALSE) * 2),5)
hospitalolm.coef

hospitalolm2 <- polr(as.factor(comparisonlvl) ~  Ratinglvl + Efficientuselvl, data=hospitalsub, Hess=TRUE)

summary(hospitalolm2)

hospitalolm2.coef <- data.frame(coef(summary(hospitalolm2)))
hospitalolm2.coef$pval = round((pnorm(abs(hospitalolm2.coef$t.value), lower.tail = FALSE) * 2),5)
hospitalolm2.coef

Efficientuselvl<-factor(hospitalsub$Efficient.use.of.medical.imaging.national.comparison,
                      levels<-c("Same as the national average","Below the national average",
                                "Above the national average"),
                      labels=c(0,1,2))

Ratinglvl<-factor(hospitalsub$Hospital.overall.rating,
                        levels<-c(2,3,4,5),
                        labels=c(0,1,2,3))

hospitalsub$Efficient.use.of.medical.imaging.national.comparison[hospitalsub$Efficient.use.of.medical.imaging.national.comparison == "Not Available"] <- NA

hospitalsub$Hospital.overall.rating[hospitalsub$Hospital.overall.rating == "Not Available"] <- NA


table(hospitalsub$Efficient.use.of.medical.imaging.national.comparison)
# Relative risk ratios allow an easier interpretation of the logit coefficients. They are the
# exponentiated value of the logit coefficients

or.hospitalolm2 = exp(hospitalolm2.coef)
or.hospitalolm2

# Use "probs" for predicted probabilities
hospitalolm2.pred <- predict(hospitalolm2, type="probs")
summary(hospitalolm2.pred)

install.packages("erer")
library(erer)

MEhospitalolm2<-ocME(hospitalolm2)
?ocME
?polr
MEhospitalolm2
MEhospitalolm2$w
MEhospitalolm2$out

###pseudoR2stats
pR2(hospitalolm2)

?pR2
