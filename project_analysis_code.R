#Import Data
waredata = read.table("C:/Users/Liam/Desktop/d. Fall 2020/STAT 512/Project/machine.data", sep=",")
colnames(waredata) [1:10] <- c('Vendor','Model','MYCT','MMIN','MMAX','CACH','CHMIN','CHMAX','PRP','ERP')

pairs(waredata[3:10])
lam_est = invTranEstimate(waredata$MYCT, waredata$ERP)
plot(waredata$MYCT^-1.115372,waredata$ERP)

#Data Processing
m1 = lm(ERP ~ MYCT + I(MYCT^-1.115372),data=waredata)#,weights = 1/MYCT^-1.115372)
yhat = predict(m1)

plot(waredata$MYCT, yhat)
points(waredata$MYCT,waredata$ERP,col='red')

m2 = lm(ERP ~ MYCT + I(MYCT^-1.115372) + MMIN,data=waredata)
summary(m2)

m3 = lm(ERP ~ MYCT + I(MYCT^-1.115372) + MMIN + MMAX,data=waredata)
summary(m3)

m4 = lm(ERP ~ MYCT + I(MYCT^-1.115372) + MMIN + MMAX + MMIN:MMAX,data=waredata)
summary(m4)

m5 = lm(ERP ~ MYCT + I(MYCT^-1.115372) + MMIN:MMAX,data=waredata)
summary(m5)

anova(m5, m4)

m6 = lm(ERP ~ MYCT + I(MYCT^-1.115372) + MMIN + MMAX + MMIN:MMAX + Vendor,data=waredata)
summary(m6)

m7 = lm(ERP ~ MYCT + I(MYCT^-1.115372) + MMIN + MMAX + CACH + CHMIN + CHMAX,data=waredata)
summary(m7)

#Best Model Candidate
m72 = lm(ERP ~ MYCT + I(MYCT^-1.115372) + MMIN + MMAX + CACH + CHMIN + CHMAX + MMIN:MMAX + CHMIN:CHMAX,data=waredata)
summary(m72)

#Best Model Candidate, possible overfitting: Don't include PRP because it is not an actual performance characteristic
m73 = lm(ERP ~ MYCT + I(MYCT^-1.115372) + MMIN + MMAX + CACH + CHMIN + CHMAX + PRP + MMIN:MMAX + CHMIN:CHMAX,data=waredata)
summary(m73)

#Best Model Candidate, without PRP, MYCT, or Intercept.
m74 = lm(ERP ~ 0 + MMIN + MMAX + CACH + CHMIN + CHMAX + MMIN:MMAX + CHMIN:CHMAX,data=waredata)
summary(m74)

anova(m74,m73)

m8 = lm(ERP ~ MYCT + MMIN + MMAX + CACH + CHMAX,data=waredata)
summary(m8)

anova(m1,m8,m7)
#Fitted MYCT and CHMIN do not seem to be as statistically significant as the other variables we keep for m8.

m9 = lm(ERP~PRP,data=waredata)
summary(m9)
anova(m9,m8)

m10 = lm(PRP~ERP,data=waredata)
m11 = lm(PRP~MYCT,data=waredata)
summary(m10)
summary(m11)


anova(m1,m2,m3,m4)



m3 = lm(ERP ~ MYCT+CACH + MMIN+MMAX, data=waredata)
m4 = lm(ERP ~ MYCT+CACH + MMIN+MMAX + MMIN:MMAX, data=waredata)
anova(m1,m2,m3,m4)