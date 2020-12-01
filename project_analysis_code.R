#Import Data
waredata = read.table("C:/Users/Liam/Desktop/d. Fall 2020/STAT 512/Project/machine.data", sep=",")
colnames(waredata) [1:10] <- c('Vendor','Model','MYCT','MMIN','MMAX','CACH','CHMIN','CHMAX','PRP','ERP')

#Remove the data columns we are not interested in.
numdata = waredata
numdata$Model = NULL  #Categorical
numdata$Vendor = NULL #Categorical
numdata$PRP = NULL    #Our comparison regressor for the rest of the regressors.

#Create linear model that is our comparison model.
m_prp = lm(ERP~PRP,data=waredata)

#Create a scatter matrix of the data to look for relevant trends.
pairs(numdata)

#Appears the MYCT has a non-linear relation to PRP and ERP, perform a transformation estimate.
MYCT_Tran = with(waredata, invTranEstimate(MYCT, ERP))

#Exhaustive Model selection: Generate a full model of all potentially interesting regressors.
m_full = lm(ERP ~ . ^2 + I(MYCT^MYCT_Tran[1]$lambda), data=numdata)

#Find the r^2 for each of the top 8 models of the data
all_test = summary(regsubsets(formula(m_full), data = numdata))
all_test_r2 = all_test$adjr2
m_ex8 = lm(ERP ~ MMAX + CACH + I(MYCT^MYCT_Tran[1]$lambda) + MYCT:CHMAX + MMIN:MMAX + MMAX:CACH + MMAX:CHMAX + CACH:CHMAX, data = numdata)

#Perform 3 types of model selection to look for the best models.
m_step   = step(m_base, formula(m_full), direction="both")
m_back   = step(m_full, direction="backward")
m_for    = step(m_base, formula(m_full), direction="forward")

#Generate Comparison Dataframes
rsqr_comp   = data.frame(summary(m_base)$adj.r.squared, 
                         summary(m_full)$adj.r.squared,
                         summary(m_step)$adj.r.squared,
                         summary(m_back)$adj.r.squared,
                         summary(m_for)$adj.r.squared,
                         summary(m_ex8)$adj.r.squared)
length_comp = data.frame(length(coef(m_base)), 
                         length(coef(m_full)),
                         length(coef(m_step)),
                         length(coef(m_back)),
                         length(coef(m_for)),
                         length(coef(m_ex8)))
aic_comp    = data.frame(AIC(m_base), 
                         AIC(m_full),
                         AIC(m_step),
                         AIC(m_back),
                         AIC(m_for),
                         AIC(m_ex8))



p = length(coef(allTest))
n = length(resid(allTest))
aic = n * log(allTest$rss / n) + 2 * (2:p)

#lam_est = invTranEstimate(waredata$MYCT, waredata$ERP)
#plot(waredata$MYCT^-1.115372,waredata$ERP)

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
m731 = lm(ERP ~ MYCT + I(MYCT^-1.115372) + MMIN + MMAX + CACH + CHMIN + CHMAX + PRP,data=waredata)
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

##NEED TO INCLUDE MODEL SELECTION HERE
mtest = lm(PRP ~ ., data=waredata)
test = stepAIC(m731,direction="forward",criterion="AIC")

anova(m1,m2,m3,m4)


m3 = lm(ERP ~ MYCT+CACH + MMIN+MMAX, data=waredata)
m4 = lm(ERP ~ MYCT+CACH + MMIN+MMAX + MMIN:MMAX, data=waredata)
anova(m1,m2,m3,m4)