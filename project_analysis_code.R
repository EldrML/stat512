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
summary(m_prp)

#Create a null model
m_base = lm(ERP ~ 0, data=numdata)

#Create a scatter matrix of the data to look for relevant trends.
pairs(numdata)

#Appears the MYCT has a non-linear relation to PRP and ERP, perform a transformation estimate.
MYCT_Tran = with(waredata, invTranEstimate(MYCT, ERP))

#First Order Exhaustive Model Selection
m_1stOrd = lm(ERP ~ . + I(MYCT^MYCT_Tran[1]$lambda) - MYCT, data=numdata)
all_test2 = summary(regsubsets(formula(m_1stOrd), data = numdata))

#Exhaustive Model selection: Generate a full model of all potentially interesting regressors.
m_full = lm(ERP ~ . ^2 -1 + I(MYCT^MYCT_Tran[1]$lambda), data=numdata)
all_test = summary(regsubsets(formula(m_full), data = numdata))
all_test_r2 = all_test$adjr2 #Find the r^2 for each of the top 8 models of the data
#Top 8 regressors model from Exhaustive search
m_ex8 = lm(ERP ~ 0 + MMAX + MMIN + CACH + CHMAX + MYCT + I(MYCT^MYCT_Tran[1]$lambda) + MYCT:CHMAX + MMIN:MMAX + MMAX:CACH + MMAX:CHMAX + CACH:CHMAX, data = numdata)

#Perform 3 types of model selection to look for the best models.
m_step   = step(m_base, formula(m_full), direction="both")
m_back   = step(m_full, direction="backward")
m_for    = step(m_base, formula(m_full), direction="forward")

#Summarize all the models
summary(m_base)
summary(m_full)
summary(m_step)
summary(m_back)
summary(m_for)
summary(m_ex8)

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
bic_comp    = data.frame(BIC(m_base), 
                         BIC(m_full),
                         BIC(m_step),
                         BIC(m_back),
                         BIC(m_for),
                         BIC(m_ex8))

#Selecting the best model, performing ANOVA, and getting a summary.
m_best = m_ex8 #Choosing the simplest model because the difference between it and the more complex models in terms of R^2 is negligible.
anova(m_best)
summary(m_best)