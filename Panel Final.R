rm(list=ls())
cat("\f")  

library(plm)
library(stargazer)
library(car)
library(lmtest)
library(gmm)
library(AER)



#load the dataset
data<- read.csv("Panel.csv", header = TRUE)
unique(data$year)

library(fastDummies)
data<-cbind(data, dummy_cols(year))
pdim(data)

data.panel<-pdata.frame(data, index = c("state", "year"))
pdim(data.panel)

data.panel$linc = log(data.panel$inc)

#Different models
#Pooled OLS
attach(data.panel)
pols<- plm(gradrte ~ internet + stutea + linc+ factor(year),
           data=data.panel, model = "pooling")

pols1<- lm(gradrte ~ internet + stutea + linc+ factor(year),
           data=data)
#RE
re<- plm(gradrte ~ internet + stutea + linc + factor(year),
         data=data.panel, model = "random")
#FD
fd<- plm(gradrte ~ internet + stutea + linc+ factor(year),
         data=data.panel, model = "fd")
#FE
fe<- plm(gradrte ~ internet + stutea + linc+ factor(year),
         data=data.panel, model = "within")

stargazer(pols1, fd, fe, type= "text", 
          keep.stat = c("n","rsq"), omit=c("year"),omit.labels=c("Year Dummies"), 
          title = "Pooled OLS, FD, FE")


#Which estimator is good
bptest(pols1)

#reject the null, therefore there is variance in 
#the unobserved heterogeniety, hence we cannot consider Pooled OLS as a model

# Serial correlation test among the errors
data<-na.omit(data)
data.panel<- na.omit(data.panel)
data$pols1.res<-pols1$residuals


data$pols1.lres<-lag(data$pols1.res)
sc.pols1<-lm(pols1.res~pols1.lres, data=data)
summary(sc.pols1)
#reject the null, hence Pooled OLS is dropped

# FE VS RE
phtest(fe,re)

# FE VS FD
pwfdtest(fd)
#reject the null, serial correlation among differenced errors
#FE is preferred

# FE Serial Correlation Test
pwartest(fe)

coeftest(fe, vcovHC(fe, method="arellano", type="HC0"))
#Differenced and lag variables
data.panel$linc = log(data.panel$inc)
data.panel$D.gradrte<- diff(data.panel$gradrte)
data.panel$D.internet<- diff(data.panel$internet)
data.panel$D.stutea<- diff(data.panel$stutea)
data.panel$D.linc<- diff(data.panel$linc)
data.panel$L.gradrte<- lag(data.panel$gradrte)
data.panel$DL.gradrte<- diff(data.panel$L.gradrte)
data.panel$L2.gradrte<- lag(data.panel$L.gradrte)

#Anderson-Hsiao Estimator
attach(data.panel)
AH_2SLS <- ivreg(D.gradrte~DL.gradrte+D.internet+D.stutea+D.linc+ factor(year)-1
               |L2.gradrte + D.internet+D.stutea+D.linc+ factor(year)-1, data=data.panel)
summary(AH_2SLS, diagnostics = TRUE)
#Weak instruments, reject the null hypothesis, model doesn't have weak IV
#Wu-hausman, reject the null hypothesis, model has endogeniety

#Arellano and Bond Estimator, Two-step
AB_GMM <- pgmm(gradrte ~ lag(gradrte) + internet + stutea + linc | lag(gradrte, 2:99),
               data = data.panel, effect = "twoways", model = "twostep")
summary(AB_GMM, time.dummies=TRUE)
#fail to reject, orthogonality condition is satisfied
#AR 2, fail to reject, no autocorrelation