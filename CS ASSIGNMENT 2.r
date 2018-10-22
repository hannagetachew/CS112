
#x = total hours of exercise 
#y = Longevity
x<- rnorm(100,1,5)
y<- 10+0.2*x+rnorm(100)
data.set<-data.frame(x,y)
plot(x, y, axes=FALSE, frame.plot=TRUE, xlab= "total hours of exercise", ylab="Longevity")
abline(lm(y ~ x, data=data.set), col="red")
a<-lm(y ~ x)
summary(a)
par(new=TRUE)
x[100]= - 200
y[100]= 90
data.set2<-data.frame(x,y)
b<-lm(y ~ x)
summary(b)
plot(x,y,xlim= c(-200, -20),ylim= c(1:90),xlab= "total hours of exercise", ylab="Longevity", col= "blue")
abline(lm(y ~ x, data=data.set2), col="blue",new=TRUE)

#ANSWER 2
library(Matching)
library(arm)
library()
data("lalonde")
newdata=lalonde[which(lalonde$treat==0),]

lmlalonde<-glm(newdata$re78 ~ newdata$age+newdata$educ+newdata$re74+newdata$re75+newdata$educ:newdata$re74+newdata$educ:newdata$re75+newdata$age:newdata$re74+newdata$age:newdata$re75+newdata$re74:newdata$re75)
summary(lmlalonde)
#lmlalonde$coefficients
Simulation <- sim(lmlalonde, n.sims = 10000)
Simulation@coef[1,5]


storage.vector <- rep(NA,10000)

m_educ=median(newdata$educ)
m_re74=median(newdata$re74)
m_re75=median(newdata$re75)


q90.edu<-quantile(newdata$educ,.90) 
q90.re74<-quantile(newdata$re74,.90) 
q90.re75<-quantile(newdata$re75,.90) 

summary(newdata$age)

#quantile(simulation@coef[,1], probs = c(0.025, 0.975))

storagedf<- matrix(NA, nrow = 10000, ncol= length(newdata$age))

for(i in 1:10000){
  
    predictedYs <- Simulation@coef[i,1] * 1 + 
      Simulation@coef[i,2] * newdata$age + 
      Simulation@coef[i,3] * m_educ + 
      Simulation@coef[i,4] * m_re74 +
      Simulation@coef[i,5] * m_re75 +
      Simulation@coef[i,6] * m_educ*m_re74 +
      Simulation@coef[i,7] * m_educ*m_re75 +
      Simulation@coef[i,8] * newdata$age*m_re74 +
      Simulation@coef[i,9] * newdata$age*m_re75 +
      Simulation@coef[i,10] * m_re74*m_re75 +
      rnorm(1,0,Simulation@sigma[i])
      
      
    # put preductedYs into a matrix
      storagedf[i,] = predictedYs
      
        
      #* c(1,newdata$age,m_educ,m_re74,m_re75,m_educ*m_re74,m_educ*m_re75,newdata$age*m_re74,newdata$age*m_re75, m_re74*m_re75))  
    
    
    #storagedf[i,age-16]<-exp(beta)/(1+exp(beta))
}

#confidence.intervals <- quantile(storagedf, probs = c(0.025, 0.975))
head(storagedf)

lowbounds.medians = rep(NA, length(newdata$age))
upperbounds.medians = rep(NA, length(newdata$age))

for (i in 1:ncol(storagedf)) {
  lowbounds.medians[i] = quantile(storagedf[,i],0.025)
  upperbounds.medians[i] = quantile(storagedf[,i],0.975)
  
}


plot(newdata$age, lowbounds.medians, ylim = c(-8000,18000), col= "blue",, xlab = "lower and upper bounds for median")
points(newdata$age, upperbounds.medians, col="red")

#Simulation@sigma

storagedf_2<- matrix(NA, nrow = 10000, ncol= length(newdata$age))

for(i in 1:10000){
  
  predictedYs_q<- Simulation@coef[i,1] * 1 + 
    Simulation@coef[i,2] * newdata$age + 
    Simulation@coef[i,3] * q90.edu + 
    Simulation@coef[i,4] * q90.re74 +
    Simulation@coef[i,5] * q90.re75 +
    Simulation@coef[i,6] * q90.edu*q90.re74 +
    Simulation@coef[i,7] * q90.edu*q90.re75 +
    Simulation@coef[i,8] * newdata$age*q90.re74 +
    Simulation@coef[i,9] * newdata$age*q90.re75 +
    Simulation@coef[i,10] * q90.re74*q90.re75 +
    rnorm(1,0,Simulation@sigma[i])
  
  
  # put preductedYs into a matrix
  storagedf_2[i,] = predictedYs_q
  
  

}

head(storagedf_2)

lowbounds.q90 = rep(NA, length(newdata$age))
upperbounds.q90 = rep(NA, length(newdata$age))

for (i in 1:ncol(storagedf)) {
  lowbounds.q90[i] = quantile(storagedf_2[,i],0.025)
  upperbounds.q90[i] = quantile(storagedf_2[,i],0.975)
  
}

plot(newdata$age, lowbounds.q90, ylim = c(-8000,18000), col= "blue")
points(newdata$age, upperbounds.q90, col="red")


Summary(lmlalonde)



head(data.frame(upperbounds.medians,lowbounds.medians,lowbounds.q90,upperbounds.q90,m_educ,m_re74,m_re75,q90.edu,q90.re74,q90.re75,newdata$age))

library(foreign)
getwd()
setwd("C:/Users/Hanna/Downloads")
mydata<-read.dta("nsw.dta")
cof_storage = rep(1:10000)
for (i in 1:10000) { boot.sample = sample(1:nrow(mydata), nrow(mydata), replace= TRUE)
 newset<-mydata[boot.sample,]
 mb<-lm(re78~ treat,data = newset)
 cof_storage[i]<-mb$coef[2]
  }
quantile(cof_storage, c(0.25,0.975))
manual<-quantile(cof_storage, c(0.25,0.975))
#analytical
ma<-lm(mydata$re78~ mydata$treat)
confint(ma)
analytical<-confint(ma)[2,]

table(analytical,manual)

preds <- cof_storage[c(1:722)] #taking the first 722 values for the example
actual <- mydata$re78
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
print(rsq)
#shows how much better the predictive model is to the mean model.

mydata<- mydata[, -1]
glm.fit= glm(treat~age+education+black+hispanic+married+nodegree+re75, data= mydata, family= "binomial")
summary(glm.fit)

Probability_distribution_for_treatment_group<- predict (glm.fit,type = "response")[which(predict(glm.fit, mydata,type="response")>0.5)]
hist(Probability_distribution_for_treatment_group,col="red")
predict (glm.fit ,mydata,type="response")
predicted_Y<- rep(0,722)
predicted_Y[which(predict(glm.fit,type = "response")>0.5)] <- 1

Probability_distribution_for_control_group<- predict (glm.fit,type = "response")[which(predict(glm.fit, mydata,type="response")<0.5)]
hist(Probability_distribution_for_control_group, col = "blue")
length(which(predict(glm.fit,type = "response")>0.5))
