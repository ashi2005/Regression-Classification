#installing package 'readxl' to import excel file
#calling the packages
library("readxl")
library(ggplot2)
#windows()
#storing ENB2012_data in the variable ds
ds=read_excel("C:/Users/ashi/Downloads/ENB2012_data.xlsx")
str(ds)
summary(ds)
attach(ds)

#detecting missing values
sum(is.na(ds))
# no missing values

#response variables
Y1=Y1
Y2=Y2
hist(Y1,col="red",breaks=10,main="HISTOGRAM OF Heating 
loads",labels=T,xlab="(Y1)")
hist(Y2,col="blue",breaks=10,main="HISTOGRAM OF Cooling 
loads",labels=T,xlab="(Y2)")
#explanatory variables
x=cbind(X1,X2,X3,X4,X5,X6,X7,X8)
#combining response and explanatory variables
z<-cbind(Y1,X1,X2,X3,X4,X5,X6,X7,X8)
#correlation b/w variables
r=cor(ds)
r
#to estimate regression coefficients
M1<-lm(Y1~X1+X2+X3+X4+X5+X6+X7+X8)
M2<-lm(Y2~X1+X2+X3+X4+X5+X6+X7+X8)
M1
M2
# anova-table
anova(M1)
anova(M2)

# final model
M1<-lm(Y1~X1+X2+X3+X5+X7+X8)
M2= lm(Y2~X1+X2+X3+X5+X7)
# variable x2 and x4 are perfectly colinear with each other(and also with 
#(the dependent variable)which is why x4 is always dropped depending upon 
# the order in the formula
ss<-summary(M1)
ss
ss<-summary(M2)
ss
Aov1<-aov(Y1~X1+X2+X3+X5+X7+X8)
Aov2<-aov(Y2~X1+X2+X3+X5+X7)
resi1<-residuals(Aov1)#to find residual values
resi2<-residuals(Aov2)
y1.hat<-fitted.values(Aov1) #to find fitted values of y
y2.hat<-fitted.values(Aov2)


layout(matrix(c(1,2,3,4),2,2))
plot(M1)
plot(M2)

ggplot() +
        geom_point(aes( Y1 , y1.hat),
                   colour = 'red') +
        geom_smooth(aes( Y1 , y1.hat))+
        ggtitle('FITTED VS ACTUAL HEATING LOAD') +
        xlab('ACTUAL') +
        ylab('FITTED')

ggplot() +
        geom_point(aes( Y2 , y2.hat),
                   colour = 'red') +
        geom_smooth(aes( Y2 , y2.hat))+
        ggtitle('FITTED VS ACTUAL COOLING LOAD') +
        xlab('ACTUAL') +
        ylab('FITTED')


#checking for normality of residuals
layout(matrix(c(1,2),ncol = 2))
hist(resi1,col="red",breaks=10,main="HISTOGRAM OF RESIDUALS:HEATING LOAD",labels=T,xlab="residuals(Y1)")
hist(resi2,col="blue",breaks=10,main="HISTOGRAM OF RESIDUALS:COOLING LOAD",labels=T,xlab="residuals(Y2)")

#to test the auto-correlation
library(lmtest)
dwtest(M1)# Durbin Watson Test
library(lmtest)
dwtest(M2)

# Breusch-Pagan test
# Homoscedasticity: the errors have constant variance about the true model
# Heteroscedasticity: the errors have non-constant variance about the true model
bptest(M1)
bptest(M2)

#test for normality of residuals
ks.test(resi1,"pnorm",m=mean(resi1),sd=sd(resi1))
ks.test(resi2,"pnorm",m=mean(resi2),sd=sd(resi2))

#2) rounding Y1 TO nearest integer
Y1= round(Y1)
z=cbind(Y1,X1,X2,X3,X4,X5,X6,X7,X8)

#Feature Scaling
w1= scale(z)
set.seed(2)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(w1, i)$withinss)
wcss
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

Y2= round(Y2)
z=cbind(Y2,X1,X2,X3,X4,X5,X6,X7,X8)

#Feature Scaling
w2= scale(z)
set.seed(2)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(w2, i)$withinss)
wcss
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')



# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(x = w1, centers = 2)
y1_kmeans = kmeans$cluster


set.seed(29)
kmeans = kmeans(x = w2, centers = 2)
y2_kmeans = kmeans$cluster
z=cbind(y1_kmeans,y2_kmeans)

# Visualising the clusters
library(cluster)
clusplot(w1,
         y1_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 4,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of buildings: Heating load'))
library(cluster)
clusplot(w2,
         y2_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 4,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of buildings: Cooling load'))


#3) Logistic Regression(classification problem)
# HEATING LOAD
library(nnet)
library("readxl")
ds=read_excel("C:/Users/ashi/Downloads/ENB2012_data.xlsx")
ds=ds[1:9]

str(ds)
attach(ds)

ds$Y1=y1_kmeans
ds$Y1=as.factor(ds$Y1)
str(ds)

set.seed(222)
ind=sample(2,nrow(ds),
           replace=T,
           prob=c(0.6,0.4))
training=ds[ind==1,]
test=ds[ind==2,]
training$Y1=relevel(training$Y1,
                    ref="1")
dsmodel=multinom(Y1~.-1,
                 data=training)
summary(dsmodel)

#2 tailed z test
z=summary(dsmodel)$coefficients/summary(dsmodel)$standard.errors
p=(1-pnorm(abs(z),0,1))*2
p
#TRAININGSET RESULTS FOR Y1
#Confusion matrix
p=predict(dsmodel,training)
p
tab=table(p,training$Y1)
tab

#ACCURACY
sum=sum(diag(tab))/sum(tab)
sum #model classification percentage
1-sum # model mis-classification percentage

#MODEL SENSITIVITY WITHIN
n=table(training$Y1)
n
per=n/sum(n)
per
modelper=tab/colSums(tab)
modelper


#TEST SET RESULTS FOR Y1
#CONFUSION MATRIX
p1=predict(dsmodel,test)
tab1=table(p1,test$Y1)
tab1
#TEST SET ACCURACY
sum1=sum(diag(tab1))/sum(tab1)
sum1
1-sum1 
# model sensitivity(within )
n=table(test$Y1)
n
n/sum(n)
tab1/colSums(tab1)

#COOLING load
library(nnet)
library("readxl")
ds=read_excel("C:/Users/ashi/Downloads/ENB2012_data.xlsx")
ds=ds[-9]

str(ds)
attach(ds)
ds$Y2=y2_kmeans

ds$Y2=as.factor(ds$Y2)
str(ds)

set.seed(222)
ind=sample(2,nrow(ds),
           replace=T,
           prob=c(0.6,0.4))
training=ds[ind==1,]
test=ds[ind==2,]
training$Y2=relevel(training$Y2,
                    ref="1")
dsmodel=multinom(Y2~.-1,
                 data=training)
summary(dsmodel)

#2 tailed z test
z=summary(dsmodel)$coefficients/summary(dsmodel)$standard.errors
p=(1-pnorm(abs(z),0,1))*2
p

#TRAINING SET RESULTS FOR Y2
#confusion matrix
p=predict(dsmodel,training)
head(p)# first 6 predicted
head(training$Y2) # first 6 real
tab=table(p,training$Y2)
tab
#ACCURACY
sum=sum(diag(tab))/sum(tab)
sum #model classification percentage
1-sum # model mis-classification percentage

#MODEL SENSITIVITY WITHIN
n=table(training$Y2)
n
per=n/sum(n)
per
modelper=tab/colSums(tab)
modelper

#TEST SET RESULTS
#CONFUSION MATRIX
p1=predict(dsmodel,test)
tab1=table(p1,test$Y2)
tab1
#ACCURACY
sum1=sum(diag(tab1))/sum(tab1)
sum1
1-sum1 

# model sensitivity(within)
n=table(test$Y2)
n/sum(n)
tab1/colSums(tab1)


count1=table(ds$X1)
count2=table(ds$X2)
count3=table(ds$X3)
count4=table(ds$X4)
count5=table(ds$X5)
count6=table(ds$X6)
count7=table(ds$X7)
count8=table(ds$X8)
count9=table(ds$Y1)
count10=table(ds$Y2)

layout(matrix(c(1,2,3,4,5,6,7,8),ncol=4))
barplot(count1,main="Bar plot for X1(Relative compactness)",xlab="possible values of x1",ylab="no. of buildings.")
barplot(count2,main="Bar plot for X2(Surface area)",xlab="possible values of X2",ylab="no. of buildings.")
barplot(count3,main="Bar plot for X3(Wall area)",xlab="possible values of X3",ylab="no. of buildings.")
barplot(count4,main="Bar plot for X4(Roof area)",xlab="possible values of X4",ylab="no. of buildings.")
barplot(count5,main="Bar plot for X5(Height)",xlab="possible values of X5",ylab="no. of buildings.")
barplot(count6,main="Bar plot for X6(Orientation)",xlab="possible values of X6",ylab="no. of buildings.")
barplot(count7,main="Bar plot for X7(Glazing area)",xlab="possible values of X7",ylab="no. of buildings.")
barplot(count8,main="Bar plot for X8(Glazing variation)",xlab="possible values of X8",ylab="no. of buildings.")

ds=read_excel("C:/Users/ashi/Downloads/ENB2012_data.xlsx")
layout(matrix(c(1,2),ncol=2))             
plot(X5,ds$Y1,col="red",xlab="height",ylab="heating load")
plot(X5,ds$Y2,col="BLUE",xlab="height",ylab="COOLING load")
