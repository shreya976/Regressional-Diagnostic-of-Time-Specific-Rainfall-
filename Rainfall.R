library(readr) 
raindata <- read_csv("C:/Users/admin/Documents/raindata.csv")

View(raindata) 
s = sum(is.na(raindata)) 
raindata <- na.omit(raindata) -
x<-raindata[order(raindata$humidity),c(7,9)] 
x
y<-raindata[order(raindata$temparature),c(4,9)] 
y 
z<-raindata[order(raindata$pressure),c(2,9)] 
z 
p<-raindata[order(raindata$windspeed),c(12,9)] 
p 
q<-raindata[order(raindata$dewpoint),c(6,9)] 
q 
r<-raindata[order(raindata$rainfall),c(9)]
r 
hist(raindata$humidity,main="histogram of humidity", 
xlab = "Humidity", 
border="blue",col = "green", 
xlim = c(36,98),las=0, 
breaks = 10) 
hist(raindata$temparature,main="histogram of temparature", 
xlab = "temperature", 
border="blue",col = "green", 
freq = FALSE, 
xlim = c(4,33), 
las=1, 
breaks = 10)
hist(raindata$pressure,main="histogram of pressure", 
xlab = "pressure", 
border="blue",col = "green", 
xlim = c(998,1035), 
las=1, 
breaks = 10) 
hist(raindata$windspeed,main="histogram of windspeed", 
xlab = "windspeed", 
border="blue",col = "green", 
xlim = c(5,60), 
las=1, 
breaks = 10) 
hist(raindata$winddirection,main="histogram of winddirection", 
xlab = "winddirection", 
border="blue",col = "green", 
xlim = c(10,350), 
las=1, 
breaks = 10) 
library(ineq) 
g<-ineq(raindata$humidity,type="Gini") 
g
# The Gini coefficient can then be thought of as the ratio of the area that lies between the line of equality and the Lorenz curve (marked A in the diagram) over the total area under the line of equality (marked A and B in the diagram); 
plot(Lc(raindata$humidity),col="purple",lwd=2)#gini index 
h<-ineq(raindata$temparature,type="Gini") 
h 
plot(Lc(raindata$temparature),col="green",lwd=2)#gini index 
i<-ineq(raindata$dewpoint,type="Gini") 
i 
plot(Lc(raindata$dewpoint),col="blue",lwd=2)#gini index 
j<-ineq(raindata$windspeed,type="Gini") 
j 
plot(Lc(raindata$windspeed),col="pink",lwd=2)#gini index 
k<-ineq(raindata$pressure,type="Gini") 
k 
plot(Lc(raindata$pressure),col="orange",lwd=2)#gini index 
a<-ineq(raindata$humidity,type="entropy") 
a 
plot(Lc(raindata$humidity),col="purple",lwd=2) 
b<-ineq(raindata$temparature,type="entropy") 
b 
plot(Lc(raindata$temparature),col="green",lwd=2)
c<-ineq(raindata$dewpoint,type="entropy") 
c<-0.06 
plot(Lc(raindata$dewpoint),col="blue",lwd=2) 
d<-ineq(raindata$windspeed,type="entropy") 
d 
plot(Lc(raindata$windspeed),col="pink",lwd=2) 
e<-ineq(raindata$pressure,type="entropy") 
e 
plot(Lc(raindata$pressure),col="orange",lwd=2) 
x1<-c(g,h,i,j,k) 
x2<-c(a,b,c,d,e) 
plot(x1,type="o",col="blue",ylim=c(0.0,0.4)) 
par(new=TRUE) 
lines(x2,type="o",col="red") 
data<-raindata 
str(data)
data$rainfallf<-factor(data$rainfall) 
#To ensure all results are reproducable 
set.seed(12)
pd<-sample(2,nrow(data),replace=TRUE,prob=c(0.8,0.2)) 
train<-data[pd==1,] 
validate<-data[pd==2,] 
library(partykit) 
library(tree) 
library(party) 
tree1<-ctree(rainfallf~humidity+dewpoint+windspeed+sunshine,data=train) 
tree1 
#Conditional inference tree 
plot(tree1,main="Conditional Inference Tree") 
#table of prediction errors 
tabl<-table(predict(tree1), train$rainfallf) 
ctr<-sum(diag(tabl))/sum(tabl) 
ctr 
# Estimated class probabilities 
predict(tree1,validate,type="prob") 
predict(tree1,validate) 
raindata$rainfall <- ifelse(raindata$cloud < 88, 'no', 'yes') 
raindata$rainfall <- as.factor(raindata$rainfall) 
table(raindata$rainfall)
set.seed(123) 
samp <- sample(nrow(raindata), 0.6 * nrow(raindata)) 
train <- raindata[samp, ] 
test <- raindata[-samp, ] 
library(randomForest) 
model <- randomForest(rainfall ~ sunshine+temparature+windspeed+pressure+dewpoint+humidity - cloud, data = train) 
fit.rf <- randomForest(rainfall ~ sunshine+temparature+windspeed+pressure+dewpoint+humidity - cloud, data = train) 
print(fit.rf) 
importance(fit.rf) 
#plot(fit.rf) 
#plot( importance(fit.rf), lty=2, pch=16) 
#lines(importance(fit.rf)) 
varImpPlot(fit.rf,type=2) 
imp = importance(fit.rf) 
#take cloud as ref 
pred <- predict(model, newdata = test) 
pred 
tab=table(pred, test$rainfall) 
tab 
rnd<-sum(diag(tab))/sum(tab) 
rnd 
ctr 
rnd
