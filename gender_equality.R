# https://data.europa.eu/euodp/data/dataset/gender-equality-index
library(betareg)
library(car)
library(effects)
library(ggplot2)
library(lawstat)
library(gridExtra)

data <- read.csv("data/data.csv", header=TRUE)
data.2005 <- read.csv("data/gender-equality-index-2005.csv", header=TRUE)[2:29,]
data.2005$YEAR <- rep(2005,nrow(data.2005)) 
data.2010 <- read.csv("data/gender-equality-index-2010.csv", header=TRUE)[2:29,]
data.2010$YEAR <- rep(2010,nrow(data.2010)) 
data.2012 <- read.csv("data/gender-equality-index-2012.csv", header=TRUE)[2:29,]
data.2012$YEAR <- rep(2012,nrow(data.2012)) 
data.2015 <- read.csv("data/gender-equality-index-2015.csv", header=TRUE)[2:29,]
data.2015$YEAR <- rep(2015,nrow(data.2015)) 
plot(data.2015$Gender.Equality.Index)
text(data.2015$Gender.Equality.Index,labels = data.2015$Country,cex=0.7)
stripchart(data.2015$Gender.Equality.Index)
data <- rbind(data.2005,data.2010)
data <- rbind(data,data.2012)
data <- rbind(data,data.2015)

# these are the high level columns without going into crazy details
cols <- c("Country","Gender.Equality.Index","WORK","MONEY","KNOWLEDGE","TIME","POWER","HEALTH","YEAR")
data <- data[,cols]
names(data) <- c("Country","Index","WORK","MONEY","KNOWLEDGE","TIME","POWER","HEALTH","YEAR")
head(data)
plot(data$WORK,data$Index)


## plots for introduction

# not used
ggplot(data, aes(x=as.factor(YEAR), y=Index)) + geom_violin(trim = FALSE)+geom_jitter(position=position_jitter(0.2)) + 
  stat_summary(fun.y=mean, geom="point", shape=18,size=3, color="red")

## used
ggplot(data, aes(x=as.factor(YEAR), y=Index)) + geom_boxplot()+geom_jitter(position=position_jitter(0.2))+
  stat_summary(fun.y=mean, geom="point", shape=18,size=3, color="red")+
  labs(title="Gender Equality",x ="Index", y = "Year")

## NOT USED
ggplot(data, aes(x=as.factor(YEAR), y=Index)) + geom_boxplot()+geom_point()+
  stat_summary(fun.y=mean, geom="point", shape=18,size=3, color="red")+
  labs(title="Gender Equality",x ="Index", y = "Year")

## USED
pairs(data[which(data$YEAR==2015),2:8])

pairs(data[,2:9])
nrow(data$Index)
## colinerarity and categories
## include year as extra factor


##### Beta distribution approach ###
# set values min =0 and max =100
# two more values to estimate
# https://beckmw.wordpress.com/2013/02/05/collinearity-and-stepwise-vif-selection/
#countries <- data$Country
#data$Country <- countries
#data[,2:8] <- data[2:8]/100
data[,2] <- data[2]/100
Model1 <- betareg(Index~MONEY+WORK+KNOWLEDGE+TIME+POWER+HEALTH+as.factor(YEAR),data=data)
## Question: as.factor reduces collinearity massively? Why?
vif(Model1)
summary(Model1)
AIC(Model1)
BIC(Model1)
Anova(Model1)

beta.fit2 <- update(Model1, .~.-MONEY)
vif(beta.fit2)
summary(beta.fit2)
AIC(beta.fit2)
BIC(beta.fit2)
Anova(beta.fit2)

beta.fit2a <- update(Model1, .~.-HEALTH-as.factor(YEAR))
vif(beta.fit2a)
AIC(beta.fit2a)
BIC(beta.fit2a)
summary(beta.fit2a)
Anova(beta.fit2a)



## best model - year as factor, no time, no money
beta.fit3 <- update(Model1, .~.-TIME-MONEY)
AIC(beta.fit3)
BIC(beta.fit3)
summary(beta.fit3)
Anova(beta.fit3)
 ## get rid of as.factor(YEAR)
beta.fit4 <- update(Model1, .~.-TIME-MONEY-as.factor(YEAR))
vif(beta.fit4)
AIC(beta.fit4)
BIC(beta.fit4)
summary(beta.fit4)
Anova(beta.fit4)

beta.fit5 <- update(Model1, .~.-TIME-HEALTH-as.factor(YEAR))
vif(beta.fit5)
AIC(beta.fit5)
BIC(beta.fit5)
summary(beta.fit5)
Anova(beta.fit5)



# year doesnt help if not a factor see here
Model2 <- update(Model1,.~.-as.factor(YEAR)+YEAR)
vif(Model2)
summary(Model2)
AIC(Model2)
beta.fit.wo.year2 <- update(Model2, .~.-MONEY)
summary(beta.fit.wo.year2)
AIC(beta.fit.wo.year2)

###
## Model 3
Model3 <- update(Model1,.~.+I(HEALTH^2))
Anova(Model3)
vif(Model3)
## removing health instead of money
beta.fit3 <- update(Model1, .~.-HEALTH)
vif(beta.fit3)
summary(beta.fit3)
AIC(beta.fit3)



##### Model Selection ####

## 
d.model1 <- summary(Model1)

ll.model1 <- Model1$loglik
ll.model2 <- Model2$loglik
ll.model3 <- Model3$loglik

# no reason to take the more complicated model
D1.3 <- 2*(ll.model3-ll.model1)
1-pchisq(D1.3,1)

# Take model 2
BIC(Model1)
-2*ll.model1+log(nrow(data))*11
BIC(Model2)
-2*ll.model2+log(nrow(data))*9

## Select Variables
Model2 <- update(Model1,.~.-as.factor(YEAR)+YEAR)
vif(Model2)
Anova(Model2)
summary(Model2)
# get rid of year
Model2 <- update(Model2,.~.-YEAR)
summary(Model2)
Anova(Model2)
vif(Model2)
BIC(Model2)
# no intercept - get warning from VIF and weird output
Model2 <- update(Model2,.~.-HEALTH)
vif(Model2)
Anova(Model2)
Model2 <- update(Model2,.~.-MONEY)
vif(Model2)
Anova(Model2)

# standard residuals
res <- residuals(Model2)
var(res)

# pearson residuals
res.pearson <- residuals(Model2, type="pearson")
var(res.pearson)

# deviance residuals
res.dev <- residuals(Model2, type="deviance")
var(res.dev)
# plot the residuals
plot.res.pearson <- ggplot(data,aes(x=Index*100,y=res.pearson))+ geom_point()+ylab("Pearson Residuals")+xlab("Gender Equality Index")
plot.res <- ggplot(data,aes(x=Index*100,y=res))+ geom_point()+ylab("Residuals")+xlab("Gender Equality Index")
plot.res.dev <- ggplot(data,aes(x=Index*100,y=res.dev))+ geom_point() +ylab("Deviance Residuals")+xlab("Gender Equality Index")
grid.arrange(plot.res,plot.res.pearson,plot.res.dev,ncol=3)

plot(data$Index,res.dev,pch=18)
## high data density between 0.5 and 0.75, but variance consistens --> good
# residual plots look fine - nothing wrong with it
# BG is a bit off
which.min(res.pearson)
res.pearson[3]
data[3,]
plot(effect("WORK",beta.fit4,residuals=TRUE),partial.residuals=TRUE, type="link")
plot(effect("HEALTH",beta.fit4,residuals=TRUE),partial.residuals=TRUE, type="link")
plot(effect("KNOWLEDGE",beta.fit4,residuals=TRUE),partial.residuals=TRUE, type="link")
plot(effect("POWER",beta.fit4,residuals=TRUE),partial.residuals=TRUE, type="link")
plot(effect("MONEY",beta.fit2a,residuals=TRUE),partial.residuals=TRUE, type="link")
acf(residuals(beta.fit4, type='pearson'))
runs.test(residuals(beta.fit4,type="pearson"))
