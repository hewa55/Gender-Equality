# https://data.europa.eu/euodp/data/dataset/gender-equality-index
library(betareg)
library(car)
library(effects)
library(ggplot2)
library(lawstat)
library(gridExtra)

## Read data
data <- read.csv("data/data.csv", header=TRUE)
data.2005 <- read.csv("data/gender-equality-index-2005.csv", header=TRUE)[2:29,]
data.2005$YEAR <- rep(2005,nrow(data.2005)) 
data.2010 <- read.csv("data/gender-equality-index-2010.csv", header=TRUE)[2:29,]
data.2010$YEAR <- rep(2010,nrow(data.2010)) 
data.2012 <- read.csv("data/gender-equality-index-2012.csv", header=TRUE)[2:29,]
data.2012$YEAR <- rep(2012,nrow(data.2012)) 
data.2015 <- read.csv("data/gender-equality-index-2015.csv", header=TRUE)[2:29,]
data.2015$YEAR <- rep(2015,nrow(data.2015)) 
# combine dataframes
data <- rbind(data.2005,data.2010)
data <- rbind(data,data.2012)
data <- rbind(data,data.2015)

# these are the columns to investigate
cols <- c("Country","Gender.Equality.Index","WORK","MONEY","KNOWLEDGE","TIME","POWER","HEALTH","YEAR")
data <- data[,cols]
names(data) <- c("Country","Index","WORK","MONEY","KNOWLEDGE","TIME","POWER","HEALTH","YEAR")
head(data)

## plots for introduction
## used
ggplot(data, aes(x=as.factor(YEAR), y=Index)) + geom_boxplot()+geom_jitter(position=position_jitter(0.2))+
  stat_summary(fun.y=mean, geom="point", shape=18,size=3, color="red")+
  labs(title="Gender Equality",x ="Index", y = "Year")
## USED
pairs(data[,2:9])

##### Beta distribution approach ###
# set values min =0 and max =1
# convert scale from 0-100 to 0-1
data[,2] <- data[2]/100

# Model 1
# all covariates and year as factor variable
Model1 <- betareg(Index~MONEY+WORK+KNOWLEDGE+TIME+POWER+HEALTH+as.factor(YEAR),data=data)

# Model2
# year as continuous variable
Model2 <- update(Model1,.~.-as.factor(YEAR)+YEAR)

## Model 3
# health as quadratic term
Model3 <- update(Model2,.~.+I(MONEY^2))

##### Model Selection ####
ll.model1 <- Model1$loglik
ll.model2 <- Model2$loglik
ll.model3 <- Model3$loglik

# Model 3 and Model 2 are nested
D2.3 <- 2*(ll.model3-ll.model2)
# difference in parameter is one
# model 2 is better
1-pchisq(D2.3,1)

# Take model 2 as BIC is lower
BIC(Model1)
-2*ll.model1+log(nrow(data))*11
BIC(Model2)
-2*ll.model2+log(nrow(data))*9

## Select Variables
# get vif scores
vif1 <- vif(Model2)
# none are above 5, but close to it
print(vif)

# see wald test
summary(Model2)

# get rid of year - not significant
Model2 <- update(Model2,.~.-YEAR)

# all covariates now significant
summary(Model2)

# get second vif scores, see if something changed
vif2 <- vif(Model2)
# are lower now
print(vif2)
# difference in vif scores
vif2-vif1[1:6]

## Residuals
# standard residuals
res <- residuals(Model2)
var(res)

# pearson residuals
res.pearson <- residuals(Model2, type="pearson")
# around 1 - but there is also a dispersion parameter
var(res.pearson)

# deviance residuals
res.dev <- residuals(Model2, type="deviance")
var(res.dev)

# plot the residuals
plot.res.pearson <- ggplot(data,aes(x=Index,y=res.pearson))+ geom_point()+ylab("Pearson Residuals")+xlab("Gender Equality Index")
plot.res <- ggplot(data,aes(x=Index,y=res))+ geom_point()+ylab("Residuals")+xlab("Gender Equality Index")
plot.res.dev <- ggplot(data,aes(x=Index,y=res.dev))+ geom_point() +ylab("Deviance Residuals")+xlab("Gender Equality Index")
grid.arrange(plot.res,plot.res.pearson,plot.res.dev,ncol=3)

## Effects Plot
# Plot the effects on link scale, should be linear
e_work <- plot(effect("WORK",Model2,residuals=TRUE),partial.residuals=TRUE, type="link")
e_health <- plot(effect("HEALTH",Model2,residuals=TRUE),partial.residuals=TRUE, type="link")
e_knowledge <- plot(effect("KNOWLEDGE",Model2,residuals=TRUE),partial.residuals=TRUE, type="link")
e_power <- plot(effect("POWER",Model2,residuals=TRUE),partial.residuals=TRUE, type="link")
e_money <- plot(effect("MONEY",Model2,residuals=TRUE),partial.residuals=TRUE, type="link")
# Arrange the plots - they seem to be linear, all good
grid.arrange(e_work,e_health,e_knowledge,e_power,e_money,nrow=2)

# runs test - not significant
runs.test(residuals(Model2,type="pearson"))

# obtain parameters for presentation
summary(Model2)

