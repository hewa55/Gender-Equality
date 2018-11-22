library(MASS)
library(car)
# https://data.europa.eu/euodp/data/dataset/gender-equality-index
data <- read.csv("data/data.csv", header=TRUE)
data <- data[1:29,]
# these are the high level columns without going into crazy details
cols <- c("Country","Gender.Equality.Index","WORK","MONEY","KNOWLEDGE","TIME","POWER","HEALTH")
data <- data[,cols]
head(data)
plot(data$WORK,data$Gender.Equality.Index)
# a bit too well correlated?
## colinerarity and categories
## include year as extra factor
pairs(data[2:8])
data <- data[order(data$Gender.Equality.Index),]
fit <- glm(Gender.Equality.Index~MONEY+WORK+KNOWLEDGE+POWER+HEALTH,family=gaussian,data=data)
step.model <- stepAIC(fit, direction = "both", 
                      trace = TRUE)
summary(step.model)
summary(fit)
plot(data[,2])
points(predict(fit),col="green")
data[15,]

# https://www.kaggle.com/unsdsn/world-happiness
happy <- read.csv("data/2017.csv", header=TRUE)
cols <- c(1,3,6:12)
happy <- happy[,cols]
colnames(happy)[c(2,3,5,8)] <- c("Score","Economy","Health","Trust")
colnames(happy)
pairs(happy[,2:9])
ncol(happy)
plot(happy[,2])
tail(happy[,1:2])
fit <- glm(Score~Economy+Family+Freedom+Generosity+Health+Trust,family=gaussian,data=happy)
fit2 <- glm(Score~Economy+Family+Freedom+Generosity+Health+Trust+Dystopia.Residual,family=gaussian,data=happy)
step.model <- stepAIC(fit, direction = "both", 
                      trace = TRUE)
step.model <- stepAIC(fit2, direction = "both", 
                      trace = TRUE)
summary(step.model)
summary(fit)
summary(happy)
plot(fit)

# NBA
nba <- read.csv("data/NBA.csv", header=TRUE)

# consumption 
# https://www.kaggle.com/anderas/car-consume#measurements.csv
nba <- read.csv("data/consumption.csv", header=TRUE)

# cacao
# https://www.kaggle.com/rtatman/chocolate-bar-ratings
cacao <- read.csv("data/cacao2.csv", header=TRUE)
colnames(cacao)
colnames(cacao) <- c("Company","Specific.Origin","REF","Date","Percent","Company.Location","Rating","Type","Broad.Origin","Percent2")
pairs(cacao[,c(7,8,9,6,1,10)])
fit <- glm(Rating~factor(Type)+Percent2,data=cacao, family=gaussian)
summary(fit)


# wine quality
# https://archive.ics.uci.edu/ml/datasets/Wine+Quality
wine <- read.csv("data/winequality.csv", header=TRUE, sep=";")
colnames(wine)
pairs(wine[,c("quality","pH","sulphates","chlorides")])
colnames(cacao) <- c("Company","Specific.Origin","REF","Date","Percent","Company.Location","Rating","Type","Broad.Origin","Percent2")
pairs(cacao[,c(7,8,9,6,1,10)])
fit <- glm(Rating~factor(Type)+Percent2,data=cacao, family=gaussian)
summary(fit)
