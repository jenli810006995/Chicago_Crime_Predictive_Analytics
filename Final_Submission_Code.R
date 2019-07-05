## read in the data ##

setwd("D:/Quantiply_Challenge/Data")

Data <- read.csv(file = "Crimes_-_2001_to_present.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, as.is = TRUE)

memory.limit(size=16000)  ## increase R memory limit

str(Data)


## install and load packages

install.packages("aod")
install.packages("ggplot2")

require(aod)
require(ggplot2)


## I am interested in the Date happening,the location(domestic or not), arrest or not, domestic or not, description and location discription

### Deal with Date ###


Date <- Data$Date

Date <- as.Date(Date,format='%m/%d/%Y')

Data$Date.1 <- Date

str(Data)

## subset the data I want ##

full <- cbind(Data$Date.1, Data$Zip.Codes, Data$Primary.Type, Data$Block, Data$Arrest, Data$Domestic, Data$Description, Data$Location.Description, Data$Police.Districts, Data$Police.Beats, Data$Community.Areas)

colnames(full) <- c("Date.1", "Zip.Codes", "Primary.Type", "Block", "Arrest", "Domestic", "Description", "Location.Description", "Police.Districts", "Police.Beats", "Community.Areas")

full <- as.data.frame(full)

View(head(full))



## I decide to subset the data having Zip.code is not NA ##


full.nNA <- full[is.na(full$Zip.Codes)==FALSE,]

summary(full.nNA$Zip.Codes) # no NA


summary(full.nNA) ## still NAs for Police.Beats, Police.Districts, and Community.area 


full.nNA <- full.nNA[is.na(full.nNA$Police.Districts)==FALSE,]

full.nNA <- full.nNA[is.na(full.nNA$Police.Beats)==FALSE,]

full.nNA <- full.nNA[is.na(full.nNA$Community.Areas)==FALSE,]

summary(full.nNA)  ## No NA for all the variables

#### make a copy ###

full.nNA.3 <- full.nNA


full.nNA.3$Primary.Type <- ifelse(full.nNA.3$Primary.Type %in% "THEFT", "THEFT",
                                  ifelse(full.nNA.3$Primary.Type %in% "BATTERY","BATTERY",
                                         ifelse(full.nNA.3$Primary.Type %in% "CRIMINAL DAMAGE","CRIMINAL DAMAGE",
                                                ifelse(full.nNA.3$Primary.Type %in% "NARCOTICS","NARCOTICS",
                                                       ifelse(full.nNA.3$Primary.Type %in% "ASSAULT","ASSAULT","OTHERS")))))
View(head(full.nNA.3))  ## finished categorize primary.type

str(full.nNA.3)

full.nNA.3 <- full.nNA.3[,-2] ## I exclude Zip code 

full.nNA.3 <- full.nNA.3[,-3] ## exclude Block

full.nNA.3 <- full.nNA.3[,-(7:9)]  ## get the variable I want 

full.nNA.3$Date.1 <- as.numeric(full.nNA.3$Date.1)


full.nNA.3$Description <- ifelse(full.nNA.3$Description%in%"SIMPLE", "SIMPLE",
                                 ifelse(full.nNA.3$Description%in%"$500 AND UNDER", "$500 AND UNDER",
                                        ifelse(full.nNA.3$Description%in%"DOMESTIC BATTERY SIMPLE", "DOMESTIC BATTERY SIMPLE",
                                               ifelse(full.nNA.3$Description%in%"TO VEHICLE", "TO VEHICLE",
                                                      ifelse(full.nNA.3$Description%in%"TO PROPERTY", "TO PROPERTY",
                                                             ifelse(full.nNA.3$Description%in%"OVER $500","OVER $500","Others"))))))

full.nNA.3$Location.Description <- ifelse(full.nNA.3$Location.Description%in%"STREET", "STREET",
                                          ifelse(full.nNA.3$Location.Description%in%"RESIDENCE","RESIDENCE",
                                                 ifelse(full.nNA.3$Location.Description%in%"APARTMENT","APARTMENT",
                                                        ifelse(full.nNA.3$Location.Description%in%"SIDEWALK", "SIDEWALK",
                                                               ifelse(full.nNA.3$Location.Description%in%" PARKING LOT/GARAGE(NON.RESID.)", "PARKING LOT","Others")))))


str(full.nNA.3) ##GOOD

full.nNA.3$Primary.Type <- as.factor(full.nNA.3$Primary.Type)
full.nNA.3$Description <- as.factor(full.nNA.3$Description)
full.nNA.3$Location.Description <- as.factor(full.nNA.3$Location.Description)
full.nNA.3$Date.1 <- as.numeric(full.nNA.3$Date.1)


## 1% of the sample size
smp_size <- floor(0.01 * nrow(full.nNA.3))

train_ind <- sample(seq_len(nrow(full.nNA.3)), size = smp_size)

train.3 <- full.nNA.3[train_ind, ]
test.3 <- full.nNA.3[-train_ind, ]  ## OK

### Random forest with CV

install.packages("randomForest")
require(randomForest)

str(train.3)
str(test.3)

View(head(train.3)) # column 2 is the target

set.seed(100)
require(randomForest)

group <- train.3[,2]
x <- train.3[,-2]
View(head(x))
Y <- factor(group) # need a factor
setwd("D:/Quantiply_Challenge") 
FitObj <- randomForest(x ,Y)
FitObj$confusion

Error.f <- function(mat) 1 - sum(diag(mat))/sum(mat)
Classif.Table <- FitObj$confusion
Error.f(Classif.Table)  ## The error rate is 0.2429517

plot(FitObj, lwd=2); abline(h=0.26,lty=3, lwd=2) ## FIGURE 1


### Random Forest CV ###

x <- train.3[,-2]
Y <- train.3[,2]
View(head(x))
View(head(Y))

CV.RandomForest2.f <- function(NoOfSeeds=10, mtry.v=c(2:5),nodesize.v=c(1:20), ntree=500, f=3) 
{p <- ncol(train.3)-1
x <- train.3[,-2] ## no primary.type
Y <-factor(train.3[,2])
k <-length(mtry.v)
m <-length(nodesize.v)
Table.arr <-array(0,c(2,2,NoOfSeeds,k,m))
for(dlof in 1:f)
{#10-folds
  #setting a seed to get similar results
  fold=sample(rep(1:f,length=nrow(x)))
  #choosing random sample for cross validation
  cond <- (fold==dlof)
  x.train<-x[!cond,]
  x.test<-x[cond,]
  y.train<-Y[!cond]
  y.test<-Y[cond]
  for (j in 1:NoOfSeeds){#10 rounds
    set.seed(99+j)
    for (i in 1:k){#different mtry
      for (ind in 1:m){#CV#training trees and using it on test sample
        FitObj <- randomForest(x.train,y.train,mtry=mtry.v[i],nodesize=nodesize.v[ind],ntree=ntree,xtest=x.test,ytest=y.test)#saving the classification 
      }
    }
  }
}
FitObj}
# need to save these objects so when it is time to knit, all I need to do is load them
CV.rf.1<-CV.RandomForest2.f(ntree=500)

save(CV.rf.1,file="CV.rf.1")

# need to save these objects 
CV.rf.1<-CV.RandomForest2.f(ntree=500)
save(CV.rf.1,file="CV.rf.1")

Table.arr <- CV.rf.1$confusion

Table <- CV.rf.1$confusion[,1:6]

Error.f <- function(mat) 1 - sum(diag(mat))/sum(mat)
Error.f(Table) ## [1] 0.2622585, a little higher than random forest without CV

plot( CV.rf.1,lwd=2); abline(h=0.26,lty=3, lwd=2)  ## Error rate versus number of trees with CV

save(Table.arr, file="Table.arr")


sd.mat <- apply(Table.arr,2,sd)
Mean.mat <- apply(Table.arr,2,mean)


sd.mat.2 <- sd.mat
sd.mat.2 <- as.matrix(sd.mat.2)

Mean.mat.2 <- as.matrix(Mean.mat)
Mean.mat.2 <- as.matrix(Mean.mat.2)



## Conditional inference trees ##
## this was not finished by the due time ##

install.packages("party")
require(party)

set.seed(415)
fit <- cforest(as.factor(Primary.Type)~.,data = train.3,
               controls = cforest_unbiased(ntree=2000, mtry=3))

save(fit, file="fit.Rdata")

## KNN CV ##
## getting too many ties error ##

### numerize ###

full.nNA.2$Primary.Type.2 <- ifelse(full.nNA.2$Primary.Type %in% "THEFT", 1,
                                    ifelse(full.nNA.2$Primary.Type %in% "BATTERY",2,
                                           ifelse(full.nNA.2$Primary.Type %in% "CRIMINAL DAMAGE",3,
                                                  ifelse(full.nNA.2$Primary.Type %in% "NARCOTICS",4,
                                                         ifelse(full.nNA.2$Primary.Type %in% "ASSAULT",5,6)))))

full.nNA.2$Block.2 <- ifelse(full.nNA.2$Block %in% "100XX W OHARE ST", 1,
                             ifelse(full.nNA.2$Block %in% " 001XX N STATE ST", 2,
                                    ifelse(full.nNA.2$Block %in% " 076XX S CICERO AVE", 3,
                                           ifelse(full.nNA.2$Block %in% "008XX N MICHIGAN AVE", 4,
                                                  ifelse(full.nNA.2$Block %in% "0000X N STATE ST", 5,
                                                         ifelse(full.nNA.2$Block %in% "023XX S STATE ST", 6,7))))))

full.nNA.2$Description.2 <- ifelse(full.nNA.2$Description %in% "SIMPLE", 1,
                                   ifelse(full.nNA.2$Description %in% "$500 AND UNDER", 2,
                                          ifelse(full.nNA.2$Description %in% "TO VEHICLE", 3,
                                                 ifelse(full.nNA.2$Description %in% "TO PROPERTY", 4,
                                                        ifelse(full.nNA.2$Description %in% "OVER $500", 5,6)))))

full.nNA.2$Location.Description.2 <- ifelse(full.nNA.2$Location.Description %in% "STREET", 1,
                                            ifelse(full.nNA.2$Location.Description %in% "RESIDENCE", 2,
                                                   ifelse(full.nNA.2$Location.Description %in% "APARTMENT", 3,
                                                          ifelse(full.nNA.2$Location.Description %in% "SIDEWALK", 4,
                                                                 ifelse(full.nNA.2$Location.Description %in% "PARKING LOT/GARAGE(NON.RESID.)", 5,6)))))


View(head(full.nNA.2))

full.want <- full.nNA.2[,-(3:8)]
summary(full.want) ## all numeric
View(head(full.want)) ## the data I want 

class(full.want)

### Data Modeling ###

## 1% of the sample size
smp_size <- floor(0.01 * nrow(full.want))

set.seed(123)
train_ind <- sample(seq_len(nrow(full.want)), size = smp_size)

train <- full.want[train_ind, ]
test <- full.want[-train_ind, ] 
View(head(train))

summary(train)
summary(test)

####

train$Date.1 <- as.numeric(train$Date.1)
train$Zip.Codes <- as.numeric(train$Zip.Codes)
train$Police.Districts <- as.numeric(train$Police.Districts)
train$Police.Beats <- as.numeric(train$Police.Beats)
train$Community.Areas <- as.numeric(train$Community.Areas)
train$Primary.Type.2 <- as.numeric(train$Primary.Type.2)

str(train)
summary(train) ## no missing data

str(train) ## test data type, only primary type is factor
str(test)

test$Date.1 <- as.numeric(test$Date.1)
test$Zip.Codes <- as.numeric(test$Zip.Codes)
test$Police.Districts <- as.numeric(test$Police.Districts)
test$Police.Beats <- as.numeric(test$Police.Beats)
test$Community.Areas <- as.numeric(test$Community.Areas)
test$Primary.Type.2 <- as.numeric(test$Primary.Type.2)

str(test)

## delete Description ##
## because here I only want to include variables make sense when they are converted to numeric 

train <- train[,-8]
test <- test[,-8]

View(head(train)) ## column 6 is Primay.Type

# k-fold CV of knn
source("CV_Functions_for_knn.R")

Data <- rbind(train,test)
View(head(Data))
data.X <- scale(Data[,-6])
Comp.Clas.f(data.X=data.X, Y=Data[,6], FUN=knn.clas.f, m=300, k=10, Rounds=5, ver=3)
Plot.Comp.f(k=10,Rounds=5, ver=3)