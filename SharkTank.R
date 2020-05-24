setwd("E:/r direct/Web & Social Analytics/WSM Assignment")
SharkTank = read.csv("Shark+Tank+Companies.csv", stringsAsFactors=FALSE)
####Identifying the count of True and Flase Values
table(SharkTank$deal)

##  FALSE  TRUE 
##  244     251 

library(tm)
library(SnowballC)
library(wordcloud)

corpusSharkTank = Corpus(VectorSource(SharkTank$description))
wordcloud(corpusSharkTank,colors=rainbow(7),max.words=50)

## Converting into lower-case

corpusSharkTank = tm_map(corpusSharkTank, tolower)

##Remiving Punctuations
corpusSharkTank = tm_map(corpusSharkTank, removePunctuation)

## Removing stopwords

corpusSharkTank = tm_map(corpusSharkTank, removeWords, c("the","and",stopwords("english")))

## Removing Extra Spaces

corpusSharkTank = tm_map(corpusSharkTank,stripWhitespace)


# Stem document 

corpusSharkTank = tm_map(corpusSharkTank, stemDocument)
## Worl cloud after data cleaning process



wordcloud(corpusSharkTank,colors=rainbow(7),max.words=50)

frequenciesSharkTank = DocumentTermMatrix(corpusSharkTank)
### The DTM contains 495 rows and 3501 columns

###From 100 to 105 ideas picking the 505 to 515 words for visulization

inspect(frequenciesSharkTank[100:105,505:515])
## Checking the Sparsity
findFreqTerms(frequenciesSharkTank,highfreq =  20)

###Removing the words which are not present in atleast 5 descriptions/statements
sparseSharkTank = removeSparseTerms(frequenciesSharkTank, 0.995)

SharkTankSparse = as.data.frame(as.matrix(sparseSharkTank))


colnames(SharkTankSparse) = make.names(colnames(SharkTankSparse))

# Adding dependent variable

SharkTankSparse$DV = SharkTank$deal
SharkTankSparse$DV=as.factor(SharkTankSparse$DV)
# Building a CART model

library(rpart)
library(rpart.plot)

SharkTankCART = rpart(DV ~ ., data=SharkTankSparse, method="class")

prp(SharkTankCART,extra=2,box.palette = "auto")
library(rattle)
library(RColorBrewer)
fancyRpartPlot(SharkTankCART)
printcp(SharkTankCART)
ptree<- prune(SharkTankCART,cp=  SharkTankCART$cptable[which.min
                                                       (SharkTankCART$cptable[,"xerror"]),"CP"])
fancyRpartPlot(ptree)


prp(SharkTankCART,extra=2,box.palette = "auto")

printcp(SharkTankCART)
plotcp(ptree)

### Accuracy
predictCARTRatio = predict(SharkTankCART, data=SharkTankSparse, type="class")
CART_ratio <- table(SharkTank$deal, predictCARTRatio)
BaseAccuracyRatio = sum(diag(CART_ratio))/sum(CART_ratio)

##Building Random Forest Model
library(randomForest)

SharkTankRF=randomForest(DV~.,data=SharkTankSparse)
varImpPlot(SharkTankRF)
predictCARTRatio1 = predict(SharkTankRF, data=SharkTankSparse, type="class")
CART_ratio1 <- table(SharkTank$deal, predictCARTRatio1)
BaseAccuracyRatio1 = sum(diag(CART_ratio1))/sum(CART_ratio1)


##BUilding Logistic Regression Model 

SharkTankLogit=glm(DV~.,data=SharkTankSparse,family="binomial")
SharkTankPred=predict(SharkTankLogit,data=SharkTankSparse,type="response")
table(SharkTankSparse$DV,SharkTankPred>0.5)
(135+115)/495

predictCARTRatio2 = predict(SharkTankLogit, data=SharkTankSparse, type="response")
CART_ratio2 <- table(SharkTankSparse$DV, predictCARTRatio2)
BaseAccuracyRatio2 = sum(diag(CART_ratio2))/sum(CART_ratio2)

SharkTankSparse$ratio=(SharkTank$askedFor/SharkTank$valuation)


##CART Model After adding ratio Variable

SharkTankCART2 = rpart(DV ~ ., data=SharkTankSparse, method="class")

prp(SharkTankCART2,extra=2,box.palette = "auto")

library(rattle)
library(RColorBrewer)
fancyRpartPlot(SharkTankCART2)
printcp(SharkTankCART2)
ptree<- prune(SharkTankCART2,cp=  SharkTankCART2$cptable[which.min
                                                       (SharkTankCART2$cptable[,"xerror"]),"CP"])
fancyRpartPlot(ptree)
plotcp(ptree)

##Randomforest Model After adding ratio Variable
SharkTankRF2=randomForest(DV~.,data=SharkTankSparse)
varImpPlot(SharkTankRF2)

##BUilding Logistic Regression Model after adding ratio variable

SharkTankLogit2=glm(DV~ratio,data=SharkTankSparse,family="binomial")
SharkTankLogit2Pred=predict(SharkTankLogit2,data=SharkTankSparse,type="response")
table(SharkTankSparse$DV,SharkTankLogit2Pred>0.5)
(125+146)/495
