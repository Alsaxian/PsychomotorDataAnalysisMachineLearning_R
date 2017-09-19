#### randomForest ####
library(randomForest)
fit.random <- randomForest(patient ~ ., data = jeuEntraine[[1]])
fit.random
# summary(fit.random)
pred.randomTrain <- predict(fit.random, newdata = jeuEntraine[[1]][-1])
tb.randomTrain <- table(Prédiction = pred.random, Vérité = jeuEntraine[[1]][[1]]) ## training error rate
pred.random <- predict(fit.random, newdata = jeuTest[[1]][-1])
tb.random <-  table(Prédiction = pred.random, Vérité = jeuTest[[1]][[1]]) 

#### Fast adaboost ####
library(fastAdaboost)
fit.ada <- adaboost(patient ~ ., data = jeuEntraine[[4]], nIter = 2000)
print(fit.ada)
pred.adaTrain <- predict(fit.ada, newdata = jeuEntraine[[1]][-1])
pred.adaTrain$class
tb.adaTrain <- table(Prédiction = pred.adaTrain$class, Vérité = jeuEntraine[[1]][[1]]) ## training error rate
pred.ada <- predict(fit.ada, newdata = jeuTest[[4]][-1])
tb.ada <-  table(Prédiction = pred.ada$class, Vérité = jeuTest[[4]][[1]]) 
#### Naïf Bayes ####
library(e1071)

nB.fit <- naiveBayes(patient ~ ., jeuEntraine[[1]], laplace = 0)
nB.fit
nB.pred <- predict(nB.fit, jeuTest[[1]][-1])
table(Vérité = jeuTest[[1]][[1]], Prédiction = nB.pred)

nB.fit2 <- naiveBayes(patient ~ ., as.data.frame(lapply(jeuEntraine[[1]], factor, ordered = TRUE)), laplace = 0)
nB.fit2
nB.pred2 <- predict(nB.fit2, jeuTest[[1]][-1])
tbnB01 <- table(Vérité = jeuTest[[1]][[1]], Prédiction = nB.pred2)

predict(nB.fit, jeuTest[[1]][-1], type = "raw")

nB.fitSD <- naiveBayes(depression ~ ., jeuEntraineSDnettoyEtoutFactorisE, laplace = 0)
nB.fitSD
nB.predSD <- predict(nB.fitSD, jeuTestSD[-1])

table(Vérité = jeuTestSD[[1]], Prédiction = nB.predSD)


nB.fitSD2 <- naiveBayes(depression ~ ., jeuEntraineSDnettoyEtoutFactorisE3, laplace = 0)
nB.fitSD2
nB.predSD2 <- predict(nB.fitSD2, jeuTestSD[-1])
table(Vérité = jeuTestSD[[1]], Prédiction = nB.predSD2)
#### Clustering EM ####
# install.packages("mclust")
library(mclust)
?mclust.options
?mclust

#### Trace ROC ####
# install.packages('mlbench')
library(mlbench)
# ?PimaIndiansDiabetes2

data("PimaIndiansDiabetes2", package = "mlbench")
# str(PimaIndiansDiabetes2)
# is.na(PimaIndiansDiabetes2)
# summary(PimaIndiansDiabetes2)

# install.packages("VIM")
library(VIM)
# install.packages("mice")
library(mice)
# md.pattern(PimaIndiansDiabetes2)
# ?md.pattern
mdp <- md.pattern(PimaIndiansDiabetes2)
# str(mdp)
aggr(PimaIndiansDiabetes2, prop = FALSE, numbers = TRUE)
?aggr

library(caret)
# install.packages("ipred")
library(ipred)
preproc <- preProcess(PimaIndiansDiabetes2[-9], method = "bagImpute")
# str(preproc)
# summary(preproc)
# dim(preproc)
preproc$dim
df <- predict(preproc, PimaIndiansDiabetes2[-9])
nrow(PimaIndiansDiabetes2)
df$diabetes <- PimaIndiansDiabetes2$diabetes
indexPima <- createDataPartition(df$diabetes, times = 1, p = 0.75, list = FALSE)
# ?createDataPartition
trainPima <- df[indexPima, ]
testPima <- df[-indexPima, ]
dim(trainPima)
dim(testPima)
??'DMwR'

modelPima <- naiveBayes(diabetes ~ ., data = trainPima)
pred1Pima <- predict(modelPima, testPima)
table(testPima$diabetes, pred1Pima)

# install.packages("gmodels")
library(gmodels)
?CrossTable
CrossTable(testPima$diabetes, pred1Pima, prop.c = FALSE, prop.r = FALSE,
           prop.t = FALSE, chisq = FALSE)

library(ROCR)
?prediction
pred2Pima <- predict(modelPima, testPima, type = "raw")
head(pred2Pima)
predFUNPima <- prediction(pred2Pima[, "pos"], testPima$diabetes)
perfPima <- performance(predFUNPima, measure = "tpr", x.measure = "fpr")
plot(perfPima, main = "ROC Curve par 'performance'", col = "blue", lwd = 3)

# install.packages("pROC")
library(pROC)
modelROCPima <- roc(testPima$diabetes, pred2Pima[, "pos"])
plot(smooth(modelROCPima), print.auc = TRUE, auc.polygon = TRUE, grid = c(0.1, 0.2), grid.col = c("green", "red"),
     max.auc.polygon = TRUE, auc.polygon.col = "skyblue", print.thres = TRUE, title = "try it!")



data(aSAH)
auc(aSAH$outcome, aSAH$s100b)
summary(aSAH)
auc(sujets$patient, sujets$imageCorps) #0.8696
auc(sujets$patient, sujets$facialeSpontaneeEmo)


#### clustering EM bernoulli ####
CluEM <- function (data, centresDonnes = TRUE, centre1 = rep(0.9, times = ncol(data)), 
                   centre2 = rep(0.1, times = ncol(data)), Iter = 100) {
  if (!centresDonnes) {
    centre1 <- runif(ncol(data)) ## proba de réussite d'un test
    centre2 <- runif(ncol(data)) ## proba de réussite d'un test
  }
    
  
  regrouper <- function () {
      apply(data, 1, function(obs) {
        dis1 <- sum(obs * log(centre1) + (1 - obs) * log(1 - centre1))
        dis2 <- sum(obs * log(centre2) + (1 - obs) * log(1 - centre2))
        return (ifelse(dis1 < dis2, 1, 2))
      })
  }
  
  calculeCentres <- function() {
    centre1 <- apply(data[data$classe1 == 1, ], 2, mean) 
    centre2 <- apply(data[data$classe1 == 2, ], 2, mean) 
    return(cbind(centre1, centre2))
  }
  
  data$classe1 <- regrouper()
  data$classe2 <- 2
  
  for(i in 1:Iter) {
    if (all((data$classe1 == data$classe2)) || sum(data$classe1 == 1) == 0 || sum(data$classe1 == 2) == 0) break else {

      print(paste("i = ", i))
      data$classe2 <- data$classe1
      centres <- calculeCentres()
      centre1 <- centres[, 1]
      centre2 <- centres[, 2]
      print(paste("centre1 =", centre1))
      print(paste("centre2 =", centre2))
      centre1[centre1 == 1] <- 0.9
      centre1[centre1 == 0] <- 0.1
      centre2[centre2 == 1] <- 0.9
      centre2[centre2 == 0] <- 0.1
      data$classe1 <- regrouper()
      print(data$classe1)
      if (sum(data$classe1 == 1) == 1) data$classe1 <- 2 else if (sum(data$classe1 == 2) == 1) data$classe1 <- 1
    }
  }
  print(i)
  return(list(classes = data$classe1, n.Iter = i))
}

resulCl <- CluEM(data = sujets[, 7:32])
resulCl <- CluEM(data = sujets[, 7:32], centresDonnes = FALSE)
resulCl2 <- CluEM(data = sujets[, 7:32], centresDonnes = FALSE)
