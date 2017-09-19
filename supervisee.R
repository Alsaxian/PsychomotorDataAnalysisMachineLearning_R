############### Régression Linéaire ################
# install.packages("glmnet")
library(glmnet)
# install.packages("e1071")
library(e1071) # Pour svm
# install.packages("caret")
library(caret) # pour tuner
# install.packages("ROCR")
library(ROCR)

library(MASS)
# install.packages("klaR")
library(klaR)

library(class)
# install.packages("tree")
library(tree)


### Schizo. vs. Depressifs 
regOrdinaire <- glmnet(x = as.matrix(sujets[c(fract$schizoEntraine, fract$depEntraine), 7:32]), 
       y = factor(sujets[c(fract$schizoEntraine, fract$depEntraine), "depression"]), lambda = 0,
       family = "binomial")
str(regOrdinaire)
coef(regOrdinaire, s = 0)
pred <- predict(regOrdinaire, s = 0, as.matrix(sujets[c(fract$schizoTest, fract$depTest), 7:32]), type = "response")
sujets[c(fract$schizoTest, fract$depTest), "depression"]
round(pred, digits = 1)


regOrdi2 <- glm(formula = depression ~ ., data = sujets[c(fract$schizoEntraine, fract$depEntraine), c(3, 7:32)], 
    family = binomial)
pred2 <- predict(regOrdi2, sujets[c(fract$schizoTest, fract$depTest), 7:32], type = "response")
pred2
round(pred2, digits = 1)
round(predict(regOrdi2, type = "response"), digits = 1)
str(regOrdi2)
regOrdi2$R
regOrdi2$residuals
which(regOrdi2$R > 0.01)
coef(regOrdi2)
regOrdi2$deviance

### Patients vs. Temoins
regPT <- glmnet(x = as.matrix(jeuEntraine[-1]), 
                y = jeuEntraine[[1]], lambda = 0,
                family = "binomial")
predPTUnrounded <- predict(regPT,s = 0, as.matrix(jeuTest[-1]), type = "response")
predPTrounded <- round(predPTUnrounded)
predPT <- factor(predict(regPT,s = 0, as.matrix(jeuTest[-1]), type = "class"), 
                 levels = c("temoin", "patient"), ordered = TRUE)
predPT
table(Vérité = jeuTest[[1]], Prédiction = predPT)
# prediction(predPTUnrounded, labels = c("patient", "temoin"))

fitPT <- factor(predict(regPT,s = 0, as.matrix(jeuEntraine[-1]), type = "class"), 
                levels = c("temoin", "patient"), ordered = TRUE)
table(Vérité = jeuEntraine[[1]], Prédiction = fitPT)
# length(fitPT)
tauxErreursEntraine <- table(true = sujets[c(fract2$patEntraine, fract2$temEntraine), "patient"], pred = fitPT)
tauxErreursEntraine

tauxErreursETest <- table(true = sujets[c(fract2$patTest, fract2$temTest), "patient"], pred = predPT)
tauxErreursETest
coef(regPT)


# Rigde
grid <- 10^seq(1, -10, length = 100)
ridge.mod <-  glmnet(x = as.matrix(jeuEntraine[-1]), 
                     y = jeuEntraine[[1]], lambda = grid, alpha = 0,
                     family = "binomial")
# dim(coef(ridge.mod))
coef(ridge.mod)[, 100]
ridge.mod$lambda[100]
ridge.pred100 <- predict(ridge.mod, s = ridge.mod$lambda[10], newx = as.matrix(jeuTest[-1]), type = "response")
ridge.predmot100 <- predict(ridge.mod, s = ridge.mod$lambda[100], newx = as.matrix(jeuTest[-1]), type = "class")
# fitRidge1 <- 
ridge.pred
fitRidge1
fitRidge1 <- ifelse(fitRidge1 == 1, "patient", "temoin")
table(true = sujets[c(fract2$patTest, fract2$temTest), "patient"], pred = fitRidge1)

ridge.predmot90 <- predict(ridge.mod, s = 90, newx = as.matrix(sujets[c(fract2$patTest,
                          fract2$temTest), 7:32]), type = "class")
table(Vérité = jeuTest[[1]], Prédiction = ridge.predmot100)
table(Vérité = jeuTest[[1]], Prédiction = ridge.predmot90)
# length(ridge.pred0)

fitRidge0 <- round(ridge.pred0)
fitRidge0 <- ifelse(fitRidge0 == 1, "patient", "temoin")
table(true = sujets[c(fract2$patTest, fract2$temTest), "patient"], pred = fitRidge0)

# Automatiser par cv, ridge
cv.out <- cv.glmnet(as.matrix(sujets[c(fract2$patEntraine, fract2$temEntraine), 7:32]), lambda = grid,
                    sujets[c(fract2$patEntraine, fract2$temEntraine), "patient"], alpha = 0, family = "binomial")
plo <- plot(cv.out, plot = FALSE)
bestlam <- cv.out$lambda.min
bestlam
pred.cvRidge <- predict(cv.out, s=bestlam, newx=as.matrix(jeuTest[-1]), type = "class")

table(Vérité = jeuTest[[1]], Prédiction = pred.cvRidge)

# Lasso
cv.out2 <- cv.glmnet(as.matrix(sujets[c(fract2$patEntraine, fract2$temEntraine), 7:32]), #lambda = grid,
                    sujets[c(fract2$patEntraine, fract2$temEntraine), "patient"], alpha = 1, family = "binomial")
bestlam <- cv.out2$lambda.min
bestlam
pred.cvLasso <- predict(cv.out2, newx=as.matrix(jeuTest[-1]), type = "class")
table(Vérité = jeuTest[[1]], Prédiction = pred.cvLasso)

plot(cv.out2)
lasso.coeff <- coef(cv.out2, s=bestlam)
lasso.coeff

cv.outENET <- cv.glmnet(as.matrix(sujets[c(fract2$patEntraine, fract2$temEntraine), 7:32]), lambda = grid,
                    sujets[c(fract2$patEntraine, fract2$temEntraine), "patient"], 
                    alpha = 0.5, family = "binomial")
bestlamENET <- cv.outENET$lambda.min
bestlamENET
predENET <- predict(cv.out, s=bestlamENET, newx=as.matrix(jeuTest[-1]), type = "class")
table(Vérité = jeuTest[[1]], Prédiction = predENET)
plot(cv.outENET)
lasso.coeff <- coef(cv.out, s=bestlam)
lasso.coeff


# Don't know why this doesn't work
# tune(glmnet, train.x = patient~ ., data = jeuEntraine, family = "binomial",
#      ranges = list(alpha = c(0.9^(0:10))))



?trainControl

expandedGrid <- expand.grid(.alpha = seq(.05, 1, length = 15), .lambda = 10^c((-10:2)))
cctrl1 <- trainControl(method = "cv", number = 10, returnResamp = "all",
                       classProbs = TRUE, summaryFunction = twoClassSummary)





test_class_cv_model <- train(as.matrix(sujets[c(fract2$patEntraine, fract2$temEntraine), 7:32]), 
                            sujets[c(fract2$patEntraine, fract2$temEntraine), "patient"], 
                             method = "glmnet", 
                             trControl = cctrl1,
                             metric = "ROC",
                             preProc = c("center", "scale"),
                             tuneGrid = expandedGrid)
test_class_pred <- predict(test_class_cv_model, as.matrix(sujets[c(fract2$patTest, fract2$temTest), 7:32]))
test_class_pred
table(Vérité = jeuTest[[1]], Prédiction = test_class_pred)



######################### SVM ######################################
### On sait qu'un hyperplan n'est pas le meilleur. On essaie donc un SVM avec la fonction de noyau radiale. ###
# svmfit <- svm(formula = patient ~ ., data = sujets[c(fract2$patEntraine, fract2$temEntraine), c(2, 7:32)],
#               kernel = "radial", gamma = 1, cost = 1, type = "C-classification")
# plot(svmfit, sujets[c(fract2$patEntraine, fract2$temEntraine), c(2, 7:32)], formula = imageCorps~absenceImpulsivite)
# summary(svmfit)
# 
# svmfit <- svm(formula = patient ~ ., data = sujets[c(fract2$patEntraine, fract2$temEntraine), c(2, 7:32)],
#               kernel = "radial", gamma = 1, cost = 1e5, type = "C-classification")
# 
# # plot(svmfit, sujets[c(fract2$patEntraine, fract2$temEntraine), c(2, 7:32)], formula = imageCorps~absenceImpulsivite)
# ypred <- predict(svmfit, sujets[c(fract2$patTest, fract2$temTest), 7:32])
# ypred
# fitted(svmfit)


tune.out <- tune(svm, train.x = patient ~ ., data = sujets[c(fract2$patEntraine, fract2$temEntraine), c(2, 7:32)],
                 kernel = "radial", ranges = list(cost = 10^(-2:6), gamma = c(0.2, 0.5, 1:4)), 
                 type = "C-classification") 
summary(tune.out)
# - best parameters:
# cost gamma
# 10   0.2
# - best performance: 0.1622222 
pred4 <- predict(tune.out$best.model, newdata =sujets[c(fract2$patTest, fract2$temTest), 7:32])
table(true = sujets[c(fract2$patTest, fract2$temTest), "patient"], pred = pred4)

rocplot <- function(pred, truth, ...) {
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  plot(perf, ...)
}

Fitted <- attributes(predict(svmfit, sujets[c(fract2$patEntraine, fract2$temEntraine), c(2, 7:32)], 
                             decision.values = TRUE))$decision.values
rocplot(Fitted, sujets[c(fract2$patEntraine, fract2$temEntraine), 2], main="Training")

Fitted2 <- attributes(predict(svmfit, sujets[c(fract2$patTest, fract2$temTest), c(2, 7:32)], 
                             decision.values = TRUE))$decision.values
rocplot(Fitted2, sujets[c(fract2$patTest, fract2$temTest), 2], main="Test")


tune.out2 <- tune(svm, train.x = patient ~ ., data = sujets[c(fract2$patEntraine, fract2$temEntraine), c(2, 7:32)],
                 kernel = "linear", ranges = list(cost = 10^(-2:6), gamma = c(0.2, 0.5, 1:4)), 
                 type = "C-classification")
#tune.out2
#summary(tune.out2)
# - best parameters:
#   cost gamma
# 0.1   0.2
# 
# - best performance: 0 
pred5 <- predict(tune.out2$best.model, newdata =sujets[c(fract2$patTest, fract2$temTest), 7:32])
# pred5
table(true = sujets[c(fract2$patTest, fract2$temTest), "patient"], pred = pred5)

tune.out3 <- tune(svm, train.x = patient ~ ., data = sujets[c(fract2$patEntraine, fract2$temEntraine), c(2, 7:32)],
                  kernel = "polynomial", ranges = list(cost = 10^(-2:6), gamma = c(0.2, 0.5, 1:5)), 
                  type = "C-classification")
summary(tune.out3)
pred6 <- predict(tune.out3$best.model, newdata = sujets[c(fract2$patTest, fract2$temTest), 7:32])
pred6
table(true = sujets[c(fract2$patTest, fract2$temTest), "patient"], pred = pred6)

tune.out4 <- tune(svm, train.x = patient ~ ., data = sujets[c(fract2$patEntraine, fract2$temEntraine), c(2, 7:32)],
                  kernel = "sigmoid", ranges = list(cost = 10^(-2:6), gamma = c(0.2, 0.5, 1:5)), 
                  type = "C-classification")
summary(tune.out4)
pred7 <- predict(tune.out4$best.model, newdata =sujets[c(fract2$patTest, fract2$temTest), 7:32])
pred7
tb7 <- table(true = sujets[c(fract2$patTest, fract2$temTest), "patient"], pred = pred7)
tb7


#### LDA ####
lda.fit <- lda(patient ~ ., jeuEntraine)
lda.pred <- predict(lda.fit, sujets[c(fract2$patTest, fract2$temTest), 7:32])
lda.class <- lda.pred$class
table(true = sujets[c(fract2$patTest, fract2$temTest), "patient"], pred = lda.class)
#### QDA ####
qda.fit <- qda(patient ~ ., jeuEntraine)
# qda.pred <- predict(qda.fit, sujets[c(fract2$patTest, fract2$temTest), 7:32])
# qda.class <- qda.pred$class
# table(true = sujets[c(fract2$patTest, fract2$temTest), "patient"], pred = qda.class)

#### DA régularisée ####
rda.fit <- rda(patient ~ ., jeuEntraine[[1]], gamma = 0, lambda = 1)
rda.pred <- predict(rda.fit, jeuTest[[1]][, -1])
rda.class <- rda.pred$class
table(true = sujets[c(fract2$patTest, fract2$temTest), "patient"], pred = rda.class)

rda.fit2 <- rda(patient ~ ., jeuEntraine, gamma = 0.1, lambda = 0.5)
rda.pred2 <- predict(rda.fit2, sujets[c(fract2$patTest, fract2$temTest), 7:32])
rda.class2 <- rda.pred2$class
table(true = sujets[c(fract2$patTest, fract2$temTest), "patient"], pred = rda.class2)


#### KNN ####
cctrl2 <- trainControl(method="repeatedcv", repeats = 3) 
knnFit <- train(patient ~ ., data = jeuEntraine, method = "knn", 
                trControl = cctrl2, preProcess = c("center","scale"), tuneLength = 12)
summary(knnFit)
knnFit
plot(knnFit)
pred8 <- predict(knnFit, newdata = jeuTest[, -1])
table(true = sujets[c(fract2$patTest, fract2$temTest), "patient"], pred = pred8)
summary(pred8)


knn.pred = knn(jeuEntraine[, -1], jeuTest[, -1], jeuEntraine[, 1], k = 1)
table(true = sujets[c(fract2$patTest, fract2$temTest), "patient"], pred = knn.pred)

knn.pred2 = knn(jeuEntraine[, -1], jeuTest[, -1], jeuEntraine[, 1], k = 3)
table(true = sujets[c(fract2$patTest, fract2$temTest), "patient"], pred = knn.pred2)
## This returns only the estimate value of the training set. No more meaning.
# cvKnn <- knn.cv(train = jeuEntraine[-1], cl = jeuEntraine[,1], k=5)

#### Tree ####
tree.mod <- tree(patient ~ ., jeuEntraine)
summary(tree.mod)
plot(tree.mod)
text(tree.mod, pretty=0)
tree.mod
tree.pred <- predict(tree.mod, jeuTest[,-1],type = "class")
table(true = sujets[c(fract2$patTest, fract2$temTest), "patient"], pred = tree.pred)



#### Emballer
EntrainePsychomoSimple <- function(methode = c("RDA", "SVM", "régression", "tree", "KNN", "régression simple",
                                               "naïf Bayes"),
                            fract2 = NA,
                            sousFamilleSVM = c("sigmoid", "linear", "polynomial", "radial"), 
                            sousFamilleRegression = c("ridge", "lasso", "melange", "elastic net"),
                            retour = "qualité", ...) {
  if (retour == "qualité") 1 else 0
  
  tableErreurs <- list()
  
  if(is.na(fract2)) fract2 <- fractionnePatientsVsTemoins()
  jeuEntraine <- sujets[c(fract2$patEntraine, fract2$temEntraine), c(2, 7:32)]
  jeuTest <- sujets[c(fract2$patTest, fract2$temTest), c(2, 7:32)]
  
  if ("RDA" %in% methode) {
    rda.fit <- rda(patient ~ ., jeuEntraine, gamma = 0.1, lambda = 0.5)
    rda.pred <- predict(rda.fit, jeuTest[-1])$class
    rda.tb <- table(Vérité = jeuTest[[1]], Prédiction = rda.pred)
    rda.fauxPositif <- rda.tb[1, 2]
    rda.fauxNegatif <- rda.tb[2, 1]
    rda.nombreErreurs <- rda.fauxNegatif + rda.fauxPositif
    
    tableErreurs <- c(tableErreurs, list(RDA = rda.tb))
  }
  
  if ("SVM" %in% methode) {
    svm.fit <- lapply(sousFamilleSVM, function (sF) {
            tune(svm, train.x = patient ~ ., data = jeuEntraine,
                kernel = sF, ranges = list(cost = 10^(-2:6), gamma = c(0.2, 0.5, 1:5)), 
                type = "C-classification")
    })
    svm.pred <- lapply(svm.fit, function(svmFit) {
      predict(svmFit$best.model, newdata = jeuTest[-1])
    })
    
    svm.tb <- lapply(svm.pred, function(svmPred) {
      table(Vérité = jeuTest[[1]], Prédiction = svmPred)
    })
    names(svm.tb) <- paste("SVM", sousFamilleSVM, sep = ".")
    
    svm.fauxPositif <- lapply(svm.tb, function(svmTb) svmTb[1, 2])
    names(svm.fauxPositif) <- paste("SVM", sousFamilleSVM, sep = ".")
    svm.fauxNegatif <- lapply(svm.tb, function(svmTb) svmTb[2, 1])
    names(svm.fauxNegatif) <- paste("SVM", sousFamilleSVM, sep = ".")
    svm.nombreErreurs <- lapply(svm.tb, function(svmTb) svmTb[1, 2] + svmTb[2, 1])
    names(svm.nombreErreurs) <- paste("SVM", sousFamilleSVM, sep = ".")
    
    tableErreurs <- c(tableErreurs, svm.tb)
  }
  
  if ("régression" %in% methode) {
    saufElasticNet <- setdiff(sousFamilleRegression, "elastic net")
    
    if (length(saufElasticNet != 0)) {
      valeursAlpha <- lapply(saufElasticNet, function(sousMethode) 
                switch(sousMethode, ridge = 0, lasso = 1, melange = 0.5))
      reg.fit <- lapply(valeursAlpha, function(Alpha) {
        cv.glmnet(x = as.matrix(jeuEntraine[-1]), y = jeuEntraine[[1]], alpha = Alpha, family = "binomial")
      })
      reg.pred <- lapply(reg.fit, function(regFit) {
        factor(predict(regFit, s = regFit$lambda.min, newx = as.matrix(jeuTest[-1]), type = "class"), 
               levels = c("temoin", "patient"), ordered = TRUE)
      })
      
      reg.tb <- lapply(reg.pred, function(regPred) {
        table(Vérité = jeuTest[[1]], Prédiction = regPred)
      })
      names(reg.tb) <- paste("Regression", saufElasticNet, sep = ".")
      
      reg.fauxPositif <- lapply(reg.tb, function(regTb) regTb[1, 2])
      names(reg.fauxPositif) <- paste("Regression", saufElasticNet, sep = ".")
      reg.fauxNegatif <- lapply(reg.tb, function(regTb) regTb[2, 1])
      names(reg.fauxNegatif) <- paste("Regression", saufElasticNet, sep = ".")
      reg.nombreErreurs <- lapply(reg.tb, function(regTb) regTb[1, 2] + regTb[2, 1])
      names(reg.nombreErreurs) <- paste("Regression", saufElasticNet, sep = ".")
      
      tableErreurs <- c(tableErreurs, reg.tb)
    }
    
    if ("elastic net" %in% sousFamilleRegression) {
      expandedGrid <- expand.grid(.alpha = seq(.05, 1, length = 15), .lambda = 10^c((-10:2)))
      cctrl1 <- trainControl(method = "cv", number = 10, returnResamp = "all",
                             classProbs = TRUE, summaryFunction = twoClassSummary)
      
      elasticNet.fit <- train(as.matrix(jeuEntraine[-1]), 
                                   jeuEntraine[[1]], 
                                   method = "glmnet", 
                                   trControl = cctrl1,
                                   metric = "ROC",
                                   preProc = c("center", "scale"),
                                   tuneGrid = expandedGrid)
      elasticNet.pred <- predict(elasticNet.fit, jeuTest[-1])
    
      elasticNet.tb <- table(Vérité = jeuTest[[1]], Prédiction = elasticNet.pred)
      elasticNet.fauxPositif <- elasticNet.tb[1, 2]
      elasticNet.fauxNegatif <- elasticNet.tb[2, 1]
      elasticNet.nombreErreurs <- elasticNet.fauxNegatif + elasticNet.fauxPositif
      
      tableErreurs <- c(tableErreurs, list(Regression.elasticNet = elasticNet.tb))
    }
  }
  
  if ("tree" %in% methode) {
    tree.fit <- tree(patient ~ ., jeuEntraine)
    tree.pred <- predict(tree.fit, jeuTest[-1], type = "class")
    
    tree.tb <- table(Vérité = jeuTest[[1]], Prédiction = tree.pred)
    tree.fauxPositif <- tree.tb[1, 2]
    tree.fauxNegatif <- tree.tb[2, 1]
    tree.nombreErreurs <- tree.fauxNegatif + tree.fauxPositif
    
    tableErreurs <- c(tableErreurs, list(tree = tree.tb))
  }
  
  if ("KNN" %in% methode) {
    cctrl2 <- trainControl(method="repeatedcv",repeats = 5) 
    knn.fit <- train(patient ~ ., data = jeuEntraine, method = "knn", 
                    trControl = cctrl2, preProcess = c("center","scale"), tuneLength = 12)
    knn.pred <- predict(knn.fit, newdata = jeuTest[, -1])
    
    knn.tb <- table(Vérité = jeuTest[[1]], Prédiction = knn.pred)
    knn.fauxPositif <- knn.tb[1, 2]
    knn.fauxNegatif <- knn.tb[2, 1]
    knn.nombreErreurs <- knn.fauxNegatif + knn.fauxPositif
    
    tableErreurs <- c(tableErreurs, list(KNN = knn.tb))
  }
  
  if ("naïf Bayes" %in% methode) {
    nB.fit <- naiveBayes(patient ~ ., jeuEntraine, laplace = 0)
    nB.pred <- predict(nB.fit, jeuTest[-1])
    
    nB.tb <- table(Vérité = jeuTest[[1]], Prédiction = nB.pred)
    nB.fauxPositif <- nB.tb[1, 2]
    nB.fauxNegatif <- nB.tb[2, 1]
    nB.nombreErreurs <- nB.fauxNegatif + nB.fauxPositif
    
    tableErreurs <- c(tableErreurs, list(NaifBayes = nB.tb))
  }
  
  return (tableErreurs)
}

tbERR <- EntrainePsychomoSimple("SVM")
tbERR <- EntrainePsychomoSimple("régression")
tbERR <- EntrainePsychomoSimple("RDA")
tbERR <- EntrainePsychomoSimple("tree")
tbERR <- EntrainePsychomoSimple("KNN")
tbERR <- EntrainePsychomoSimple("régression")

EntrainePsychomoMultiple <- function(fois = 5, affichage = "par méthode",
                                     methode = c("RDA", "SVM", "régression", 
                                                           "tree", "KNN", "régression simple", "naïf Bayes"),
                                     fract2liste = NA,
                                     sousFamilleSVM = c("sigmoid", "linear", "polynomial", "radial"), 
                                     sousFamilleRegression = c("ridge", "lasso", "melange", "elastic net"),
                                     retour = "qualité", ...) {
  if (is.na(fract2liste)) fract2liste <- rep(list(NA), fois) else
      fois = length(fract2liste)
  
  resultats <- lapply(1:fois, function(i) EntrainePsychomoSimple(methode = methode, sousFamilleSVM = sousFamilleSVM,
                                                                 sousFamilleRegression = sousFamilleRegression, 
                                                                 retour = retour, ...))
  names(resultats) <- paste("Essai", 1:fois, sep=".")
  
  if (affichage == "par méthode") {
    liste1 <- lapply(1:length(resultats[[1]]), function(Methode) {
      liste2 <- lapply(1:length(resultats), function(essai) {
        return(resultats[[essai]][[Methode]])
      })
      names(liste2) <- names(resultats)
      return(liste2)
    })

    names(liste1) <- names(resultats[[1]])

    return (liste1)
  } else return (resultats)
}


tbERRm <- EntrainePsychomoMultiple(fois = 3, methode = c("tree", "SVM"))
tbERRm2 <- EntrainePsychomoMultiple(fois = 3, methode = c("tree", "KNN"))
tbERRm
tbERRm2

tbERRm3 <- EntrainePsychomoMultiple(fois = 3, fract2liste = fractpt, methode = "tree")
tbERRm3 # 5 fois. OK.

# fract2 <- lapply(1:fois, function(i) fractionnePatientsVsTemoins(...))