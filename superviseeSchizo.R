jeuEntraineSDnettoyE <- jeuEntraineSD
jeuEntraineSDnettoyE$depression <- factor(jeuEntraineSDnettoyE$depression, levels = c("depressif", 
                                                                  "schizophrene"), ordered = TRUE)
jeuTestSDnettoyE <- jeuTestSD
jeuTestSDnettoyE$depression <- factor(jeuTestSDnettoyE$depression, levels = c("depressif", 
                                                                                      "schizophrene"), ordered = TRUE)
jeuEntraineSDnettoyEtoutFactorisE <- jeuEntraineSDnettoyE
# for(i in 2:27) jeuEntraineSDnettoyEtoutFactorisE[[i]] <- as.factor(jeuEntraineSDnettoyEtoutFactorisE[[i]])
jeuEntraineSDnettoyEtoutFactorisE %>% mutate_if(is.numeric, factor, ordered = TRUE)


jeuEntraineSDnettoyEtoutFactorisE2 <- as.data.frame(jeuEntraineSDnettoyE) %>% mutate_if(is.numeric, as.factor)
class(jeuEntraineSDnettoyEtoutFactorisE2[[2]])

jeuEntraineSDnettoyEtoutFactorisE3 <- as.data.frame(lapply(jeuEntraineSDnettoyE, factor, ordered = TRUE))
class(jeuEntraineSDnettoyEtoutFactorisE3[[2]])
#### LDA ####
lda.fitSD <- lda(depression ~ ., jeuEntraineSD)
lda.predSD <- predict(lda.fitSD, jeuTestSD[-1])
lda.classSD <- lda.predSD$class
table(Vérité = jeuTestSD[[1]], Prédiction = lda.classSD)
#### QDA ####
# qda.fit <- qda(patient ~ ., jeuEntraine)
# qda.pred <- predict(qda.fit, sujets[c(fract2$patTest, fract2$temTest), 7:32])
# qda.class <- qda.pred$class
# table(true = sujets[c(fract2$patTest, fract2$temTest), "patient"], pred = qda.class)

#### DA régularisée ####
rda.fitSD <- rda(depression ~ ., jeuEntraineSD, gamma = 0, lambda = 1)
rda.predSD <- predict(rda.fitSD, jeuTestSD[-1])
rda.classSD <- rda.predSD$class
table(Vérité = jeuTestSD[[1]], Prédiction = rda.classSD)

rda.fitSD2 <- rda(depression ~ ., jeuEntraineSD, gamma = 0.1, lambda = 0.5)
rda.predSD2 <- predict(rda.fitSD2, jeuTestSD[-1])
rda.classSD2 <- rda.predSD2$class
table(Vérité = jeuTestSD[[1]], Prédiction = rda.classSD2)

rda.fitSD3 <- rda(depression ~ ., jeuEntraineSD, gamma = 0.5, lambda = 0.8)
rda.predSD3 <- predict(rda.fitSD3, jeuTestSD[-1])
rda.classSD3 <- rda.predSD3$class
table(Vérité = jeuTestSD[[1]], Prédiction = rda.classSD3, exclude = "autre")


#### KNN ####
cctrl2 <- trainControl(method="repeatedcv",repeats = 3) 

knnFitSD <- train(depression ~ ., data = jeuEntraineSDnettoyE, method = "knn", 
                trControl = cctrl2, preProcess = c("center","scale"), tuneLength = 22)

plot(knnFitSD)
predSD8 <- predict(knnFitSD, newdata = jeuTestSD[-1])
table(true = jeuTestSD[[1]], pred = predSD8)
summary(predSD8)


knn.pred = knn(jeuEntraineSD[-1], jeuTestSD[-1], jeuEntraineSD[[1]], k = 1)
table(true = jeuTestSD[[1]], pred = knn.pred)

knn.pred3 = knn(jeuEntraineSD[-1], jeuTestSD[-1], jeuEntraineSD[[1]], k = 3)
table(true = jeuTestSD[[1]], pred = knn.pred3)

knn.pred5 = knn(jeuEntraineSD[-1], jeuTestSD[-1], jeuEntraineSD[[1]], k = 5)
table(true = jeuTestSD[[1]], pred = knn.pred5)

knn.pred7 = knn(jeuEntraineSD[-1], jeuTestSD[-1], jeuEntraineSD[[1]], k = 7)
table(true = jeuTestSD[[1]], pred = knn.pred7)

#### Tree ####
tree.modSD <- tree(depression ~ ., jeuEntraineSD)
summary(tree.modSD)
plot(tree.modSD)
text(tree.modSD, pretty=0)
tree.modSD
tree.predSD <- predict(tree.modSD, jeuTestSD[-1],type = "class")
table(true = jeuTestSD[[1]], pred = tree.predSD)

#### SVM ####
### radial ###
tune.outSD1 <- tune(svm, train.x = depression ~ ., data = jeuEntraineSD,
                 kernel = "radial", ranges = list(cost = 10^(-2:6), gamma = c(0.2, 0.5, 1:4)), 
                 type = "C-classification") 
summary(tune.outSD1)
pred.SVMSD1 <- predict(tune.outSD1$best.model, newdata = jeuTestSD[-1])
table(true = jeuTestSD[[1]], pred = pred.SVMSD1)

### polynomial ###
tune.outSD2 <- tune(svm, train.x = depression ~ ., data = jeuEntraineSD,
                    kernel = "polynomial", ranges = list(cost = 10^(-2:6), gamma = c(0.2, 0.5, 1:4)), 
                    type = "C-classification") 
summary(tune.outSD2)
pred.SVMSD2 <- predict(tune.outSD2$best.model, newdata = jeuTestSD[-1])
table(true = jeuTestSD[[1]], pred = pred.SVMSD2)

### linéaire ###
tune.outSD3 <- tune(svm, train.x = depression ~ ., data = jeuEntraineSD,
                    kernel = "linear", ranges = list(cost = 10^(-2:6), gamma = c(0.2, 0.5, 1:4)), 
                    type = "C-classification") 
summary(tune.outSD3)
pred.SVMSD3 <- predict(tune.outSD3$best.model, newdata = jeuTestSD[-1])
table(true = jeuTestSD[[1]], pred = pred.SVMSD3)

### sigmoïde ###
tune.outSD4 <- tune(svm, train.x = depression ~ ., data = jeuEntraineSD,
                    kernel = "sigmoid", ranges = list(cost = 10^(-2:6), gamma = c(0.2, 0.5, 1:4)), 
                    type = "C-classification") 
summary(tune.outSD4)
pred.SVMSD4 <- predict(tune.outSD4$best.model, newdata = jeuTestSD[-1])
table(true = jeuTestSD[[1]], pred = pred.SVMSD4)


#### Régression logistique ####
### Ridge ###
cv.outRidgeSD <- cv.glmnet(as.matrix(jeuEntraineSDnettoyE[-1]), lambda = grid,
                    jeuEntraineSDnettoyE[[1]], alpha = 0, family = "binomial")
plo <- plot(cv.outRidgeSD)
bestlamRidgeSD <- cv.outRidgeSD$lambda.min

bestlamRidgeSD
pred.cvRidgeSD <- predict(cv.outRidgeSD, s=bestlamRidgeSD, newx=as.matrix(jeuTestSD[-1]), type = "class")

table(Vérité = jeuTestSD[[1]], Prédiction = pred.cvRidgeSD)

### Lasso ###
cv.outLassoSD <- cv.glmnet(as.matrix(jeuEntraineSDnettoyE[-1]), lambda = grid,
                           jeuEntraineSDnettoyE[[1]], alpha = 1, family = "binomial")
plo <- plot(cv.outLassoSD)
bestlamLassoSD <- cv.outLassoSD$lambda.min
bestlamLassoSD
pred.cvLassoSD <- predict(cv.outLassoSD, s=bestlamLassoSD, newx=as.matrix(jeuTestSD[-1]), type = "class")

table(Vérité = jeuTestSD[[1]], Prédiction = pred.cvLassoSD)

### mélange ###
cv.outMelangeSD <- cv.glmnet(as.matrix(jeuEntraineSDnettoyE[-1]), lambda = grid,
                           jeuEntraineSDnettoyE[[1]], alpha = 0.5, family = "binomial")
plo <- plot(cv.outMelangeSD)
bestlamMelangeSD <- cv.outMelangeSD$lambda.min
bestlamMelangeSD
pred.cvMelangeSD <- predict(cv.outMelangeSD, s=bestlamMelangeSD, newx=as.matrix(jeuTestSD[-1]), type = "class")

table(Vérité = jeuTestSD[[1]], Prédiction = pred.cvMelangeSD)

#### Tout le monde renvoie une proba!

retourneROC <- function(methode = c("RDA", "SVM", "régression", "tree", "KNN", "naif Bayes",
                                    "régression logistique", "naïf Bayes"),
  sousFamilleSVM = c("sigmoid", "linear", "polynomial", "radial"), 
  sousFamilleRegression = c("ridge", "lasso", "melange", "elastic net"),
  entraine = jeuEntraine[[1]], test = jeuTest[[1]], 
  retour = "ROC", ...) {
  if (retour == "ROC") 1 else 0
  probaListe <- list()
  
  if ("RDA" %in% methode) {
    rda.fit <- rda(patient ~ ., entraine)
    prob <- predict(rda.fit, test[, -1])$posterior[, "patient"]
    probaListe <- c(probaListe, list(RDA = prob))
  }
  
  if ("SVM" %in% methode) {
    svm.fit <- lapply(sousFamilleSVM, function (sF) {
      tune(svm, train.x = patient ~ ., data = entraine,
           kernel = sF, ranges = list(cost = 10^(-2:6), gamma = c(0.2, 0.5, 1:5)), 
           type = "C-classification")
    })
    
    names(svm.fit) <-  paste("SVM", sousFamilleSVM, sep = ".")
    
    prob <- lapply(svm.fit, function(svmFit) {
      attributes(predict(svmFit$best.model, newdata = test[, -1], decision.values = TRUE))$decision.values
    })
    probaListe <- c(probaListe, prob)
  }
  
  if ("régression" %in% methode) {
    saufElasticNet <- setdiff(sousFamilleRegression, "elastic net")
    
    if (length(saufElasticNet != 0)) {
      valeursAlpha <- lapply(saufElasticNet, function(sousMethode) 
        switch(sousMethode, ridge = 0, lasso = 1, melange = 0.5))
      reg.fit <- lapply(valeursAlpha, function(Alpha) {
        cv.glmnet(x = as.matrix(entraine[-1]), y = entraine[[1]], alpha = Alpha, family = "binomial")
      })
      prob <- lapply(reg.fit, function(regFit) {
        predict(regFit, s = regFit$lambda.min, newx = as.matrix(test[, -1]), type = "response")
      })
      
      names(prob) <-  paste("Regression", saufElasticNet, sep = ".")
      probaListe <- c(probaListe, prob)
    }
    
    if ("elastic net" %in% sousFamilleRegression) {
      expandedGrid <- expand.grid(.alpha = seq(.05, 1, length = 15), .lambda = 10^c((-10:2)))
      cctrl1 <- trainControl(method = "cv", number = 10, returnResamp = "all",
                             classProbs = TRUE, summaryFunction = twoClassSummary)
      
      elasticNet.fit <- train(as.matrix(entraine[-1]), 
                              entraine[[1]], 
                              method = "glmnet", 
                              trControl = cctrl1,
                              metric = "ROC",
                              preProc = c("center", "scale"),
                              tuneGrid = expandedGrid)
      prob <- predict(elasticNet.fit, test[-1], type = "prob")[, "patient"]
      probaListe <- c(probaListe, list(Regression.ENET = prob))
    }
  }
  
  if ("régression logistique" %in% methode) {
    regL.fit <- glm(formula = patient ~ ., data = entraine, family = "binomial")
    prob <- predict(regL.fit, test[-1])
    probaListe <- c(probaListe, list(Regression.logistique = prob))
  }

  
  if ("KNN" %in% methode) {
    cctrl2 <- trainControl(method="repeatedcv",repeats = 5) 
    knn.fit <- train(patient ~ ., data = entraine, method = "knn", 
                     trControl = cctrl2, preProcess = c("center","scale"), tuneLength = 12)
    prob <- predict(knn.fit, newdata = test[, -1], type = "prob")[, "patient"]
    
    probaListe <- c(probaListe, list(KNN = prob))
  }
  
  if ("naïf Bayes" %in% methode) {
    nB.fit <- naiveBayes(patient ~ ., entraine, laplace = 0)
    prob <- predict(nB.fit, test[-1], type = "raw")[, "patient"]
    probaListe <- c(probaListe, list(NaifBayes = prob))
  }
  
  rocListe <- lapply(probaListe, function(proba) roc(test[[1]], proba))
  plotListe <- lapply(rocListe, plot, print.auc = TRUE, auc.polygon = TRUE, grid = c(0.1, 0.2), 
                      grid.col = c("green", "red"),
                      max.auc.polygon = TRUE, auc.polygon.col = "skyblue", print.thres = TRUE,
                      plot = FALSE)
  return(plotListe)    
}

reTry <- retourneROC("régression")
reTry2 <- retourneROC("SVM")
reTry2
lapply(reTry, function(plota) {
  plot(1, type = "n", title = "look", xlim = c(1, 0), ylim = c(0, 1))
  plot(plota, print.auc = TRUE, auc.polygon = TRUE, grid = c(0.1, 0.2), 
       grid.col = c("green", "red"),
       max.auc.polygon = TRUE, auc.polygon.col = "skyblue", print.thres = TRUE, add = TRUE)
})

reTry3 <- retourneROC(c("KNN", "naif Bayes", "RDA"))
rereTry <- retourneROC(c("KNN", "naif Bayes", "RDA"), entraine = jeuEntraineSDnettoyE, test = jeuTestSDnettoyE)
reTrylogi <- retourneROC("régression logistique")
plot(reTrylogi$Regression.logistique, print.auc = TRUE, auc.polygon = TRUE, grid = c(0.1, 0.2), 
     grid.col = c("green", "red"),
     max.auc.polygon = TRUE, auc.polygon.col = "skyblue", print.thres = TRUE)
reTrylogi$Regression.logistique

rocPlotPT <- retourneROC()
plot(rocPlotPT$Regression.logistique, print.auc = TRUE, auc.polygon = TRUE, grid = c(0.1, 0.2), 
     grid.col = c("green", "red"),
     max.auc.polygon = TRUE, auc.polygon.col = "skyblue", print.thres = TRUE)
plot(rocPlotPT$SVM.sigmoid, print.auc = TRUE, auc.polygon = TRUE, grid = c(0.1, 0.2), 
     grid.col = c("green", "red"),
     max.auc.polygon = TRUE, auc.polygon.col = "skyblue", print.thres = TRUE)
title (main = "SVM.sigmoid", line = -11, cex.main = 1.25)




#### Emballer SChizoDepr
EntrainePsychomoSimpleSchizoDepr <- function(methode = c("RDA", "SVM", "régression", "tree", "naïf Bayes", "KNN"),
                                   sousFamilleSVM = c("sigmoid", "linear", "polynomial", "radial"), 
                                   sousFamilleRegression = c("ridge", "lasso", "melange", "elastic net"),
                                   fract2 = NA, 
                                   retour = "qualité", ...) {
  if (retour == "qualité") 1 else 0
  
  tableErreurs <- list()
  
  if(is.na(fract2)) fract2 <- fractionneSchizoVsDep()
  jeuEntraine <- sujets[c(fract2$schizoEntraine, fract2$depEntraine), c(3, 7:32)]
  jeuTest <- sujets[c(fract2$schizoTest, fract2$depTest), c(3, 7:32)]
  
  jeuEntraine$depression <- factor(jeuEntraine$depression, levels = c("depressif", "schizophrene"), ordered = TRUE)
  jeuTest$depression <- factor(jeuTest$depression, levels = c("depressif", "schizophrene"), ordered = TRUE)
  
  if ("RDA" %in% methode) {
    rda.fit <- rda(depression ~ ., jeuEntraine)
    rda.pred <- predict(rda.fit, jeuTest[-1])$class
    rda.tb <- table(Vérité = jeuTest[[1]], Prédiction = rda.pred)
    rda.fauxPositif <- rda.tb[1, 2]
    rda.fauxNegatif <- rda.tb[2, 1]
    rda.nombreErreurs <- rda.fauxNegatif + rda.fauxPositif
    
    tableErreurs <- c(tableErreurs, list(RDA = rda.tb))
  }
  
  if ("SVM" %in% methode) {
    svm.fit <- lapply(sousFamilleSVM, function (sF) {
      tune(svm, train.x = depression ~ ., data = jeuEntraine,
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
               levels = c("depressif", "schizophrene"), ordered = TRUE)
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
    tree.fit <- tree(depression ~ ., jeuEntraine)
    tree.pred <- predict(tree.fit, jeuTest[-1], type = "class")
    
    tree.tb <- table(Vérité = jeuTest[[1]], Prédiction = tree.pred)
    tree.fauxPositif <- tree.tb[1, 2]
    tree.fauxNegatif <- tree.tb[2, 1]
    tree.nombreErreurs <- tree.fauxNegatif + tree.fauxPositif
    
    tableErreurs <- c(tableErreurs, list(tree = tree.tb))
  }
  

  if ("KNN" %in% methode) {
    cctrl2 <- trainControl(method="repeatedcv",repeats = 5) 
    knn.fit <- train(depression ~ ., data = jeuEntraine, method = "knn", 
                     trControl = cctrl2, preProcess = c("center","scale"), tuneLength = 12)
    knn.pred <- predict(knn.fit, newdata = jeuTest[, -1])
    
    knn.tb <- table(Vérité = jeuTest[[1]], Prédiction = knn.pred)
    knn.fauxPositif <- knn.tb[1, 2]
    knn.fauxNegatif <- knn.tb[2, 1]
    knn.nombreErreurs <- knn.fauxNegatif + knn.fauxPositif
    
    tableErreurs <- c(tableErreurs, list(KNN = knn.tb))
  }
  
  if ("naïf Bayes" %in% methode) {
    nB.fit <- naiveBayes(depression ~ ., jeuEntraine, laplace = 0)
    nB.pred <- predict(nB.fit, jeuTest[-1])
    
    nB.tb <- table(Vérité = jeuTest[[1]], Prédiction = nB.pred)
    nB.fauxPositif <- nB.tb[1, 2]
    nB.fauxNegatif <- nB.tb[2, 1]
    nB.nombreErreurs <- nB.fauxNegatif + nB.fauxPositif
    
    tableErreurs <- c(tableErreurs, list(NaifBayes = nB.tb))
  }
  return (tableErreurs)
}


tbERRnB <- EntrainePsychomoSimpleSchizoDepr("naif Bayes")
tbERRnB
tbERRReg <- EntrainePsychomoSimpleSchizoDepr("régression")


EntrainePsychomoMultipleSchizoDepr <- function(fois = 5, affichage = "par méthode",
                                     methode = c("RDA", "SVM", "régression", 
                                                 "tree", "naïf Bayes", "KNN"),
                                     fract2liste = NA,
                                     sousFamilleSVM = c("sigmoid", "linear", "polynomial", "radial"), 
                                     sousFamilleRegression = c("ridge", "lasso", "melange", "elastic net"),
                                     retour = "qualité", ...) {
  if (is.na(fract2liste)) fract2liste <- rep(list(NA), fois) else
    fois = length(fract2liste)
  
  resultats <- lapply(1:fois, function(i) EntrainePsychomoSimpleSchizoDepr(methode = methode, 
                                                                           sousFamilleSVM = sousFamilleSVM,
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

tbERRKnn3 <- EntrainePsychomoMultipleSchizoDepr(3, methode = "KNN", fract2liste = fractsd)

tbERRSchizoDeprTout <- EntrainePsychomoMultipleSchizoDepr(fract2liste = fractsd)
tbERRSD5nB <- EntrainePsychomoMultipleSchizoDepr(methode = "naïf Bayes", fract2liste = fractsd)
tbERRSD5nB
cfSchizoDeprnB <- lapply(tbERRSD5nB, function(methode) {
  cf <- lapply(methode, confusionMatrix, positive = "schizophrene")
  Accuracy <- mean(sapply(cf, function(essai) essai$overall["Accuracy"]))
  Sensitivity <- mean(sapply(cf, function(essai) essai$byClass["Sensitivity"]))
  Specificity <- mean(sapply(cf, function(essai) essai$byClass["Specificity"]))
  Precision <- mean(sapply(cf, function(essai) essai$byClass["Precision"]))
  return(list(Accuracy = Accuracy, Sensitivity = Sensitivity, Specificity = Specificity, Precision = Precision))
}) 
perfoSchizoDeprnB <- do.call(rbind.data.frame, cfSchizoDeprnB)


cfSchizoDeprTout <- lapply(tbERRSchizoDeprTout, function(methode) {
  cf <- lapply(methode, confusionMatrix, positive = "schizophrene")
  Accuracy <- mean(sapply(cf, function(essai) essai$overall["Accuracy"]))
  Sensitivity <- mean(sapply(cf, function(essai) essai$byClass["Sensitivity"]))
  Specificity <- mean(sapply(cf, function(essai) essai$byClass["Specificity"]))
  Precision <- mean(sapply(cf, function(essai) essai$byClass["Precision"]))
  return(list(Accuracy = Accuracy, Sensitivity = Sensitivity, Specificity = Specificity, Precision = Precision))
})

perfoSchizoDeprTout <- do.call(rbind.data.frame, cfSchizoDeprTout)
perfoSchizoDeprTout


tbERRDSRDA <- EntrainePsychomoMultipleSchizoDepr(methode = "RDA", fract2liste = fractsd)
tbERRDSRDA
cfSchizoDeprRDAModifiE <- lapply(tbERRDSRDA, function(methode) {
  cf <- lapply(methode, confusionMatrix, positive = "schizophrene")
  Accuracy <- mean(sapply(cf, function(essai) essai$overall["Accuracy"]))
  Sensitivity <- mean(sapply(cf, function(essai) essai$byClass["Sensitivity"]))
  Specificity <- mean(sapply(cf, function(essai) essai$byClass["Specificity"]))
  Precision <- mean(sapply(cf, function(essai) essai$byClass["Precision"]))
  return(list(Accuracy = Accuracy, Sensitivity = Sensitivity, Specificity = Specificity, Precision = Precision))
})
# cfSchizoDeprRDAModifiE
perfoSchizoDeprRDA <- do.call(rbind.data.frame, cfSchizoDeprRDAModifiE)
perfoSchizoDeprTout["RDA", ] <- perfoSchizoDeprRDA

perfoSchizoDeprTout["Naïf Bayes", ] <- perfoSchizoDeprnB
