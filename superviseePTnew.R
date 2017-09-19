#### 1èrement essayons l'adaboost et forêts aléatoires

md.pattern(sujets) # only NA values

# install.packages("randomForest")
library(randomForest)

rf <- randomForest(patient ~ ., data = jeuEntraine)
rf$oob.times[1:10]
rf$predicted[1:10]  
table(jeuEntraine$patient, rf$predicted)
rf$confusion
rf$importance
plot(rf$err.rate[, 1], type="l")
pred.rf <- predict(rf,newdata = jeuTest[, -1])
table(Vérité = jeuTest[[1]], Prédiction = pred.rf)

# install.packages("fastAdaboost")
library(fastAdaboost)
fa <- adaboost(patient ~ ., data = jeuEntraine, nIter = 50)
fa
summary(fa)
fa.pred <- predict(fa, newdata = jeuTest[, -1])
table(Vérité = jeuTest[[1]], Prédiction = fa.pred$class)
fa.pred
fa.pred$class
jeuTest[49, ]


#### maintenant formater les méthodes avec 5 échantillons
set.seed(21007979)


fractpt <- lapply(1:5, function(x) fractionnePatientsVsTemoins())
fractpt

fractsd <- lapply(1:5, function(x) fractionneSchizoVsDep())
fractsd

tbErrRDARegSVM <- EntrainePsychomoMultiple(methode = c("RDA", "SVM", "régression"), fract2liste = fractpt)
save(fractpt, tbErrRDARegSVM, file = "fractptRDARegSVM.RData")

tbErrRDARegSVM$Regression.elasticNet
tbErrRDARegSVM$Regression.lasso
tbErrRDARegSVM$RDA
cftry <- confusionMatrix(tbErrRDARegSVM$RDA$Essai.4, positive = "patient")

cfRDARegSVM <- lapply(tbErrRDARegSVM, function(methode) {
  cf <- lapply(methode, confusionMatrix, positive = "patient")
  Accuracy <- mean(sapply(cf, function(essai) essai$overall["Accuracy"]))
  Sensitivity <- mean(sapply(cf, function(essai) essai$byClass["Sensitivity"]))
  Specificity <- mean(sapply(cf, function(essai) essai$byClass["Specificity"]))
  Precision <- mean(sapply(cf, function(essai) essai$byClass["Precision"]))
  return(list(Accuracy = Accuracy, Sensitivity = Sensitivity, Specificity = Specificity, Precision = Precision))
})

dctry <- do.call(rbind.data.frame, cfRDARegSVM)
class(cfRDARegSVM$RDA)
class(dctry)
str(dctry)



jeuEntraine <- lapply(fractpt, function(essai) sujets[c(essai$patEntraine, essai$temEntraine), c(2, 7:32)])
jeuTest <- lapply(fractpt, function(essai) sujets[c(essai$patTest, essai$temTest), c(2, 7:32)])

### Régression linéaire, seulement table d'erreurs
rl.fit <- lapply(jeuEntraine, function(essai) glm(formula = patient ~ ., data = essai, 
    family = binomial(link = "logit")))
rl.fit
rl.fitted <- lapply(1:length(rl.fit), function(i) ifelse (predict(rl.fit[[i]], newdata = jeuTest[[i]][, -1], 
                                                            type = "response") > 0.5, "patient", "temoin"))
rl.fitted <- lapply(rl.fitted, factor, ordered = TRUE, levels = c("temoin", "patient"))
rl.tb <- lapply(1:length(rl.fit), function(i) table(Vérité = jeuTest[[i]][[1]], Prédiction = rl.fitted[[i]]))
rl.tb
rl.cf <- lapply(rl.tb, confusionMatrix, positive = "patient")
Regression.classique <- c(mean(sapply(rl.cf, function(essai) essai$overall["Accuracy"])), 
                          mean(sapply(rl.cf, function(essai) essai$byClass["Sensitivity"])),
                          mean(sapply(rl.cf, function(essai) essai$byClass["Specificity"])),
                          mean(sapply(rl.cf, function(essai) essai$byClass["Precision"])))
Regression.classique
accuracyRDARegSVM <- rbind(dctry, Regression.classique)
rownames(accuracyRDARegSVM)[[10]] <- "Regression.classique"
save(fractpt, jeuEntraine, jeuTest, tbErrRDARegSVM, accuracyRDARegSVM, file = "fractptRDARegSVM.RData")

### les coeffs des méthodes, Regs
cv.out.ridge <- cv.glmnet(as.matrix(jeuEntraine[[1]][, -1]), lambda = grid,
                    jeuEntraine[[1]][[1]], alpha = 0, family = "binomial")

cv.out.lasso <- cv.glmnet(as.matrix(jeuEntraine[[1]][, -1]), lambda = grid,
                          jeuEntraine[[1]][[1]], alpha = 1, family = "binomial")
cv.out.melange <- cv.glmnet(as.matrix(jeuEntraine[[1]][, -1]), lambda = grid,
                          jeuEntraine[[1]][[1]], alpha = 0.5, family = "binomial")



coeffReg <- cbind(coef(cv.out.ridge), coef(cv.out.lasso), coef(cv.out.melange))
colnames(coeffReg) <- c("Ridge", "Lasso", "Melange")
coeffReg
save(coeffReg, file = "coeffReg")

### RDA
rda.fit <- lapply(jeuEntraine, function(essai) rda(patient ~ ., essai))
rda.fit[[1]]$regularization
lapply(rda.fit, function(Fit) Fit$regularization)
rda.pred <- lapply(1:length(rda.fit), function(i) predict(rda.fit[[i]], newdata = jeuTest[[i]][, -1])$class)
rda.tb <- lapply(1:length(rda.fit), function(i) table(Vérité = jeuTest[[i]][[1]], Prédiction = rda.pred[[i]]))
rda.tb
rda.para <- do.call(rbind, lapply(rda.fit, function(essai) essai$regularization))
rownames(rda.para) <- paste("Essai", 1:5, sep = ".")
names(rda.tb) = rownames(rda.para)
rda.tb
rda.cf <- lapply(rda.tb, confusionMatrix, positive = "patient")
RDAaccuracy <- c(mean(sapply(rda.cf, function(essai) essai$overall["Accuracy"])), 
                          mean(sapply(rda.cf, function(essai) essai$byClass["Sensitivity"])),
                          mean(sapply(rda.cf, function(essai) essai$byClass["Specificity"])),
                          mean(sapply(rda.cf, function(essai) essai$byClass["Precision"])))

accuracyRDARegSVM["RDA", ] <- RDAaccuracy
save(fractpt, jeuEntraine, jeuTest, tbErrRDARegSVM, accuracyRDARegSVM, file = "fractptRDARegSVM.RData")
# accuracyRDARegSVM


### KNN et tree
tbErrKnnTree <- EntrainePsychomoMultiple(methode = c("KNN", "tree"), fract2liste = fractpt)

cfKnnTree <- lapply(tbErrKnnTree, function(methode) {
  cf <- lapply(methode, confusionMatrix, positive = "patient")
  Accuracy <- mean(sapply(cf, function(essai) essai$overall["Accuracy"]))
  Sensitivity <- mean(sapply(cf, function(essai) essai$byClass["Sensitivity"]))
  Specificity <- mean(sapply(cf, function(essai) essai$byClass["Specificity"]))
  Precision <- mean(sapply(cf, function(essai) essai$byClass["Precision"]))
  return(list(Accuracy = Accuracy, Sensitivity = Sensitivity, Specificity = Specificity, Precision = Precision))
})
dc.knnTree <- do.call(rbind.data.frame, cfKnnTree)
dc.knnTree

tbErrnB <- EntrainePsychomoMultiple(methode = c("naïf Bayes"), fract2liste = fractpt)

cfnB <- lapply(tbErrnB, function(methode) {
  cf <- lapply(methode, confusionMatrix, positive = "patient")
  Accuracy <- mean(sapply(cf, function(essai) essai$overall["Accuracy"]))
  Sensitivity <- mean(sapply(cf, function(essai) essai$byClass["Sensitivity"]))
  Specificity <- mean(sapply(cf, function(essai) essai$byClass["Specificity"]))
  Precision <- mean(sapply(cf, function(essai) essai$byClass["Precision"]))
  return(list(Accuracy = Accuracy, Sensitivity = Sensitivity, Specificity = Specificity, Precision = Precision))
})
dc.nB <- do.call(rbind.data.frame, cfnB)
dc.nB
accuracyRDARegSVM["Naif.Bayes", ] 
rownames(accuracyRDARegSVM)[length(accuracyRDARegSVM)]
save(fractpt, fractsd, jeuEntraine, jeuTest, tbErrRDARegSVM, rocPlotPT,
     accuracyRDARegSVM, perfoSchizoDeprTout, tbnB01, nB.fit, nB.fit2, file = "fractptRDARegSVM.RData")
rownames(accuracyRDARegSVM)[rownames(accuracyRDARegSVM) == "Naïf Bayes"] <- "Naif.Bayes"
colnames(accuracyRDARegSVM)
rownames(perfoSchizoDeprTout)[rownames(perfoSchizoDeprTout) == "Naïf Bayes"] <- "Naif.Bayes"
