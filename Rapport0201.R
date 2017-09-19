#' ---
#' title: Classification patients vs. témoins
#' author: Xian YANG
#' date: 19 juillet 2017
#' output:
#'    html_document:
#'      toc: true
#'      highlight: zenburn
#' ---
#' 
#+ echo=FALSE, warning=FALSE, message=FALSE, results='hide'
load("jeunesV3.RData")

library(ggplot2)
library(reshape2)
library(dplyr)
library(plotly)
library(ggfortify)
library("RColorBrewer")
library(glmnet)
library(e1071) # Pour svm
library(caret) # pour tuner
library(ROCR)
library(MASS)
library(klaR)
library(class)
library(tree)

library(dplyr)

set.seed(21007979)

#### Créons quelques couleurs ####
## display.brewer.pal(n = 11, name = 'RdBu')
couleurs <- brewer.pal(n = 11, name = 'RdBu')
#' 
#' ## Hiérarchisation des critères par leur sensibilité pour schizophrènes vs. dépressifs
#' C'est la continuation du travail de la semaine dernière. Cette fois-ci, les analyses sont transmises au 
#' jeu de données "V3" (donc avec 6 patients de plus). Pour l'intégralité de l'article on reprend d'abord
#' l'hiérarchisation patients vs. témoins, puis la compare avec la nouvelle hiérarchisation schizophrènes
#' vs. dépressifs.  
#' Le test utilisé est toujours celui de Man-Whitney.  
#' 
#' ### Barplots des critères dans l'ordre décroissant de leur importance, Témoins vs. Patients 
#' marron : _patients_, bleu : _témoins_, 0 : _raté_, 1 : _réussi_.
#+ echo=FALSE, warning=FALSE, message=FALSE


testWM <- sapply(7:32, function(i) {
  return(wilcox.test(patients %>% pull(i), temoins %>% pull(i), alternative = "less")$p.value)
}, USE.NAMES = TRUE)
names(testWM) <- 7:32
testWMSorted <- sort(testWM)

oldPar <- par(mfrow = c(2, 3))
plot_par_critere_ordered <- lapply(as.integer(names(testWMSorted)), function(i) {
  tb <- table(sujets[c(2, i)])
  barplot(tb, # main=,
          xlab=paste(colnames(sujets)[i], "\n p : ", signif(testWM[as.character(i)], digits = 3)),
          col=c("brown", "darkblue"),
          # legend = rownames(counts),
          beside=TRUE)
})
par(oldPar)

#' ### Barplots des critères dans l'ordre décroissant de leur importance, Témoins vs. Dépressifs 
#' 
#' orange foncé : _dépressifs_,  : rouge : _patients_, 0 : _raté_, 1 : _réussi_.
#+ echo=FALSE, warning=FALSE, message=FALSE
testWM3 <- sapply(7:32, function(i) {
return(wilcox.test(patients[patients$depression == "schizophrene", ] %>% pull(i), 
                   patients[patients$depression == "depressif", ] %>% pull(i), alternative = "less")$p.value)
}, USE.NAMES = TRUE)
names(testWM3) <- 7:32
testWMSorted3 <- sort(testWM3)

oldPar <- par(mfrow = c(2, 3))
plot_par_critere_ordered <- lapply(as.integer(names(testWMSorted3)), function(i) {
  tb <- table(patients[c(3, i)], exclude = "autre")
  barplot(tb, # main=,
          xlab=paste(colnames(patients)[i], "\n p : ", signif(testWM3[as.character(i)], digits = 3)),
          col=c("darkred", "darkorange"),
          # legend = rownames(counts),
          beside=TRUE)
})
par(oldPar)
#' Quels sont les critères ayant une grande augmentation ou descende en termes de rang de sensibilité 
#' ( >= 5) ?
#'  
#+ echo=FALSE, warning=FALSE, message=FALSE
rangBouge <- sapply(as.character(7:32), function(i) {
  return(abs((which(i == names(testWMSorted))) - (which(i == names(testWMSorted3)))) >= 5)
})

for(i in 7:32) {
  if(rangBouge[i-6]) {
    mess <- paste(names(patients)[i], "\n",
                  "Son rang dans le test Patients vs. Témoins est", (which(as.character(i) == names(testWMSorted))),
                  "avec une p-valeur", signif(testWMSorted[as.character(i)], digits = 3), "\n",
                  "Son rang dans le test Schizo. vs. Dépr. est", (which(as.character(i) == names(testWMSorted3))),
                  "avec une p-valeur", signif(testWMSorted3[as.character(i)], digits = 3), "\n\n"
                  )
    cat(mess)
  }
}
#' Il convient de remarquer que la sensibilité des critères entre patients et témoins est en général beacoup
#' plus importante que celle entre schizophrènes et dépressifs. Ça veut dire qu'un rang très en arrière en 
#' termes de patients vs. témoins n'implique pas forcément une sensibilité plus faible qu'un rang très en 
#' avant en termes de schizophrènes vs. dépressifs. Un bon exemple est le premier résultat si-dessus.
#' 
#' ## Méthode 1 (famille) : Classification patients vs. témoins par méthode de régression logistique
#' La semaine dernière on a constaté, dans le graphique d'ACP, que les patients et les témoins se regroupent entre
#' eux (en revanche ce n'est apparemment pas le cas de schizophrènes vs. dépressifs). On peut donc penser à
#' trouver une méthode, qui sert à classfier un sujet dans le sous-groupe patients ou le sous-groupe témoins 
#' uniquement à partir de ses résultats psychomoteurs. L'étude du problème de classification schizoprènes vs.
#' dépressifs étant plus difficile à cause d'un considérable mélange l'un dans l'autre, je la laisse pour la
#' sémaine prochaine.  
#'   
#' Pour l'instant, on se concentre principalement sur la qualité des différentes méthodes de classification.
#' Ceci pour l'instant toujours à l'aide de la table d'erreurs de la prédiction de la méthode. Les détailles
#' mathématiques de chaque méthode seront mises en évidence plus tard mais, ne sont pas notre première priorité 
#' pour le moment.   
#'    
#' On verra dans la suite que certaines méthodes délivrent des résultats plus robustes que les autres. 
#' Sachant qu'aucune méthode puisse toujours donner une réponse parfaite sans erreur, on verra également 
#' que certaines méthodes préfèrent donner de faux positifs alors que certaines préfèrent donner de faux négatifs. 
#' On pourra donc poser la question : peut-on camparer et combiner de différentes méthodes avec de préférences
#' opposées, de façon que le résultat de prédiction puisse être amlioré ? Cette question est très intéressante
#' et sera aussi être étudiée une autre fois.  
#' 
#' ### Méthode de travail
#' 
#+ echo=FALSE, warning=FALSE, message=FALSE
fractionnePatientsVsTemoins <- function(tailleTest = c(25, 25), jeuDonnees = sujets) {
  patTest <- sample(x = which(sujets$patient == "patient"), size = tailleTest[1])
  temTest <- sample(x = which(sujets$patient == "temoin"), size = tailleTest[2])
  patEntraine <- setdiff(which(sujets$patient == "patient"), patTest)
  temEntraine <- setdiff(which(sujets$patient == "temoin"), temTest)
  return(list(patEntraine = patEntraine, temEntraine = temEntraine, 
              patTest = patTest, temTest = temTest))
}
fract2 <- fractionnePatientsVsTemoins()
jeuEntraine <- sujets[c(fract2$patEntraine, fract2$temEntraine), c(2, 7:32)]
jeuTest <- sujets[c(fract2$patTest, fract2$temTest), c(2, 7:32)]
fract3 <- fractionnePatientsVsTemoins()
jeuEntraine2 <- sujets[c(fract3$patEntraine, fract3$temEntraine), c(2, 7:32)]
jeuTest2 <- sujets[c(fract3$patTest, fract3$temTest), c(2, 7:32)]
fract4 <- fractionnePatientsVsTemoins()
jeuEntraine3 <- sujets[c(fract4$patEntraine, fract4$temEntraine), c(2, 7:32)]
jeuTest3 <- sujets[c(fract4$patTest, fract4$temTest), c(2, 7:32)]
#'  En principe, d'ailleurs pour les autres familles de méthodes statistiques dans la suite aussi, je découpe au
#'  hasard l'ensemble des sujets en 2 parties, un jeu d'entraînement et un jeu de test, de manière que le jeu 
#'  de test est composé de 25 patients et 25 témoins. Il y a donc 52 patients et 46 témoins dans le jeu 
#'  d'entraînement. J'étudie d'abord un classifieur (une règle de classification, ou encore une frontière) 
#'  sur le jeu d'entraïnement, 
#'  c'est-à-dire sans connaitre au préalable aucune information sur lees sujets qui se trouvent dans le jeu de test.
#'  En suite, je vérifierai ce classifieur sur le jeu de test, pour en tirer la table d'erreurs. Cette table 
#'  d'erreurs est une très bonne mesure de la qualité de prédiction de la méthode, qui comprend toutefois 
#'  du hasard car les membres du jeu de test sont choisis au hasard. Je répète donc 3 fois cette procédure,
#'  afin que l'erreur aléatoire reste relativement minuscule.
#'  
#'  
#' ### Régression logistique simple
#'  
#' Une centaine d'observations contre 26 variables, le graphique patients vs. témoins possédant une forme
#' qui ne s'oppose pas de façon considérable à une frontière linéaire, tout ça nous dit de commencer par 
#' les méthodes linéraies les plus classiques : régression logistique. On montre tout d'abord la variante 
#' la plus simple, i.e. la régression logistique sans régularisation. Cependant, il convient de savoir que,
#' pour un problème de classification à 26 variables et 98 observations, qui subit en plus pas mal de 
#' co-linéarité puisque les scores ne sont que binaires, la dimension (nombre de variable) est 
#' sans doute un peu trop élevée pour le nombre d'observation. Il en résulte que sans régularisation,
#' la régression logistique va être parfaitement performante sur le jeu d'entraînement (aucune faute), alors
#' qu'elle est susceptible d'avoir une mauvaise qualité de prédiction. On parle d'un sur-appretissage.
#+ echo=FALSE, warning=FALSE, message=FALSE

regOrdi2 <- glm(formula = patient ~ ., data = jeuEntraine, family = binomial)

summary(regOrdi2)
#' On observe qu'aucun coefficient n'est significatif ! Cela mène à une prédiction d'une relativement
#' mauvaise qualité :
#+ echo=FALSE, warning=FALSE, message=FALSE
pred1 <- predict(regOrdi2, newdata = jeuTest[, -1], type = "response")
pred1 <- ifelse(pred1 > 0.5, "patient", "temoin")
table1 <- table(Réalité = jeuTest[, 1], Prédiction = pred1)

regOrdi3 <- glm(formula = patient ~ ., data = jeuEntraine2, family = binomial)
pred2 <- predict(regOrdi3, newdata = jeuTest2[, -1], type = "response")
pred2 <- ifelse(pred2 > 0.5, "patient", "temoin")
table2 <- table(Réalité = jeuTest2[, 1], Prédiction = pred2)

regOrdi4 <- glm(formula = patient ~ ., data = jeuEntraine3, family = binomial)
pred3 <- predict(regOrdi4, newdata = jeuTest3[, -1], type = "response")
pred3 <- ifelse(pred3 > 0.5, "patient", "temoin")
table3 <- table(Réalité = jeuTest3[, 1], Prédiction = pred3)

table1
table2
table3
#' Le nombre d'erreurs est ```r table1[1, 2] + table1[2, 1]```, ce qui correspond à un taux d'erreurs égale
#' à ```r (table1[1, 2] + table1[2, 1]) / sum(table1)```.
#' 
#' ### Régression logistique avec régularisation Lasso
#' 
#+ echo=FALSE, warning=FALSE, message=FALSE
cvLasso <- cv.glmnet(as.matrix(jeuEntraine[, -1]), 
                     jeuEntraine[, 1], alpha = 1, family = "binomial")
summary(cvLasso)
bestlam <- cvLasso$lambda.min
bestlam
coef(cvLasso)
pred4 <- predict(cvLasso, s=bestlam, newx = as.matrix(jeuTest[, -1]), type = "response")
pred4 <- ifelse(pred4 > 0.5, "patient", "temoin")
table4 <- table(Réalité = jeuTest[, 1], Prédiction = pred4)

plot(cvLasso)

cvLasso2 <- cv.glmnet(as.matrix(jeuEntraine2[, -1]), 
                     jeuEntraine2[, 1], alpha = 1, family = "binomial")
bestlam2 <- cvLasso2$lambda.min
pred5 <- predict(cvLasso2, s=bestlam2, newx = as.matrix(jeuTest2[, -1]), type = "response")
pred5 <- ifelse(pred5 > 0.5, "patient", "temoin")
table5 <- table(Réalité = jeuTest2[, 1], Prédiction = pred5)


cvLasso3 <- cv.glmnet(as.matrix(jeuEntraine3[, -1]), 
                      jeuEntraine3[, 1], alpha = 1, family = "binomial")
bestlam3 <- cvLasso3$lambda.min
pred6 <- predict(cvLasso3, s=bestlam3, newx = as.matrix(jeuTest3[, -1]), type = "response")
pred6 <- ifelse(pred6 > 0.5, "patient", "temoin")
table6 <- table(Réalité = jeuTest3[, 1], Prédiction = pred6)

table4
table5
table6

#' Le nombre d'erreurs est amélioré et se lève à ```r table2[1, 2] + table2[2, 1]```, 
#' ce qui correspond à un taux d'erreurs égale
#' à ```r (table2[1, 2] + table2[2, 1]) / sum(table2)```.
#' 
#' ### Régression logistique avec régularisation Ridge
#+ echo=FALSE, warning=FALSE, message=FALSE
cvRidge <- cv.glmnet(as.matrix(jeuEntraine[, -1]), lambda = 10^seq(1, -5, length = 100),
                     jeuEntraine[, 1], alpha = 0, family = "binomial")
summary(cvRidge)
bestlam4 <- cvRidge$lambda.min
bestlam4
coef(cvRidge)
pred7 <- predict(cvRidge, s=bestlam4, newx = as.matrix(jeuTest[, -1]), type = "response")
pred7 <- ifelse(pred7 > 0.5, "patient", "temoin")
table7 <- table(Réalité = jeuTest[, 1], Prédiction = pred7)
plot(cvRidge)

cvRidge2 <- cv.glmnet(as.matrix(jeuEntraine2[, -1]), 
                      jeuEntraine2[, 1], alpha = 0, family = "binomial")
bestlam5 <- cvRidge2$lambda.min
pred8 <- predict(cvRidge2, s=bestlam5, newx = as.matrix(jeuTest2[, -1]), type = "response")
pred8 <- ifelse(pred8 > 0.5, "patient", "temoin")
table8 <- table(Réalité = jeuTest2[, 1], Prédiction = pred8)


cvRidge3 <- cv.glmnet(as.matrix(jeuEntraine3[, -1]), 
                      jeuEntraine3[, 1], alpha = 0, family = "binomial")
bestlam6 <- cvRidge3$lambda.min
pred9 <- predict(cvLasso3, s=bestlam6, newx = as.matrix(jeuTest3[, -1]), type = "response")
pred9 <- ifelse(pred9 > 0.5, "patient", "temoin")
table9 <- table(Réalité = jeuTest3[, 1], Prédiction = pred9)

table7
table8
table9
# Le nombre d'erreurs se lève à ```r table3[1, 2] + table3[2, 1]```, 
# ce qui correspond à un taux d'erreurs égale
# à ```r (table3[1, 2] + table3[2, 1]) / sum(table3)```.
#' 
#' ### Régression logistique avec régularisation réseau élastique (elastic net)
#+ echo=FALSE, warning=FALSE, message=FALSE
cvElasticNet <- cv.glmnet(as.matrix(jeuEntraine[, -1]), 
                     jeuEntraine[, 1], alpha = 0.5, family = "binomial")

bestlam <- cvElasticNet$lambda.min
bestlam
coef(cvElasticNet)
pred4 <- predict(cvElasticNet, s=bestlam, newx = as.matrix(jeuTest[, -1]), type = "response")
pred4 <- ifelse(pred4 > 0.5, "patient", "temoin")
table4 <- table(Réalité = jeuTest[, 1], Prédiction = pred4)
table4
plot(cvElasticNet)
#' Le nombre d'erreurs se lève à ```r table4[1, 2] + table4[2, 1]```, 
#' ce qui correspond à un taux d'erreurs égale
#' à ```r (table4[1, 2] + table4[2, 1]) / sum(table4)```.
#' 
#' ### Et si on fait varier aussi alpha ?
#' 
#+ echo=FALSE, warning=FALSE, message=FALSE
expandedGrid <- expand.grid(.alpha = seq(.05, 1, length = 15), .lambda = 10^c((-10:2)))
cctrl1 <- trainControl(method = "cv", number = 10, returnResamp = "all",
                       classProbs = TRUE, summaryFunction = twoClassSummary)
test_class_cv_model <- train(as.matrix(jeuEntraine[, -1]), 
                             jeuEntraine[, 1], 
                             method = "glmnet", 
                             trControl = cctrl1,
                             metric = "ROC",
                             preProc = c("center", "scale"),
                             tuneGrid = expandedGrid)
test_class_pred <- predict(test_class_cv_model, as.matrix(sujets[c(fract2$patTest, fract2$temTest), 7:32]))
table5 <- table(Réalité = jeuTest[, 1], Prédiction = test_class_pred)
table5
test_class_cv_model$bestTune
plot(test_class_cv_model)

#' ## Méthode 2 (famille) : Classification patients vs. témoins par méthode de SVM
#' ### SVM avec le noyau radial  
#+ echo=FALSE, warning=FALSE, message=FALSE
tune.out <- tune(svm, train.x = patient ~ ., data = jeuEntraine,
                 kernel = "radial", ranges = list(cost = 5^(-2:6), gamma = c(0.2, 0.5, 1:4)), 
                 type = "C-classification")
summary(tune.out)

pred6 <- predict(tune.out$best.model, newdata =jeuTest[, -1])
table6 <- table(Réalité = jeuTest[, 1], Prédiction = pred6)
table6

svmfit <- svm(formula = patient ~ ., data = jeuEntraine,
              kernel = "radial", gamma = 0.2, cost = 1e5, type = "C-classification")
pred7 <- predict(svmfit, newdata =jeuTest[, -1])
table7 <- table(Réalité = jeuTest[, 1], Prédiction = pred7)
table7

## The best SVM radial ! à étudier. Une seule erreur. Mais ce résultat n'est pas réalistique. 
svmfit2 <- svm(formula = patient ~ ., data = jeuEntraine,
              kernel = "radial", gamma = 0.2, cost = 0.1, type = "C-classification")
pred8 <- predict(svmfit2, newdata =jeuTest[, -1])
table8 <- table(Réalité = jeuTest[, 1], Prédiction = test_class_pred)
table8

#' Avec deux autres exemples entraînés de la même façon :
#' 
#+ echo=FALSE, warning=FALSE, message=FALSE
tune.out2 <- tune(svm, train.x = patient ~ ., data = jeuEntraine2,
                 kernel = "radial", ranges = list(cost = 5^(-2:6), gamma = c(0.2, 0.5, 1:4)), 
                 type = "C-classification")
pred9 <- predict(tune.out2$best.model, newdata =jeuTest2[, -1])
table9 <- table(Réalité = jeuTest2[, 1], Prédiction = pred9)
tune.out3 <- tune(svm, train.x = patient ~ ., data = jeuEntraine3,
                 kernel = "radial", ranges = list(cost = 5^(-2:6), gamma = c(0.2, 0.5, 1:4)), 
                 type = "C-classification")
pred10 <- predict(tune.out3$best.model, newdata =jeuTest3[, -1])
table10 <- table(Réalité = jeuTest3[, 1], Prédiction = pred10)
table6
table9
table10
## ROC function
rocplot <- function(pred, truth, ...) {
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  plot(perf, ...)
}
#' ### SVM avec le noyau polynomial
#'
#+ echo=FALSE, warning=FALSE, message=FALSE
tune.out <- tune(svm, train.x = patient ~ ., data = sujets[c(fract2$patEntraine, fract2$temEntraine), c(2, 7:32)],
                 kernel = "polynomial", ranges = list(cost = 5^(-2:6), gamma = c(0.2, 0.5, 1:4)), 
                 type = "C-classification")

pred6 <- predict(tune.out$best.model, newdata =jeuTest[, -1])
table6 <- table(Réalité = jeuTest[, 1], Prédiction = pred6)

tune.out2 <- tune(svm, train.x = patient ~ ., data = jeuEntraine2,
                  kernel = "polynomial", ranges = list(cost = 5^(-2:6), gamma = c(0.2, 0.5, 1:4)), 
                  type = "C-classification")
pred9 <- predict(tune.out2$best.model, newdata =jeuTest2[, -1])
table9 <- table(Réalité = jeuTest2[, 1], Prédiction = pred9)
tune.out3 <- tune(svm, train.x = patient ~ ., data = jeuEntraine3,
                  kernel = "polynomial", ranges = list(cost = 5^(-2:6), gamma = c(0.2, 0.5, 1:4)), 
                  type = "C-classification")
pred10 <- predict(tune.out3$best.model, newdata =jeuTest3[, -1])
table10 <- table(Réalité = jeuTest3[, 1], Prédiction = pred10)

table6
table9
table10
# ## Training ROC
# FittedSVMRadial <- attributes(predict(tune.out$best.model, jeuEntraine[, -1], 
#                              decision.values = TRUE))$decision.values
# rocplot(FittedSVMRadial, jeuEntraine[, 1], main="Training ROC, SVM radial")
# 
# ## Test ROC
# predSVMRadial <- attributes(predict(tune.out$best.model, jeuTest[, -1], 
#                               decision.values = TRUE))$decision.values
# rocplot(predSVMRadial, jeuTest[, 1], main="Test ROC, SVM radial")
#'
#' ### SVM avec le noyau linéaire  
#' Ceci est avec le noyau linéaire.
#+ echo=FALSE, warning=FALSE, message=FALSE
tune.out <- tune(svm, train.x = patient ~ ., data = sujets[c(fract2$patEntraine, fract2$temEntraine), c(2, 7:32)],
                 kernel = "linear", ranges = list(cost = 5^(-2:6), gamma = c(0.2, 0.5, 1:4)), 
                 type = "C-classification")

pred6 <- predict(tune.out$best.model, newdata =jeuTest[, -1])
table6 <- table(Réalité = jeuTest[, 1], Prédiction = pred6)

tune.out2 <- tune(svm, train.x = patient ~ ., data = jeuEntraine2,
                  kernel = "linear", ranges = list(cost = 5^(-2:6), gamma = c(0.2, 0.5, 1:4)), 
                  type = "C-classification")
pred9 <- predict(tune.out2$best.model, newdata =jeuTest2[, -1])
table9 <- table(Réalité = jeuTest2[, 1], Prédiction = pred9)
tune.out3 <- tune(svm, train.x = patient ~ ., data = jeuEntraine3,
                  kernel = "linear", ranges = list(cost = 5^(-2:6), gamma = c(0.2, 0.5, 1:4)), 
                  type = "C-classification")
pred10 <- predict(tune.out3$best.model, newdata =jeuTest3[, -1])
table10 <- table(Réalité = jeuTest3[, 1], Prédiction = pred10)

table6
table9
table10
#' ### SVM avec le noyau sigmoïde
#'
#+ echo=FALSE, warning=FALSE, message=FALSE
tune.out <- tune(svm, train.x = patient ~ ., data = sujets[c(fract2$patEntraine, fract2$temEntraine), c(2, 7:32)],
                 kernel = "sigmoid", ranges = list(cost = 5^(-2:6), gamma = c(0.2, 0.5, 1:4)), 
                 type = "C-classification")

pred6 <- predict(tune.out$best.model, newdata =jeuTest[, -1])
table6 <- table(Réalité = jeuTest[, 1], Prédiction = pred6)

tune.out2 <- tune(svm, train.x = patient ~ ., data = jeuEntraine2,
                  kernel = "sigmoid", ranges = list(cost = 5^(-2:6), gamma = c(0.2, 0.5, 1:4)), 
                  type = "C-classification")
pred9 <- predict(tune.out2$best.model, newdata =jeuTest2[, -1])
table9 <- table(Réalité = jeuTest2[, 1], Prédiction = pred9)
tune.out3 <- tune(svm, train.x = patient ~ ., data = jeuEntraine3,
                  kernel = "sigmoid", ranges = list(cost = 5^(-2:6), gamma = c(0.2, 0.5, 1:4)), 
                  type = "C-classification")
pred10 <- predict(tune.out3$best.model, newdata =jeuTest3[, -1])
table10 <- table(Réalité = jeuTest3[, 1], Prédiction = pred10)

table6
table9
table10
#' ## Méthode 3 (famille) : méthode d'analyse discriminante
#' ### Analyse discriminante linéaire
#+ echo=FALSE, warning=FALSE, message=FALSE
lda.fit <- lda(patient ~ ., jeuEntraine)
lda.pred <- predict(lda.fit, jeuTest[-1])
lda.class <- lda.pred$class
table1 <- table(true = jeuTest[[1]], pred = lda.class)

lda.fit2 <- lda(patient ~ ., jeuEntraine2)
lda.pred2 <- predict(lda.fit2, jeuTest2[-1])
lda.class2 <- lda.pred2$class
table2 <- table(true = jeuTest2[[1]], pred = lda.class2)

lda.fit3 <- lda(patient ~ ., jeuEntraine3)
lda.pred3 <- predict(lda.fit3, jeuTest3[-1])
lda.class3 <- lda.pred3$class
table3 <- table(true = jeuTest3[[1]], pred = lda.class3)

table1
table2
table3

#' ### Analyse discriminante combinée
#+ echo=FALSE, warning=FALSE, message=FALSE
rda.fit <- rda(patient ~ ., jeuEntraine, gamma = 0.1, lambda = 0.5)
rda.pred <- predict(rda.fit, jeuTest[-1])
rda.class <- rda.pred$class
table <- table(true = jeuTest[[1]], pred = rda.class)

rda.fit2 <- rda(patient ~ ., jeuEntraine2, gamma = 0.1, lambda = 0.5)
rda.pred2 <- predict(rda.fit2, jeuTest2[-1])
rda.class2 <- rda.pred2$class
table2 <- table(true = jeuTest2[[1]], pred = rda.class2)

rda.fit3 <- rda(patient ~ ., jeuEntraine3, gamma = 0.1, lambda = 0.5)
rda.pred3 <- predict(rda.fit3, jeuTest3[-1])
rda.class3 <- rda.pred3$class
table3 <- table(true = jeuTest3[[1]], pred = rda.class3)

table1
table2
table3

#' ## Méthode 4 : méthode de décision d'arbres 
#+ echo=FALSE, warning=FALSE, message=FALSE
tree.mod <- tree(patient ~ ., jeuEntraine)
summary(tree.mod)
plot(tree.mod)
text(tree.mod, pretty=0)
tree.mod
tree.pred <- predict(tree.mod, jeuTest[,-1],type = "class")
table1 <- table(true = jeuTest[[1]], pred = tree.pred)

tree.mod2 <- tree(patient ~ ., jeuEntraine2)
tree.pred2 <- predict(tree.mod2, jeuTest2[,-1],type = "class")
table2 <- table(true = jeuTest2[[1]], pred = tree.pred2)

tree.mod3 <- tree(patient ~ ., jeuEntraine3)
tree.pred3 <- predict(tree.mod3, jeuTest3[,-1],type = "class")
table3 <- table(true = jeuTest3[[1]], pred = tree.pred3)

table1
table2
table3
#' ## Méthode 5 : méthode KNN (des k plus proches voisins)
#' Cette méthode ayant une taux d'erreurs élevée, elle a pour avantage qu'elle ne se trompe prèsque jamais 
#' chez les témoins, car les témoins sont spacialement très troches les uns des autres, au contraires 
#' des patients.
#+ echo=FALSE, warning=FALSE, message=FALSE
cctrl2 <- trainControl(method="repeatedcv",repeats = 3) 
knnFit <- train(patient ~ ., data = jeuEntraine, method = "knn", 
                trControl = cctrl2, preProcess = c("center","scale"), tuneLength = 12)
summary(knnFit)
knnFit
plot(knnFit)
pred8 <- predict(knnFit, newdata = jeuTest[, -1])
table1 <- table(true = jeuTest[[1]], pred = pred8)


knnFit2 <- train(patient ~ ., data = jeuEntraine2, method = "knn", 
                trControl = cctrl2, preProcess = c("center","scale"), tuneLength = 12)
pred2 <- predict(knnFit2, newdata = jeuTest2[, -1])
table2 <- table(true = jeuTest2[[1]], pred = pred2)

knnFit3 <- train(patient ~ ., data = jeuEntraine3, method = "knn", 
                 trControl = cctrl2, preProcess = c("center","scale"), tuneLength = 12)
pred3 <- predict(knnFit3, newdata = jeuTest3[, -1])
table3 <- table(true = jeuTest3[[1]], pred = pred3)

table1
table2
table3
#' 
