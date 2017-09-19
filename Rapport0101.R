#' ---
#' title: Première analyse des données avec quelques tests
#' author: Xian YANG
#' date: 10 juillet 2017
#' output:
#'    html_document:
#'      toc: true
#'      highlight: zenburn
#' ---
#' 
#' ## Quelques notes concernant la préparation des données
#' 
#' ### Jeus de données
#' Comme la semaine dernière j'ai commencé avec le fichier Excel que vous m'aviez donné au début, qui contient 19 
#' dépressifs et 25 schizoprènes, ce premier rapport reste basé sur ce jeu de données. D'après ma brève constatation 
#' des deux autres fichiers que vous m'avez donnés ultérieurement, il s'agit simplement d'un ajout de libellé des 6 
#' autres schizoprènes. L'impact qu'apporte cette différence sur les résultats d'analyse sera donc pour l'instant
#' miniscule. De l'autre côté, la transmission des méthodes statistiques sur le nouveau jeu de données n'est pas 
#' compliqué. Dont si on le souhaite la prochaine fois je continuerai le travail avec le fichier "V3".
#' 
#' ### Valeurs manquantes 
#' Le fichier des patients est complet lorsque pour les témoins, seuls l'obtention du bac du témoin 100 ainsi
#' que le contrôle respiratoire du témoin 167 n'ont pas été saisis. Ni dans le fichier "V3". Pour la crédibilité
#' de l'analyse, j'ai complété le jeu de données en défaveur des témoins, i.e. de façon que le premier n'aie pas 
#' eu son bac et le dernier n'aie pas passé le test de contrôle respiratoire. cela pourra être facilement modifié 
#' une fois qu'on trouve les informations correspondantes.
#' 
#' ### Nom des critères
#' Pour éviter les modifications par erreur dans le fichier Excel original, les noms des 26 critères sont pris
#' tels quels dans le jeu de données de R puis traités par une fonction de R pour être compatibles. 
#' Lorsque cela peut affecter la reconnaissance des noms dans les résultats, ils peuvent cependant être saisis
#' manuellement dans R en cas de besoin.
#' 
#+ echo=FALSE, message=FALSE, results='hide'
set.seed(21007979)
setwd("C:/Users/ellie/Documents/R/stageCHR/")
load("jeunes01.RData")
#### Traitement des NA ####
positionsNA <- which(is.na(objets), arr.ind = TRUE, useNames = TRUE)
colnames(objets)[positionsNA[, 2]]
#### Attention ici on attribute une valeur défavorable à chaque NA. D'autres traitements de NA possibles. ####
objets[positionsNA[1,1], positionsNA[1,2]] <- "B"
objets[positionsNA[2,1], positionsNA[2,2]] <- 1
#'
#' ## Visualisaton de la sensibilité des critères par le test U de Man-Whitney unilatéral
#' 
#' Même quand cela appartient aux méthodes statistiques classiques et plutôt préliminaire dans un problème
#' de classification comme dans notre cas, une analyse et hiérarchisation des variables (i.e. critères) par leur 
#' importance (i.e. séparabilité, sensibilité) est toujours très utile pour avoir une bonne compréhension de
#' nos données. Nous illustrons donc par la suite trois hiérarchisations des variables resp. pour patients vs.
#' témoins, schizoprènes vs. témoins et dépressifs vs. témoins.  
#'   
#' Quant au test utilisé, le test U de Man-Whitney est le plus pertinent dans notre cas, qui est aussi la 
#' version deux groupes du test de Kruskal–Wallis. Sans aucune exigence sur la distribution des données, il
#' consiste à jouer uniquement avec les rangs de celles-ci. Son hypothèse nulle est que "il est également 
#' probable qu'un élément tiré au hasard du premier échantillon soit plus grand ou plus petit qu'un élément 
#' tiré au hasard du second échantillon". Ce test répond exactement aux exigences de ce que nous cherchons 
#' en termes de sensibilité d'un critère : (dans un langage quotidien et non parfaitement correct) 
#' quel est la chance que le critère en question n'est nullement utile pour distinguer un patient d'un témoin, 
#' étant donné la distribution des résultats de ces deux groupe dans ce critère ?  
#'   
#' Les tests d'indépendance (Khi-carré, Fisher exact, maximum vraisemblence...) passent, dans cette perspective,
#' plutôt à côté du but. Les tests d'indépendance cherchent à regarder le ratio des réalisations possibles qui sont
#' encore plus extrèmes que celle qu'on a en main, alors que le test U de Man-Whitney il compte le ratio des
#' "victoires" d'un groupe dans tous les couples possibles composé d'un patient et d'un témoin. Ce n'est pas
#' tout à fait la même chose en général. 
#' Cependant, cela étant dit, seulement dans notre cas avec deux groupes et une cotation à deux niveaux, 
#' ces deux manipulations reviennent par hasard au même. Donc tous ces tests sont en principe équivalents
#' et délivrent à peu près le même résultat. Mais si jamais on passe à une cotation à plus que deux niveaux,
#' seul le test U de Man-Whitney reste à mon avis valide. Et c'est aussi là que je crois que tant qu'on prend 
#' le bon test, l'hiérarchisation des critères par les p-valeurs ne perd pas son sens, même avec une cotation
#' à plusieurs niveaux. Je vous l'expliquerai, si vous voulez, une autre fois.  
#'   
#' Le test est unilatéral dû au simple fait qu'on sait a priori, l'ensemble des témoins
#' passent mieux à chaque test que l'ensemble des dépressifs, ainsi celui des dépressifs que celui 
#' des schizophrènes. 
#' 
#' ### Barplots des critères dans l'ordre décroissant de leur importance, Témoins vs. Patients 
#' marron : _patients_, bleu : _témoins_, 0 : _raté_, 1 : _réussi_.
#+ echo=FALSE, warning=FALSE, message=FALSE
library(dplyr)

testWM <- sapply(7:32, function(i) {
  return(wilcox.test(patients %>% pull(i), temoins %>% pull(i), alternative = "less")$p.value)
}, USE.NAMES = TRUE)
names(testWM) = 7:32
testWMSorted <- sort(testWM)

oldPar <- par(mfrow = c(2, 3))
plot_par_critere_ordered <- lapply(as.integer(names(testWMSorted)), function(i) {
  tb <- table(objets[c(2, i)])
  barplot(tb, # main=,
          xlab=paste(colnames(objets)[i], "\n p : ", signif(testWM[as.character(i)], digits = 3)),
          col=c("brown", "darkblue"),
          # legend = rownames(counts),
          beside=TRUE)
})
par(oldPar)
#' On peut bien y sentir le "sens" d'une grande ou petite p-valeur.
#' 
#' ### Barplots des critères dans l'ordre décroissant de leur importance, Témoins vs. Schizophrènes 
#' rouge : _schizophrènes_, bleu : _témoins_, 0 : _raté_, 1 : _réussi_.
#+ echo=FALSE, warning=FALSE, message=FALSE
testWM2 <- sapply(7:32, function(i) {
  return(wilcox.test(patients[patients$depression == 3, ] %>% pull(i), 
                     temoins %>% pull(i), alternative = "less")$p.value)
}, USE.NAMES = TRUE)
names(testWM2) = 7:32
testWMSorted2 <- sort(testWM2)

oldPar <- par(mfrow = c(2, 3))
plot_par_critere_ordered <- lapply(as.integer(names(testWMSorted2)), function(i) {
  tb <- table(objets[objets$depression %in% c(3, 0), c(2, i)])
  barplot(tb, # main=,
          xlab=paste(colnames(objets)[i], "\n p : ", signif(testWM2[as.character(i)], digits = 3)),
          col=c("red", "darkblue"),
          # legend = rownames(counts),
          beside=TRUE)
})
par(oldPar)
#' 
#' ### Barplots des critères dans l'ordre décroissant de leur importance, Témoins vs. Dépressifs 
#' orange foncé : _dépressifs_, bleu : _témoins_, 0 : _raté_, 1 : _réussi_.
#+ echo=FALSE, warning=FALSE, message=FALSE
testWM3 <- sapply(7:32, function(i) {
  return(wilcox.test(patients[patients$depression == 2, ] %>% pull(i), 
                     temoins %>% pull(i), alternative = "less")$p.value)
}, USE.NAMES = TRUE)
names(testWM3) = 7:32
testWMSorted3 <- sort(testWM3)

oldPar <- par(mfrow = c(2, 3))
plot_par_critere_ordered <- lapply(as.integer(names(testWMSorted3)), function(i) {
  tb <- table(objets[objets$depression %in% c(2, 0), c(2, i)])
  barplot(tb, # main=,
          xlab=paste(colnames(objets)[i], "\n p : ", signif(testWM3[as.character(i)], digits = 3)),
          col=c("darkorange", "darkblue"),
          # legend = rownames(counts),
          beside=TRUE)
})
par(oldPar)
#' On voit donc qu'en gros, bien que dans les trois-cas ci-déssus l'importance des crières n'a pas toujours
#' le même ordre, la différence est miniscule.
#' 
#' 
#' ## L'analyse en composantes principales de l'ensemble des deux groupes
#' Les barplots plus haut démonstre le fait que bien que certains critères soient très utiles pour distinguer
#' les patients des témoins, aucun d'entre eux ne peut tout seul être un critère "propre" (bon sépérateur,
#' classifieur). Il y a toujous des mélanges. Il est alors une bonne idée de regarder la séparabilité 
#' des données dans son intégralité, par p. ex. la méthode d'analyse en composantes principales. On trace
#' tous les points dans les 2 (3) directions les plus étendues.
#' 
#' ### Plot d'ACP en 2D et en 3D, Patients vs. Témoins
#' PC1 : _la 1e composante principale_, PC2 : _la 2nde composante principale_,  rouge : _patients_, bleu : _témoins_.
#+ echo=FALSE, warning=FALSE, message=FALSE
acpObjets <- prcomp(formula = ~ . - N. - patient - depression - Sexe - Age - Bac - de.oui, data = objets, rank. = 3)
library(ggfortify)
autoplot(acpObjets, data = objets[, c(2, 7:32)], colour = 'patient')
#' 
#' On voit bien une tendance que les deux groupes se regroupent entre soi. Ceci est encore plus lisible 
#' lors en 3D.
#+ echo=FALSE, warning=FALSE, message=FALSE
#### Créons quelques couleurs ####
## install.packages("RColorBrewer")
library("RColorBrewer")
## display.brewer.pal(n = 11, name = 'RdBu')
couleurs <- brewer.pal(n = 11, name = 'RdBu')

library(plotly)

objets <- cbind(objets, acpObjets$x)

plot3D <- plot_ly(objets, x = ~PC1, y = ~PC2, z = ~PC3, color = ~patient, colors = couleurs[c(2,11)],
                  hoverinfo = 'text', text = ~paste("No. de patient : ", N., "\nN. de oui : ", de.oui))  %>%
  add_markers() %>% layout(scene = list(xaxis = list(title = 'PC1'),
                                        yaxis = list(title = 'PC2'),
                                        zaxis = list(title = 'PC3')),
                           title = "Vraie Distribution Patients vs. Témoins")
plot3D
#' Visiblement, à quelques exceptions près, on peut bien trouver un classifieur linéaire pour séparer 
#' les témoins et les patients, et ceci déjà dans les 3 dimensions principales. ça veut dire que les 
#' données sont de bonne qualité à ce sujet.  
#' 
#' ### Matrice de correlation
#' Une autre chose qui peut être intéressante est de chercher de la co-linéarité élevée parmi les variables.
#' Une co-linéarité élevée (proche de 1 ou -1) implique une redondance entre deux variables. C'est-à-dire,
#' que le resultat pour deux critères différents est presque toujours le même (ou toujours en opposé), 
#' qu'en effet l'un des deux est là pour rien.  
#' Voici une carte thermique représentant la matrix de correlation de tous les 26 critères.
#+ echo=FALSE, warning=FALSE, message=FALSE
corObjets <- cor(objets[, 7:32])
corHmp <- plot_ly(x = colnames(objets)[7:32], y = colnames(objets)[7:32], z = corObjets, type = "heatmap")
corHmp
#' On voit qu'il n'y a pas de co-linéarité très haute (proche de 1) ni de très basse (proche de -1), i.e. 
#' très jaune ou très foncé, sauf sur la diagonale. 
#' C'est déjà une bonne chose. On veut savoir plus concrètement ou se situent les correlations relativement
#' élevées (ici : >= 0,5) et combien valent elles.
#+ echo=FALSE, warning=FALSE, message=FALSE
for (i in 7:31){
  for (j in (i+1):32){
    if (corObjets[i-6, j-6] >= 0.5)
      cat (paste(colnames(objets)[i], " vs. ", colnames(objets)[j], " : ", corObjets[i-6, j-6]), "\n")
  }
}
#' 
#' ### Garder seulement les 5 critères les plus sensibles ?
#' Ensuite la question qui se pose de façon naturelle est que, si on veut essayer de diminuer le nombre 
#' de critères utilisés, est-ce qu'il suffit de prendre tout simplement les 5 critères les
#' plus sensibles et de jeter tout le reste ?  
#'   
#' Pour répondre à cette question, on peut regarder de nouveaux le plot d'ACP avec seulement ces 5
#' critères.
#+ echo=FALSE, warning=FALSE, message=FALSE
#### Voir la matrice de corrélations des 5 premières variables ####
nbVar <- 5
objetsSymplifiEs <- objets[c(1:6, as.integer(names(testWMSorted))[1:nbVar])]
#### ACP ####
acpObjetsSym <- prcomp(formula = ~ . - N. - patient - depression - Sexe - Age - Bac,
                       data = objetsSymplifiEs, rank. = 3)

objetsSymplifiEs <- cbind(objetsSymplifiEs, acpObjetsSym$x)

plot3DSym <- plot_ly(objetsSymplifiEs, x = ~PC1, y = ~PC2, z = ~PC3, color = ~patient, colors = couleurs[c(2,11)],
                     hoverinfo = 'text', text = ~paste("No. de patient : ", N.))  %>%
  add_markers() %>% layout(scene = list(xaxis = list(title = 'PC1'),
                                        yaxis = list(title = 'PC2'),
                                        zaxis = list(title = 'PC3')))
plot3DSym
#' On voit que la qualité de séparation est discutable dans les trois directions principales.
#' La raison qui peut expliquer ce fait est qu'il y de la co-linéarité dans les 5 critères les plus
#' sensibles, comme le montre la matrice de correlation ci-après. ça veut dire que bien que ces 5 critères
#' soient les plus importants, ils racontent, très grossièrement dit, chacun un peu la même histoire.
#+ echo=FALSE, warning=FALSE, message=FALSE
cor(objetsSymplifiEs[7:11])
#' Donc lorsque la diminuation du nombre de variables est envisagée, une méthode de sélection plus prudente 
#' pourra être considérée.
#' 
#' ### Analyse en composantes principales avec les dépressifs et les schizophrènes
#' Maintenant on illustre les dépressifs et les schizophrènes, toujours dans le même plot d'ACP.  
#'   
#' rouge : _25 schizophrènes_, orange : _19 dépressifs_, blanc : _sans libellés_, bleu : _témoins_
#+ echo=FALSE, warning=FALSE, message=FALSE
plot3D2 <- plot_ly(objets, x = ~PC1, y = ~PC2, z = ~PC3, color = ~depression,
                   colors = couleurs[c(6, 4, 2, 7)],
                   hoverinfo = 'text', text = ~paste("No. de patient : ", N., "\nN. de oui : ", de.oui))  %>%
  add_markers() %>% layout(scene = list(xaxis = list(title = 'PC1'),
                                        yaxis = list(title = 'PC2'),
                                        zaxis = list(title = 'PC3')),
                           title = "Vraie Distribution avec les 4 sous-groupes")
plot3D2
#' Le graphe nous apporte une mauvaise nouvelle : visiblement, les dépressifs et les schizophrènes ne se
#' laissent pas parfaitement différencier, bien qu'en gros, les schizophrènes aient eu une mauvaise note 
#' en moyenne relativement aux dépressifs. On fait par la suite un clustering (en français : partitionnement
#' ou encore classification non-supervisée) sur l'ensemble des patients, mais le mélange des dépressifs
#' et des schizophrènes un peu partout dans le graghe ne nous permet peut-être pas d'attendre à un résultat 
#' de bonne qualité.
#' 
#' ## Clustering (partitionnement) de l'ensemble des patients en deux sous-groupes
#' Si vous me demandez de catégoriser les patients en deux sous-groupes d'après la forme de données
#' AVANT de consulter leurs libellés, c'est bien la méthode non-supervisée qui s'appelle clustering que
#' nous cherchons. C'est en fait une grande famille des méthodes statistiques, dont les deux les plus
#' fameuses sont le "k-moyennes" et le "hiérarchique", que nous illustrerons respectivement.
#' 
#' ### Une supposition : clustering k-moyennes de l'ensemble des patients
#+ echo=FALSE, warning=FALSE, message=FALSE
library(amap)
Km <- Kmeans(x = patients[7:32], 2, iter.max = 20, nstart = 30)
resulKm <- Km$cluster + 1
resulKm <- sapply(resulKm, function(x) if(x == 2) x <- 3 else x <- 2)
resulKmpart <- resulKm[as.numeric(patients$depression) > 1.5]
#' rouge : _`r sum(resulKmpart == 3)` schizophrènes_, orange : _`r sum(resulKmpart == 2)` dépressifs_, 
#' blanc : _sans libellés (pris tels quels, aucune différence)_, bleu : _témoins (pris tels quels, 
#' aucune différence)_
#+ echo=FALSE, warning=FALSE, message=FALSE

plot3DKm <- plot_ly(objets, x = ~PC1, y = ~PC2, z = ~PC3, color = as.factor(c(Km$cluster, rep(-1, times = 71)) * 
                                                                              (objets$depression != 1) + 1),
                    colors = couleurs[c(7, 6, 2, 4)],
                    hoverinfo = 'text', text = ~paste("No. de patient : ", N., "\nN. de oui : ", de.oui))  %>%
  add_markers() %>% layout(scene = list(xaxis = list(title = 'PC1'),
                                        yaxis = list(title = 'PC2'),
                                        zaxis = list(title = 'PC3')),
                           title = "Clustering k-moyennes Schizophrènes vs. Dépressifs")
plot3DKm
#' En comparant avec la vraie distribution des deux sous-groupes on peut obtenir un taux d'erreurs égal à
#' `r sum(resulKmpart != with(patients, depression[as.numeric(depression) > 1.5]))`. Celui-ci est pour la
#' taille de de nos données avec libellé inacceptable. Malgré ce résultat de mauvaise qualité, on constate 
#' que la manière dont cet algorithme a coupé les données est raisonnable.
#' 
#' ### Une supposition : clustering hiérarchique de l'ensemble des patients
#+ echo=FALSE, warning=FALSE, message=FALSE
hc.complete <- hclust(dist(patients[7:32]), method = "complete")
resulhc <- cutree(hc.complete, 2)
resulhcpart <- resulhc[as.numeric(patients$depression) > 1.5]
#' rouge : _`r sum(resulhcpart == 1)` schizophrènes_, orange : _`r sum(resulhcpart == 2)` dépressifs_, 
#' blanc : _sans libellés (pris tels quels, aucune différence)_, bleu : _témoins (pris tels quels, 
#' aucune différence)_
#+ echo=FALSE, warning=FALSE, message=FALSE
plot3Dhc <- plot_ly(objets, x = ~PC1, y = ~PC2, z = ~PC3, color = as.factor(c(resulhc, rep(-1, times = 71)) * 
                                                                              (objets$depression != 1) + 1),
                    colors = couleurs[c(7, 6, 2, 4)],
                    hoverinfo = 'text', text = ~paste("No. de patient : ", N., "\nN. de oui : ", de.oui))  %>%
  add_markers() %>% layout(scene = list(xaxis = list(title = 'PC1'),
                                        yaxis = list(title = 'PC2'),
                                        zaxis = list(title = 'PC3')),
                           title = "Clustering Hiérarchique Schizophrènes vs. Dépressifs")
plot3Dhc
#' En comparant avec la vraie distribution des deux sous-groupes on peut obtenir un taux d'erreurs égal à
#' `r sum(resulhcpart + 1 == with(patients, depression[as.numeric(depression) > 1.5]))`. Lorsque le problème d'un
#' taux d'erreurs trop élevé n'a pas pu être considérablement amélioré qu'avant, on constate tout de même 
#' une telle tendance, que les deux algorithmes catégorisent en générale les points avec un bas taux de réussites 
#' dans la classe schizophrènes, et les points avec un taux de réussites plus élevé dans la classe dépressifs. On 
#' peut par conséquent deviner que la distribution de ces deux sous-groupes en soi ne possèdent peut-être pas 
#' de forme clairement regroupable (trop de mélange l'un dans l'autre, et que ce mélange n'a pas une 
#' forme identifiable).
#' 
#' ### D'autres méthodes de clustering
#' Lorsque ces deux algorithmes de clustering les plus généralement utilisés ont raté de nous délivrer un 
#' résultat satisfaisant, il y a des algorithmes de clustering personnalisés et susceptibles d'être plus
#' pertinents dans notre problème, qui demandent une programmation plus profonde. En attendant de découvrir
#' ceux-là, il convient de remarquer que pour obtenir une solution de meilleure qualité, les méthodes 
#' supervisées peuvent être également considérées.
#' 
#' ### D'autres considérations : essai avec les algorithmes supervisés (régression logistique, SVM, forêts aléatoires, kNN...) ?
#' Ce qui distingue les algorithmes supervisés de ceux non-supervisés, comme le clustering ci-dessus, consiste
#' à regarder dès le début les libellés des éléments et en apprendre un classifieur. Ce dernier peut plus 
#' tard servir à catégoriser de nouveaux éléments sans libellé. Souvent, pour valider la méthode supervisée
#' en question en estimant sa qualité de classification, on coupe les éléments avec un libellé de façon 
#' aléatoire en deux parties, ensuite on en prend l'une pour apprendre le classifieur avant de le vérifier sur
#' l'autre partie en comparant les libellés ainsi obtenus avec les vrais. Par conséquent, ce genre de méthodes
#' demande une quantité de données plus volumieuse, lorsqu'elles délivrent en général un meilleur résultat.
#'  