nomFichier <- "Grille psychomotrice 5 pts.xls"
chemin <- "C://Users//ellie//Documents//R//stageCHR//data//"
maxLignes = c(308-8, 94-8)
ignoreTete = c(7, 7)
library('readxl')
vraiChemin <- paste(chemin, nomFichier, sep = "")

patients5 <- read_excel(vraiChemin, sheet = "patient", 
                        skip = ignoreTete[1], n_max = maxLignes[1])
temoins5 <- read_excel(vraiChemin, sheet = "témoin", 
                        skip = ignoreTete[2], n_max = maxLignes[2])

colnames(patients5) <- make.names(colnames(patients5), unique = TRUE)
colnames(temoins5) <- make.names(colnames(temoins5), unique = TRUE)


library(VIM)
aggr(patients5, prop = FALSE, numbers = TRUE)
aggr(temoins5, prop = FALSE, numbers = TRUE)

head(patients5)
tail(temoins5[(length(temoins5) - 8) : length(temoins5)])
hist(temoins5$globale)
hist(patients5$globale)

library(mice)
md.pattern(patients5)
md.pattern(temoins5)

### Conclusion : une seule valeur manquante, un patien n'a pas d'âge. Cela n'interviendra pas dans l'analyse, donc
### c'est bon. Chez les témoins, aucun souci.
names(patients5)[4:27] <- 
  c("tonusPostural", "equilibreStatique", "equilibreDynamique", "coordination", "tonusMouvement", 
    "harmonieGEste", "ralentissement", "sensation", "tonusFond", "impulsivite", "schemaCorporel", 
    "imageCorps", "representationSpatiale", "preceptionRythme", "concentration", "praxoGnosie", 
    "regulationEnergie", "perceptionCorps", "inhibition", "instabilite", "respiration", "expressionEmotion",
    "preceptReconEmotions", "gestionEmotions")
names(temoins5)[4:27] <- names(patients5)[4:27]
save(patients5, temoins5, file = "cotation5.RData")


### Application de la formule d'odds ratio
names(patients5)[4] ; names(patients5)[24:28]  ## Donc colonnes en question : 4:27

### Maintenant pour une colonne
mode(table(patients5[[4]])) ## numeric
probaPat <- table(patients5[[4]])/length(patients5[[4]])
probaCumulPat <- cumsum(probaPat)
probaTem <- table(temoins5[[4]])/length(temoins5[[4]])
probaCumulTem <- cumsum(probaTem)
oddsRatio <- probaCumulPat[1:3] / (1- probaCumulPat[1:3]) / (probaCumulTem / (1 - probaCumulTem))
oddsRatio
ExpectedOR <- max(oddsRatio)
probaCumulTemExpected <- probaCumulPat / (probaCumulPat + ExpectedOR * (1 - probaCumulPat))
probaTemExpected <- c(probaCumulTemExpected[1], diff(probaCumulTemExpected))
probaMoyennesExpected <- (probaPat + probaTemExpected)/2
1 - sum(probaMoyennesExpected^3)
6 * (qnorm(0.975) + qnorm(0.90))^2/log(ExpectedOR)^2 / (1 - sum(probaMoyennesExpected^3)) * 1.067 ## needed: 10.36158

#### Now generalize to each colomne
lapply(4:27, function(i) (table(patients5[[i]])))[c(2, 8, 14)]
lapply(4:27, function(i) (table(temoins5[[i]])))

sampleSize <- sapply(4:27, function(i) {
  probaPat <- table(patients5[[i]])/length(patients5[[i]])
  probaCumulPat <- cumsum(probaPat)
  probaTem <- table(temoins5[[i]])/length(temoins5[[i]])
  probaCumulTem <- cumsum(probaTem)
  
  if (length(probaPat) < length(probaTem)) return(Inf) else if (length(probaPat) == length(probaTem)) {
    longueur <- length(probaTem) - 1
  } else longueur <- length(probaTem)
    
    
  oddsRatio <- probaCumulPat[1:longueur] / (1- probaCumulPat[1:longueur]) / 
    (probaCumulTem[1:longueur] / (1 - probaCumulTem[1:longueur]))
  # oddsRatio
  ExpectedOR <- max(oddsRatio)
  probaCumulTemExpected <- probaCumulPat / (probaCumulPat + ExpectedOR * (1 - probaCumulPat))
  probaTemExpected <- c(probaCumulTemExpected[1], diff(probaCumulTemExpected))
  probaMoyennesExpected <- (probaPat + probaTemExpected) / 2
  # 1 - sum(probaMoyennesExpected^3)
  6 * (qnorm(0.975) + qnorm(0.90))^2/log(ExpectedOR)^2 / (1 - sum(probaMoyennesExpected^3)) * 
    switch(length(probaMoyennesExpected), Inf, 1.333, 1.125, 1.067, 1.042, 1)
})
sampleSize

tryCol <- function(i) {
  probaPat <- table(patients5[[i]])/length(patients5[[i]])
  probaCumulPat <- cumsum(probaPat)
  probaTem <- table(temoins5[[i]])/length(temoins5[[i]])
  probaCumulTem <- cumsum(probaTem)
  print(c(probaPat, probaTem))
  if (length(probaPat) < length(probaTem)) return(Inf) else 
    if (length(probaPat) == length(probaTem)) {
      longueur <- length(probaTem) - 1
    } else longueur <- length(probaTem)
  
  
  oddsRatio <- probaCumulPat[1:longueur] / (1- probaCumulPat[1:longueur]) / 
    (probaCumulTem[1:longueur] / (1 - probaCumulTem[1:longueur]))
  print(oddsRatio)
  # oddsRatio
  ExpectedOR <- max(oddsRatio)
  probaCumulTemExpected <- probaCumulPat / (probaCumulPat + ExpectedOR * (1 - probaCumulPat))
  probaTemExpected <- c(probaCumulTemExpected[1], diff(probaCumulTemExpected))
  probaMoyennesExpected <- (probaPat + probaTemExpected) / 2
  print(probaMoyennesExpected)
  # 1 - sum(probaMoyennesExpected^3)
  6 * (qnorm(0.975) + qnorm(0.90))^2/log(ExpectedOR)^2 / (1 - sum(probaMoyennesExpected^3)) * 
    switch(length(probaMoyennesExpected), Inf, 1.333, 1.125, 1.067, 1.042, 1)
}

tryCol(3+14)
tryCol(3+6)
tryCol(3+12)
tryCol(3+21)
