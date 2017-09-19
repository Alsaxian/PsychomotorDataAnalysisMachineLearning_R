
nomFichier <- "Double cotation échelle 5 pt.xls"
chemin <- "C://Users//ellie//Documents//R//stageCHR//data//"
maxLignes = c(9, 9)
ignoreTete = c(8, 8)
library('readxl')
vraiChemin <- paste(chemin, nomFichier, sep = "")

cotateur1 <- read_excel(vraiChemin, sheet = "t1", 
                        skip = ignoreTete[1], n_max = maxLignes[1])
cotateur2 <- read_excel(vraiChemin, sheet = "t2", 
                        skip = ignoreTete[2], n_max = maxLignes[2])

colnames(cotateur1) <- make.names(colnames(cotateur1), unique = TRUE)
colnames(cotateur2) <- make.names(colnames(cotateur2), unique = TRUE)

save(cotateur1, cotateur2, file = "deuxCotateurs5Niv.RData")

colnames(cotateur1) == colnames(cotateur2)
summary(cotateur1)
str(cotateur1)
dim(cotateur1)
sum(cotateur1[4:27] != cotateur2[4:27])
names(cotateur1)
differences <- as.vector(cotateur1[4:27] - cotateur2[4:27])
differences <- as.matrix(differences)
table(differences)
?wilcox.paired.multcomp
cota1 <- as.vector(as.matrix(cotateur1[4:27]))
cota2 <- as.vector(as.matrix(cotateur2[4:27]))
# dfCotas <- data.frame(cota1 = cota1, cota2 = cota2)
# wilcox.paired.multcomp(cota2|cota1)
wilcox.test(cota1, cota2, paired = TRUE, alternative = "less")
?wilcox.test
length(differences)
bt <- binom.test(46 + (216-46)/2, 216, alternative = "two.sided")
bt <- binom.test(21 + (216-46)/2, 216, alternative = "two.sided")


binomTest <- function(df1 = cotateur1[4:27], df2 = cotateur2[4:27], alpha = 0.05){
  pValueCols <- sapply(1:ncol(df1), function(i){
#    print(i)
#    print(df1[, i])
#    print(df2[, i])
    gagnes <- sum(df1[, i] > df2[, i], na.rm = TRUE)
#    print(gagnes)
    parites <- sum(df1[, i] == df2[, i], na.rm = TRUE)
#    print(parites)
    pertes <- sum(df1[, i] < df2[, i], na.rm = TRUE)
#    print(pertes)
    return(binom.test(min(gagnes, pertes) + floor(parites / 2), 
                      gagnes + parites + pertes, alternative = "two.sided")$p.value)
  })
  
  pValueLignes <- sapply(1:nrow(df1), function(i){
    gagnes <- sum(df1[i, ] > df2[i, ], na.rm = TRUE)
    parites <- sum(df1[i, ] == df2[i, ], na.rm = TRUE)
    pertes <- sum(df1[i, ] < df2[i, ], na.rm = TRUE)
    return(binom.test(min(gagnes, pertes) + floor(parites / 2), 
                      gagnes + parites + pertes, alternative = "two.sided")$p.value)
  })
  
  v1 <- as.vector(as.matrix(df1))
  v2 <- as.vector(as.matrix(df2))
  gagnes <- sum(v1 > v2, na.rm = TRUE)
  parites <- sum(v1 == v2, na.rm = TRUE)
  pertes <- sum(v1 < v2, na.rm = TRUE)
  pValueEnsemble <- binom.test(min(gagnes, pertes) + floor(parites / 2), 
                               gagnes + parites + pertes, alternative = "two.sided")$p.value
  
  return(list(pValueCols, pValueLignes, pValueEnsemble, colIncoherentes = which(pValueCols < alpha),
              ligneIncoherentes = which(pValueLignes < alpha), ensembleIncoherent = pValueEnsemble < alpha))


}
binomTest()
