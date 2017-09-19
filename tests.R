library(dplyr)

pWM <- wilcox.test(patients %>% pull(7), temoins %>% pull(7), alternative = "less")
# wilcox.test(patients %>% pull(7), temoins %>% pull(7), alternative = "greater")
wilcox.test(patients %>% pull(7), temoins %>% pull(7), alternative = "two.sided")
# install.packages("Deducer")
library(Deducer)
RP <- sum(patients %>% pull(7))
RT <- sum(temoins %>% pull(7))
likelihood.test(c(RP, 71 - RP), c(RT, 71 - RT))
likelihood.test(matrix(c(RP, 71 - RP, RT, 71 - RT), byrow = TRUE))
install.packages("RVAideMemoire")
library(RVAideMemoire)
G.test(matrix(c(RP, 71 - RP, RT, 71 - RT), nrow = 2, byrow = TRUE))
# wilcox.test(c(RP, 71 - RP, RT, 71 - RT), alternative = "greater")
fisher.test(matrix(c(RP, 71 - RP, RT, 71 - RT), nrow = 2, byrow = TRUE))


str(pWM)
testWM <- sapply(7:32, function(i) {
  return(wilcox.test(patients %>% pull(i), temoins %>% pull(i), alternative = "less")$p.value)
}, USE.NAMES = TRUE)
names(testWM) = 7:32
testWMSorted <- sort(testWM)


#### Trace les barplots en suivant l'ordre décroissant de la sensibilité. ####
oldPar <- par(mfrow = c(2, 3))
plot_par_critere_ordered <- lapply(as.integer(names(testWMSorted)), function(i) {
  tb <- table(objets[c(2, i)])
  barplot(tb, # main=,
          xlab=colnames(objets)[i],
          col=c("darkblue","red"),
          # legend = rownames(counts),
          beside=TRUE)
})
par(oldPar)




#### Comparaison des tests ####
testsCompa <- function(vars = 7:32, side = "less"){
  Uman <- sapply(vars, function(i) {
    return(wilcox.test(patients %>% pull(i), temoins %>% pull(i), alternative = side)$p.value)
  }, USE.NAMES = TRUE)
  
  nbPatients <- sum(sujets$patient == "patient")
  nbTemoins <- sum(sujets$patient == "temoin")
  
  Fisher <- sapply(vars, function(i) {
    nb0P <- nbPatients - sum(patients %>% pull(i))
    nb0T <- nbTemoins - sum(temoins %>% pull(i))
    MX <- matrix(c(nb0T, nbTemoins - nb0T, nb0P, nbPatients - nb0P), nrow = 2, byrow = FALSE)
    return(fisher.exact(MX, alternative = side, conf.int = FALSE)$p.value)
  })
  
  Boschl <- sapply(vars, function(i) {
    nb0P <- nbPatients - sum(patients %>% pull(i))
    nb0T <- nbTemoins - sum(temoins %>% pull(i))
    return(boschloo(x1 = nb0P, n1 = nbPatients, x2 = nb0T, n2 = nbTemoins, alternative = side)$p.value)
  })
  
  return(cbind(Uman, Fisher, Boschl))
}

tC <- testsCompa()
tC

Chi2 <- sapply(vars, function(i) {
  nb0P <- nbPatients - sum(patients %>% pull(i))
  nb0T <- nbTemoins - sum(temoins %>% pull(i))
  MX <- matrix(c(nb0T, nbTemoins - nb0T, nb0P, nbPatients - nb0P), nrow = 2, byrow = FALSE)
  return(chisq.test(MX)$p.value)
})

cb <- cbind(Boschl, Fisher, testWM, Chi2)
#### Voir la matrice de corrélations des 5 premières variables ####
nbVar <- 5
objetsSymplifiEs <- objets[c(1:6, as.integer(names(testWMSorted))[1:nbVar])]


#### ACP ####
acpObjetsSym <- prcomp(formula = ~ . - N. - patient - depression - Sexe - Age - Bac,
                       data = objetsSymplifiEs, rank. = 3)
#### Affichage en 2D ####
## install.packages("ggfortify")
library(ggfortify)
autoplot(acpObjetsSym, data = objetsSymplifiEs[, c(2, 7:11)], colour = 'patient')

#### Maintenant en 3D ####
## install.packages("plotly")
objetsSymplifiEs <- cbind(objetsSymplifiEs, acpObjetsSym$x)

#### Créons quelques couleurs ####
## install.packages("RColorBrewer")
library("RColorBrewer")
## display.brewer.pal(n = 11, name = 'RdBu')
couleurs <- brewer.pal(n = 11, name = 'RdBu')

plot3DSym <- plot_ly(objetsSymplifiEs, x = ~PC1, y = ~PC2, z = ~PC3, color = ~patient, colors = couleurs[c(2,11)],
                  hoverinfo = 'text', text = ~paste("No. de patient : ", N.))  %>%
  add_markers() %>% layout(scene = list(xaxis = list(title = 'PC1'),
                                        yaxis = list(title = 'PC2'),
                                        zaxis = list(title = 'PC3')))
save(plot3DSym, file = "plot3DSym.RData")

#### Conclusion : Choisir les 5 critères les plus sensibles est susceptible d'être insuffisant. ####

cor(objetsSymplifiEs[7:11])
