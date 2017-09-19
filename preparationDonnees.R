## setwd("C:/Users/ellie/Documents/R/stageCHR/")

## load("jeunesV2.RData")
load("jeunesV3.RData")

## install.packages("plotly")
## install.packages("ggfortify")
## install.packages("RColorBrewer")

library(ggplot2)
library(reshape2)
library(dplyr)
library(plotly)
library(ggfortify)
library("RColorBrewer")

set.seed(21007979)






#### Créons quelques couleurs ####
## display.brewer.pal(n = 11, name = 'RdBu')
couleurs <- brewer.pal(n = 11, name = 'RdBu')



#### Séparation en jeu d'entrainement et de test ####
#### Taille du test : Si Patients vs. Temoins, 25:25. Si Schzophrènes vs. Dépressifs, 5:5. ####
fractionneSchizoVsDep <- function(tailleTest = c(5, 5), jeuDonnees = patients) {
  depTest <- sample(x = which(patients$depression == "depressif"), size = tailleTest[2])
  schizoTest <- sample(x = which(patients$depression == "schizophrene"), size = tailleTest[1])
  depEntraine <- setdiff(which(patients$depression == "depressif"), depTest)
  schizoEntraine <- setdiff(which(patients$depression == "schizophrene"), schizoTest)
  return(list(schizoEntraine = schizoEntraine, depEntraine = depEntraine, 
              schizoTest = schizoTest, depTest = depTest))
}

fract <- fractionneSchizoVsDep()
jeuEntraineSD <- patients[c(fract$schizoEntraine, fract$depEntraine), c(3, 7:32)]
jeuTestSD <- patients[c(fract$schizoTest, fract$depTest), c(3, 7:32)]
# head(jeuEntraineSD)


CouleursEntTest <- rep("white", times = nrow(sujets))
CouleursEntTest[fract$schizoEntraine] <- "darkred"
CouleursEntTest[fract$schizoTest] <- "red"
CouleursEntTest[fract$depEntraine] <- "darkorange"
CouleursEntTest[fract$depTest] <- "yellow"
CouleursEntTest <- factor(CouleursEntTest, ordered = TRUE)
plot3DEntTest <- plot_ly(sujets, x = ~PC1, y = ~PC2, z = ~PC3, color = CouleursEntTest, 
                         colors = c("darkorange", "darkred", "red", "white", "yellow"),
                  hoverinfo = 'text', text = ~paste("No. de patient : ", N., "\nN. de oui : ", de.oui))  %>%
  add_markers() %>% layout(scene = list(xaxis = list(title = 'PC1'),
                                        yaxis = list(title = 'PC2'),
                                        zaxis = list(title = 'PC3')),
                           title = "EntrainementTest")

save(plot3DEntTest, file = "plot3DEntTest.RData")



fractionnePatientsVsTemoins <- function(tailleTest = c(25, 25), jeuDonnees = sujets) {
  patTest <- sample(x = which(sujets$patient == "patient"), size = tailleTest[1])
  temTest <- sample(x = which(sujets$patient == "temoin"), size = tailleTest[2])
  patEntraine <- setdiff(which(sujets$patient == "patient"), patTest)
  temEntraine <- setdiff(which(sujets$patient == "temoin"), temTest)
  return(list(patEntraine = patEntraine, temEntraine = temEntraine, 
              patTest = patTest, temTest = temTest))
}

fract2 <- fractionnePatientsVsTemoins()
# sapply(fract2, length)

jeuEntraine <- sujets[c(fract2$patEntraine, fract2$temEntraine), c(2, 7:32)]
jeuTest <- sujets[c(fract2$patTest, fract2$temTest), c(2, 7:32)]
# sapply(fract2, function(x) sujets[x, "patient"])

