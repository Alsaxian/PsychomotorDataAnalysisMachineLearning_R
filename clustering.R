## install.packages("amap")
install.packages("DiagrammeR")
library(amap)
library(dplyr)
library(plotly)
set.seed(21007979)

traceClusteringACP <- function(fonction, type = "SchizophrenesVsDepressifsSansAutresP", nbClasses = 2,
                               couleurs1 = couleurs[c(7, 6, 4, 2)], jeuDonnees = sujets, titre = "Clustering", 
                               enregistreChemin = "clusteringTemp.RData", ...) {
 
  critere <- as.numeric(levels(jeuDonnees$depression))[jeuDonnees$depression] 
  jdAPatitionner <- switch(type,
    SchizophrenesVsDepressifsSansAutresP = jeuDonnees[critere >= 2, 7:32],
    SchizophrenesVsDepressifsAvecAutresP = jeuDonnees[critere >= 1, 7:32],
    tout = jeuDonnees[7:32],
    stop('Objectif du clustering non-reconnaissable. ')
  )
  
  clus <- fonction(x = jdAPatitionner, centers = nbClasses, ...)
  ## print(clus$cluster)
  
  resultat <- clus$cluster + 1
  

  if (type == "SchizophrenesVsDepressifsAvecAutresP") {
    resultatPart <- resultat[critere >= 2]
  } else if (type == "SchizophrenesVsDepressifsSansAutresP")
      resultatPart <- resultat
  
  correcte <- resultatPart == with(jeuDonnees, depression[critere >= 2])
  nbVrais <- sum(correcte)
  print(nbVrais)
  nbFaux <- sum(critere >= 2) - nbVrais
  print(nbFaux)
  if (nbVrais < nbFaux) {
    clus$cluster <- ifelse (clus$cluster == 1, 2, 1)
    nbFaux <- nbVrais
  }
  print(nbFaux)
  
  couleursClasses <- if(type == "SchizophrenesVsDepressifsAvecAutresP") {
    factor(c(clus$cluster, rep(-1, times = 71)) * (sujets$depression != 1) + 1, ordered = TRUE)
  } else if (type == "SchizophrenesVsDepressifsSansAutresP") {
      prediClasse <- critere
      prediClasse[prediClasse >= 2] <- clus$cluster + 1
      factor(prediClasse, ordered = TRUE)
  }
  
  plot3DClustering <- plot_ly(jeuDonnees, x = ~PC1, y = ~PC2, z = ~PC3, color = couleursClasses,
        colors = couleurs1,
        hoverinfo = 'text', text = ~paste("No. de patient : ", N., "\nN. de oui : ", de.oui))  %>%
    add_markers() %>% layout(scene = list(xaxis = list(title = 'PC1'),
                                          yaxis = list(title = 'PC2'),
                                          zaxis = list(title = 'PC3')),
                             title = paste(titre, type))
  
  save(plot3DClustering, file = enregistreChemin)
  return(list(clus = clus, nbFaux = nbFaux))
}

traceAvecAutres <- traceClusteringACP(kmeans)
traceAvecAutres$nbFaux

traceSansAutres <- traceClusteringACP(kmeans,  type = "SchizophrenesVsDepressifsSansAutresP")

kmclus <- kmeans(x = patients[7:32], 2, nstart = 20)
resulk <- kmclus$cluster + 1
resulkpart <- resulk[as.numeric(patients$depression) > 1.5] 
resulq <- resulkpart == with(patients, depression[as.numeric(depression) > 1.5])
sum(resulq)

mean(with(patients, de.oui[depression == 2])) #  16.57895
mean(with(patients, de.oui[depression == 3])) #  11.48

# kmclus2 <- kmeans(x = patients[as.numeric(patients$depression) >= 2, 7:32], 2, nstart = 20)
## aucune différence de :
kmclus2 <- kmeans(x = patients[as.numeric(patients$depression) >= 2, 7:32], 2, nstart = 30, iter.max = 20)
resulk2 <- kmclus2$cluster + 1
resulq2 <- resulk2 == with(patients, depression[as.numeric(depression) > 1.5])
which(!resulq2)
patients[names(which(resulq2)), 'de.oui']
patients[names(which(!resulq2)), 'de.oui']



Kmclus2 <- Kmeans(x = patients[as.numeric(patients$depression) >= 2, 7:32], 2, nstart = 30, iter.max = 20, method = "binary")
Kmclus2
Kmclus <-  Kmeans(x = patients[7:32], 2, nstart = 30, iter.max = 20, method = "binary")
Kmclus


Km <- Kmeans(x = patients[7:32], 2, nstart = 30, iter.max = 20, method = "manhattan")
resulKm <- Km$cluster + 1
resulkpart <- resulKm[as.numeric(patients$depression) > 1.5] 
resulQ <- resulkpart == with(patients, depression[as.numeric(depression) > 1.5])
sum(resulQ); length(resulQ)
sum(resulKmpart == 3)

Km2 <- Kmeans(x = patients[as.numeric(patients$depression) >= 2, 7:32], 2, nstart = 30, iter.max = 20, method = "binary")
resulKm2 <- Km2$cluster + 1
resulQ2 <- resulKm2 == with(patients, depression[as.numeric(depression) > 1.5])
which(!resulQ2)
sum(resulQ2)

Km3 <- Kmeans(x = patients[as.numeric(patients$depression) >= 2, 7:32], 2, nstart = 20, iter.max = 30, method = "manhattan")
resulKm3 <- Km3$cluster + 1
resulQ3 <- resulKm3 == with(patients, depression[as.numeric(depression) > 1.5])
which(!resulQ3)
sum(resulQ3)

acpSujets <- prcomp(formula = ~ . - N. - patient - depression - Sexe - Age - Bac - de.oui, data = sujets, rank. = 3)
sujets <- cbind(sujets, acpSujets$x)

couleurs <- brewer.pal(n = 11, name = 'RdBu')
plot3DKm <- plot_ly(sujets, x = ~PC1, y = ~PC2, z = ~PC3, color = as.factor(c(kmclus$cluster, rep(-1, times = 71)) * 
                      (sujets$depression != 1) + 1),
                   colors = couleurs[c(7, 6, 2, 4)],
                   hoverinfo = 'text', text = ~paste("No. de patient : ", N., "\nN. de oui : ", de.oui))  %>%
  add_markers() %>% layout(scene = list(xaxis = list(title = 'PC1'),
                                        yaxis = list(title = 'PC2'),
                                        zaxis = list(title = 'PC3')),
                           title = "Clustering k-moyennes Schizophrènes vs. Dépressifs")
save(plot3DKm, file = "plot3DKm.RData")




## EM clustering
# install.packages("EMCluster")
# library(EMCluster)



## clustering hiérarchique
hc.complete <- hclust(dist(patients[patients$depression %in% c("schizophrene", "depressif"), 7:32]), method = "complete")
resulhc <- cutree(hc.complete, 2)
# resulhc <- ifelse (resulhc == 2, 1, 2)
resulhctout <- rep("témoin", times = nrow(sujets))
resulhctout[sujets$depression == "autre"] <- "autre"
resulhctout[sujets$depression %in% c("schizophrene", "depressif")] <- ifelse(resulhc == 2, "diag. dépressif",
                                                                                  "diag. schizophrène")
resulhctout <- factor(resulhctout, levels = c("témoin", "autre", "diag. dépressif", "diag. schizophrène"))
# sum(resulqhc)
plot3Dhc <- plot_ly(sujets, x = ~PC1, y = ~PC2, z = ~PC3, color = resulhctout,
                    colors = couleurs[c(7, 6, 4, 2)],
                    hoverinfo = 'text', text = ~paste("No. de patient : ", N., "\nN. de oui : ", de.oui))  %>%
  add_markers() %>% layout(scene = list(xaxis = list(title = 'PC1'),
                                        yaxis = list(title = 'PC2'),
                                        zaxis = list(title = 'PC3')),
                           title = "Clustering Hiérarchique Schizophrènes contre Dépressifs")
save(plot3Dhc, file = "plot3Dhc.RData")
Pourcomparerhc1 <- ifelse(sujets$depression[sujets$depression %in% c("schizophrene", "depressif")] == "depressif", 1, 2)
Pourcomparerhc2 <- ifelse(sujets$depression[sujets$depression %in% c("schizophrene", "depressif")] == "depressif", 2, 1)
min(sum(Pourcomparerhc1 != resulhc), sum(Pourcomparerhc2 != resulhc))

dd <- as.dist(1-cor(t(patients[7:32])))
hc.completeCor <- hclust(dd, method = "average")
resulhc <- cutree(hc.completeCor, 2)
resulhc <- ifelse (resulhc == 2, 1, 2)
resulhcpart <- resulhc[as.numeric(patients$depression) > 1.5] 
resulqhc <- resulhcpart + 1 == with(patients, depression[as.numeric(depression) > 1.5])
resulqhc
sum(resulqhc)
plot3Dhc <- plot_ly(sujets, x = ~PC1, y = ~PC2, z = ~PC3, color = as.factor(c(resulhc, rep(-1, times = 71)) * 
                                                                              (sujets$depression != 1) + 1),
                    colors = couleurs[c(7, 6, 4, 2)],
                    hoverinfo = 'text', text = ~paste("No. de patient : ", N., "\nN. de oui : ", de.oui))  %>%
  add_markers() %>% layout(scene = list(xaxis = list(title = 'PC1'),
                                        yaxis = list(title = 'PC2'),
                                        zaxis = list(title = 'PC3')),
                           title = "Clustering Hiérarchique Schizophrènes vs. Dépressifs")
save(plot3Dhc, file = "plot3Dhc.RData")
