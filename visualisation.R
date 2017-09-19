#### Analyse par critère 

#### afficher la table de contingence de toutes les variables ####
## lapply(7:32, function(x) table(sujets[, c(2, x)]))

#### afficher le résumé de tous les critères ####
oldPar <- par(mfrow = c(2, 3))
plot_par_critere <- lapply(7:32, function(i) {
  tb <- table(sujets[c(2, i)])
  barplot(tb, # main=,
          xlab=colnames(sujets)[i],
          col=c("darkblue","red"),
          # legend = rownames(counts),
          beside=TRUE)
})
par(oldPar)

#### afficher la heatmap de la matrice de correlation ####
corSujets <- cor(sujets[, 7:32])
corHmp <- plot_ly(x = colnames(sujets)[7:32], y = colnames(sujets)[7:32], z = corSujets, type = "heatmap")
corHmp

#### afficher les couples de variables ayant une correlation >= 0.5 ####
for (i in 7:31){
  for (j in (i+1):32){
    if (corSujets[i-6, j-6] >= 0.5)
      cat (paste(colnames(sujets)[i], " vs. ", colnames(sujets)[j], " : ", corSujets[i-6, j-6]), "\n")
  }
}


#### Affichage en 2D ####
library(ggfortify)
autoplot(acpSujets, data = sujets[, c(2, 7:32)], colour = 'patient')

#### Maintenant en 3D ####
library(plotly)
plot3D <- plot_ly(sujets, x = ~PC1, y = ~PC2, z = ~PC3, color = ~patient, colors = couleurs[c(11,2)],
                  hoverinfo = 'text', text = ~paste("No. de patient : ", N., "\nN. de oui : ", de.oui))  %>%
  add_markers() %>% layout(scene = list(xaxis = list(title = 'PC1'),
                                        yaxis = list(title = 'PC2'),
                                        zaxis = list(title = 'PC3')),
                           title = "Vraie Distribution Patients vs. Témoins")

save(plot3D, file = "plot3D.RData")
## plot3D

#### Maintenant, les dépressifs ####
plot3D2 <- plot_ly(sujets, x = ~PC1, y = ~PC2, z = ~PC3, color = ~depression,
                  colors = couleurs[c(7, 6, 4, 2)],
                  hoverinfo = 'text', text = ~paste("No. de patient : ", N., "\nN. de oui : ", de.oui))  %>%
  add_markers() %>% layout(scene = list(xaxis = list(title = 'PC1'),
                                        yaxis = list(title = 'PC2'),
                                        zaxis = list(title = 'PC3')),
                           title = "Vraie Distribution avec 4 Sous-groupes")

save(plot3D2, file = "plot3D2.RData")
## plot3D2
