## install.packages('readxl')
## install.packages("tibble")

chargeFichier <- function(nomFichier, maxLignes = c(71, 71), ignoreTete = c(7, 7), 
                          chemin = "C://Users//ellie//Documents//R//stageCHR//data//") {
  library('readxl')
  vraiChemin <- paste(chemin, nomFichier, sep = "")
  
  #### Charger "patients" et "témoins" ####
  patients <- read_excel(vraiChemin, sheet = "p", 
                         skip = ignoreTete[1], n_max = maxLignes[1])
  temoins <- read_excel(vraiChemin, sheet = "t", 
                        skip = ignoreTete[2], n_max = maxLignes[2])
  
  #### Rendre les noms de colonnes légitimes pour R - jeu de données "patients" ####
  colnames(patients) <- make.names(colnames(patients), unique = TRUE)
  names(patients)[5:30] <- 
    c("presenceSensations",
      "imageCorps", "perceptionEmotions", "absenceImpulsivite", "tonusPosture", "equilibreStatique",
      "coordinationGlobale", "coordinationFine", "facialeVolontaire", "reconnaissanceEmotions",
      "tonusMouvement", "absenceRalentissement", "sensationAdaptee", "tonusFond", "imageSoi",
      "schemaCorporelConnu", "schemaCorporelstructure", "abstractionEspace", "organisationSpacialeTemp",
      "praxoGnosie", "regularisationEnergie", "perceptionCorps", "absenceInhibition", "respiratoire",
      "facialeSpontaneeEmo", "toniqueSpontaneeEmo")
  
  #### Rendre les noms de colonnes légitimes pour R - jeu de données "témoins" ####
  colnames(temoins) <- make.names(colnames(temoins), unique = TRUE)
  names(temoins)[5:30] <- 
    c("presenceSensations",
      "imageCorps", "perceptionEmotions", "absenceImpulsivite", "tonusPosture", "equilibreStatique",
      "coordinationGlobale", "coordinationFine", "facialeVolontaire", "reconnaissanceEmotions",
      "tonusMouvement", "absenceRalentissement", "sensationAdaptee", "tonusFond", "imageSoi",
      "schemaCorporelConnu", "schemaCorporelstructure", "abstractionEspace", "organisationSpacialeTemp",
      "praxoGnosie", "regularisationEnergie", "perceptionCorps", "absenceInhibition", "respiratoire",
      "facialeSpontaneeEmo", "toniqueSpontaneeEmo")

  #### Lire les No. des schizophrènes et des dépressifs ####  
  id_schizophrene <- read_excel(vraiChemin, sheet = "schizophrénie", col_names = FALSE)
  id_depressif <- read_excel(vraiChemin, sheet = "dépression", col_names = FALSE)
  
  #### Etablir le jeu de données composé "sujets" ####
  library(tibble)
  patients <- add_column(patients, patient = as.factor("patient"), .before = 2)
  patients <- add_column(patients, depression = factor(
                           ifelse(patients$N. %in% id_schizophrene[[1]], "schizophrene",
                                  ifelse(patients$N. %in% id_depressif[[1]], "depressif", "autre")),
                           levels = c("autre", "depressif", "schizophrene"), ordered = TRUE
                           ), .before = 3)
  
  temoins <- add_column(temoins, patient = as.factor("temoin"), .before = 2)
  temoins <- add_column(temoins, depression = as.factor("temoin"), .before = 3)

  
  sujets <- rbind(patients, temoins)
  sujets$patient <- factor(sujets$patient, levels = c("temoin", "patient"), ordered = TRUE)
  sujets$depression <- factor(sujets$depression, 
                                levels = c("temoin", "autre", "depressif", "schizophrene"), ordered = TRUE)
  
  #### Retourner ####
  return(list(patients=patients, temoins=temoins, sujets=sujets, 
              id_depressif=id_depressif, id_schizophrene=id_schizophrene))
}
  
## nouvellesDonnees <- chargeFichier("Patient Témoin V 2.xls")    
nouvellesDonnees <- chargeFichier("Patient Témoin V 3.xls", maxLignes = c(77, 71))  
patients <- nouvellesDonnees$patients
temoins <- nouvellesDonnees$temoins
sujets <- nouvellesDonnees$sujets 
id_depressif <- nouvellesDonnees$id_depressif
id_schizophrene <- nouvellesDonnees$id_schizophrene
rm(nouvellesDonnees)
  
## Traitement des NA
### sujets
positionsNA <- which(is.na(sujets), arr.ind = TRUE, useNames = TRUE)
# colnames(sujets)[positionsNA[, 2]]
#### Attention ici on attribute une valeur défavorable à chaque NA. D'autres traitements de NA possibles. ####
sujets[positionsNA[1,1], positionsNA[1,2]] <- "B"
sujets[positionsNA[2,1], positionsNA[2,2]] <- 1
### témoins
positionsNA2 <- which(is.na(temoins), arr.ind = TRUE, useNames = TRUE)
# colnames(temoins)[positionsNA[, 2]]
temoins[positionsNA2[1,1], positionsNA2[1,2]] <- "B"
temoins[positionsNA2[2,1], positionsNA2[2,2]] <- 1

rm(positionsNA, positionsNA2)


## ACP
acpSujets <- prcomp(formula = ~ . - N. - patient - depression - Sexe - Age - Bac - de.oui, data = sujets, rank. = 3)
sujets <- cbind(sujets, acpSujets$x)


rm(chargeFichier)
## save.image("jeunesV2.RData")
save.image("jeunesV3.RData")
  
