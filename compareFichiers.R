V3 <- chargeFichier("Patient Témoin V 3.xls", maxLignes = c(77, 71))
V2 <- chargeFichier("Patient Témoin V 3.xls", maxLignes = c(71, 71))  

setdiff(V3$patients$N., V2$patients$N.)
setdiff(V2$patients$N., V3$patients$N.)

setdiff(V3$temoins$N., V2$temoins$N.)
setdiff(V2$temoins$N., V3$temoins$N.)
