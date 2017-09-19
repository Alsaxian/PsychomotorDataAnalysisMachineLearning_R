#' ---
#' title: Test sur la différence entre les deux cotateurs
#' author: Xian YANG
#' date: 23 juillet 2017
#' output:
#'    html_document:
#'      toc: true
#'      theme : lumen
#'      highlight: kate
#' ---
#' 
#' 
#' ### Objectif du test
#' Afin de pouvoir transmettre les analyses qu'on avait effectuées avec l'échelle binaire sur l'échelle à 5 niveaux,
#' il convient de vérifier à nouveau l'intégrité de nos données. Ici, on s'occupe de la question, si les résultats
#' de l'examen entre deux cotateurs représentent une différence significative.
#' 
#' ### Jeu de données cible
#' On utilise le jeu de données composé de 9 sujets à 24 critères psychomoteurs. Chaque sujet est coté, pour chaque
#' critère psychomoteur, indépendemment par deux cotatateurs avec chacun une note en nombre entier entre 0 et 4,
#' 0 représentant aucun trouble par ce critère et 4 un trouble majeur. Cette façon de cotation implique que
#' les 24 critères sont toutes des variables ordinales.
#' 
#' ### Objet du test
#' On veut donc vérifier, pour chaque patient puis pour chaque variable, enfin pour l'ensemble de l'examen, si 
#' la cotation faite par les deux cotateurs porte une différence significative.
#' 
#' ### Test statistique choisi
#' Comme les modalités sont ordinales, il faut considérer un test non-paramétrique. A l'égard de la nature de
#' la problématique, le test de Wilcoxon apparié et le test des signes apparié sont deux candidats intéressants.
#' Pour finalement en choisir un, il convient de constater de manière plus précise le jeu de données, en 
#' résumant la liste de différences entre les deux cotations.
#+ warning=FALSE, message=FALSE
load(file = "deuxCotateurs5Niv.RData")
differences <- as.vector(as.matrix(cotateur1[4:27] - cotateur2[4:27]))
tb <- table(differences)
tb
#' On voit que pour la plupart des notes, il n'y a pas de différence entre les deux cotateurs. Pour les notes où
#' il y a une différence, la différence ne saute au plus que d'un cran. En plus, en réalité la chance que la 
#' différence entre les deux cotateurs pour une seule note vaille plus que 2 est très rare. Par conséquent, 
#' On considère que la différence des notes n'a peu de sens en termes de valeur mais qu'en termes de signe,
#' et que le test des signes apparié est ainsi le test le plus pertinent ici.
#' 
#' ### Condition d'application
#' La seule condition d'application pour ce test non-paramétrique est que les observations sont appariés. Elle
#' est bien évidemment vérifiée dans notre cas.
#' 
#' ### Hypothèses et seuil de rejet
#' __Hypothèse nulle :__ les notes ne révèlent pas une différence significative entre les deux cotateurs
#' sur l'objet en question.  
#' __Hypothèse alternative :__ la différence entre les deux cotations et significative.  
#' __Seuil de rejet :__ alpha = 0,05  
#' Le test doit d'ailleurs être bilatéral.
#' 
#' ### Résultats du test
#' Les p-valeurs pour respectivement chaque critère, chaque patient et l'ensemble de l'examen sont présentées 
#' comme suit :
#+ echo=FALSE, warning=FALSE, message=FALSE
binomTest <- function(df1 = cotateur1[4:27], df2 = cotateur2[4:27], alpha = 0.05){
  pValeurCols <- sapply(1:ncol(df1), function(i){
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
  names(pValeurCols) <- paste("Critère", 1:length(pValeurCols))
  
  pValeurLignes <- sapply(1:nrow(df1), function(i){
    gagnes <- sum(df1[i, ] > df2[i, ], na.rm = TRUE)
    parites <- sum(df1[i, ] == df2[i, ], na.rm = TRUE)
    pertes <- sum(df1[i, ] < df2[i, ], na.rm = TRUE)
    return(binom.test(min(gagnes, pertes) + floor(parites / 2), 
                      gagnes + parites + pertes, alternative = "two.sided")$p.value)
  })
  names(pValeurLignes) <- paste("Patient", 1:length(pValeurLignes))
  
  v1 <- as.vector(as.matrix(df1))
  v2 <- as.vector(as.matrix(df2))
  gagnes <- sum(v1 > v2, na.rm = TRUE)
  parites <- sum(v1 == v2, na.rm = TRUE)
  pertes <- sum(v1 < v2, na.rm = TRUE)
  pValeurEnsemble <- binom.test(min(gagnes, pertes) + floor(parites / 2), 
                               gagnes + parites + pertes, alternative = "two.sided")$p.value
  
  return(list(pValeurCols = pValeurCols, pValeurLignes = pValeurLignes, pValeurEnsemble = pValeurEnsemble, 
              colsIncoherentes = which(pValeurCols < alpha),
              lignesIncoherentes = which(pValeurLignes < alpha), ensembleIncoherent = pValeurEnsemble < alpha))

}

binomTest()
#' Les résultats nous conseille de conserver l'hypothèse nulle pour tous nos objets testés (i.e. les critères,
#' les patients et l'ensemble des données).
#' 
#' ### Interprétation des résultats
#' Par ce qu'il n'y a pas de différence significative entre les deux cotateurs on doit comprendre qu'il n'y
#' a pas de différence significative pour chaque objet testé _dans son intégralité_. Ça veut dire que la 
#' différence ou non doit se comprendre dans le sens que pour chaque objet testé, les notes d'un cotateur 
#' ne sont _en moyenne_ pas plus élevées ni plus basses que celles de l'autre. Il n'existait donc pas de tendance.
#' Le test par contre ne peut pas parfaitement exclure par exemple le cas extrême où les cotateurs
#' cotent différemment dans chaque case mais qu'au total ça se compense
#' pour un objet testé. Donc, en étant rassuré par l'absence de différence pour tous les objets en termes de
#' la moyenne, on peut quand même poser la question si la situation décrite plus haut s'est produite, i.e. si
#' le nombre d'occurences de la différence, les deux sens confondus, est élevé. Malheureusement à ce jour il n'y 
#' a pas de test ou critère statistique définitif pour répondre à cette question. Toutfois il contient de 
#' remarquer deux choses :  
#' 1. C'est pour cette raison-là qu'on a effectué le test pour les colonnes (critères) ET pour les lignes 
#' (patients), pour que la probabilité de cette histoire extrême de la compensation reste miniscule.  
#' 2. En revenant au tableau de comtage plus haut, on voit qu'il y a ```r  tb[[1]] + tb[3]``` occurences de
#' la différence sur un total de ```r sum(tb)``` notes, soit ```r (tb[[1]] + tb[3]) / sum(tb)```. On va dire
#' que le nombre d'occurences de la différence est assez petit par rapport au nombre total des notes, tout
#' étant donné que cela reste plutôt subjectif et est à définir par l'utilisateur en fonction des analyses 
#' à effectuer par la suite.