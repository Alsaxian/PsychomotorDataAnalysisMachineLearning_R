#' ---
#' title: Analyse des données par des méthodes d'apprentissage supervisé et non-supérvisé à travers un jeu de données psychomoteur
#' author: Xian YANG <xian.yang@etu.unistra.fr>
#' date: 07 août 2017
#' output:
#'    pdf_document:
#'      toc: true
#'      highlight: zenburn
#'      df_print: tibble
#' always_allow_html: yes
#' ---
#' 
#' ## Résumé
#' Les jeux de données biomédicaux à taille petit-moyenne avec quelques centaines d’observations 
#' demandent des techniques d’analyse spécifiques. Cet article explore et discute 
#' l’analyse statistique d’un tel jeu de données allant du traitement préliminaire à l'évaluation 
#' des modèles de prédiction en passant par la visualisation des données, applique 
#' un ensemble de méthodes statistiques telles qu'analyse des correspondances multiples, 
#' analyse en composantes principlales, 
#' clustering, régression logistique, ridge, lasso, le filet élastique, SVM, analyse discrimimante, arbres de décisions,
#' le naïf Bayes et KNN, tout en étudiant à la fois leurs théories 
#' mathématiques, leurs effets sur ce genre de jeux de données et leur généralisabilité dans d’autres contextes.
#' 
#' ## 1. Introduction
#' Dans la pratique d’analyse biomédicale, les jeux de données à une jusqu'à quelques centaines 
#' d’observations et quelques dizaines de variables sont les plus affrontés par les praticiens et 
#' les analystes. Cette situation-là se distingue surtout de celle de big data du type $n >> p$, 
#' où plein de méthodes sont disponibles sans avoir le souci de sur-apprentissage et où l’enjeu 
#' est plutôt l’efficacité. Elle se distingue aussi des situations classiques dans la recherche génétique, 
#' où en revanche on a des milliers même des millions de variables contre seulement 
#' quelques centaines d’individu dans l’échantillon. Dans ce dernier cas le médecin 
#' n’a peu de connaissances sur la nature des très nombreuses variables. Ce qu’il peut demander 
#' au statisticien n’est que de donner une compréhension générale sur l’effet de celles-ci et 
#' cette compréhension générale souvent reste descriptive.  
#'   
#' Mais ici, le problème est différent. Lorsque le nombre de variables reste raisonnable et que 
#' le médecin connaît déjà, dans une certaine mesure, l’impact de chacune des variables dans 
#' la problématique, il veut souvent aller plus profond et savoir, avec un nombre d’observations qui, 
#' par rapport au nombre de variables, n’est cependant pas toujours suffisant pour donner un résultat fiable 
#' et robuste, de manière plus précise :  
#'   
#'   1) Comment puis-je mieux comprendre et/ou présenter la relation entre ces variables et la relation entre les différents groupes d'observations ?  
#'   
#'   2) Quelles variables sont significatives pour la maladie ou encore, quelles variables sont significatives pour quelle pathologie ?  
#'   
#'   3) Est-ce que ou comment est-ce que mes données me permettent de prédire si un nouveau sujet est malade ou non, voire sa pathologie la plus probable uniquement à partir de ses résultats de l’examen médical ou des symptômes qu’il manifeste, même si la collecte de données à l’époque n’était pas parfaitement sans biais ?   
#'  
#' Cet article va essayer de répondre à ces trois questions à l’aide d’un jeu de données psychomoteur réel. 
#' Il est organisé comme suit.   
#'   
#' La section 2 s’occupe du traitement préliminaire des données. Dans le paragraphe 2.1 on présente 
#' le jeu de données et effectue un simple traitement de valeurs manquantes. 2.2 étudie la colinéarité 
#' deux à deux parmi les variables explicatives à travers la matrice de variance-covariance. Dans le paragraphe 2.3 
#' il s’agit d’une analyse des correspondances multiples et dans le paragraphe 2.4 une représentation graphique 
#' par l’analyse en composantes principales, suivi du paragraphe 2.5 qui discute les avantages et les désavantages de 
#' ces deux méthodes pour la présentation du jeu de données en questions.  
#'  
#' La sélection de caractéristiques (anglais : feature selection) est un maillon important 
#' pour la compréhension de l’impact qu'ont les variables sur les données. Selon ce que la méthode statistique 
#' est plutôt orientée manuelle ou automatique, on parle souvent des méthodes dites "de filtre" 
#' (anglais : filter), "d’emballage" (anglais : wrapper) ou encore "d’intégration" (anglais : embedding) 
#' (Guyon et Elisseeff, 2004). Ici, cependant, vu que le nombre de variables n’est pas énorme et 
#' qu’elles ont toutes un sens pratique en psychomotricité pour le médecin, il n’est pas pertinent 
#' de faire un test statistique puis de dire au médecin : voilà celle et celle variables sont 
#' statistiquement significatives, le reste est pour la poubelle. Plutôt doit-on effectuer dans 
#' la section 3 une hiérarchisation des variables à l’aide du test U de Mann-Whitney et laisse 
#' l’interprétation du résultat au médecin. On explique également pourquoi on choisit ce test 
#' ainsi que sa signification statistique.  
#'   
#' Avant de commencer l’apprentissage d’un modèle de prédiction, il est souvent utile d’explorer 
#' les pistes des données par les méthodes d’apprentissage non-supervisé. Dans la section 4 on essaie 
#' de répondre, en utilisant 3 différentes variantes de méthodes de clustering, à deux questions : 
#' est-ce que les patients et les témoins se regroupent entre eux de manière naturelle et 
#' est-ce que les schizophrènes et les dépressifs se regroupent entre eux de manière naturelle.  
#'   
#' En vue de prédire si un nouveau sujet est malade ou pas, la section 5 étudie de façon détaillée 
#' l’application des méthodes principales d’apprentissage statistique supervisé. 5.1 introduit 
#' une manière générale de définir un problème d’apprentissage statistique et fixe 
#' les critères d’évaluation du modèle convenant à cet exemple. 5.2 compare la régression logistique 
#' sans terme de pénalité avec Ridge, Lasso et le filet élastique, met en évidence 
#' les significations théoriques et pratique de ces 3 dernières méthodes et la raison pour laquelle 
#' le filet élastique a des avantages en termes de sélection de caractéristiques. 5.3 étudie la 
#' performance du support vecteur machine avec de différents noyaux. 5.4 applique les approches 
#' des arbres de décision et des forêts aléatoires ainsi que l’ada-boosting. 5.5 essaie 
#' l’analyse discriminante linéaire et l’analyse discriminante régularisée. 5.6 utilise la méthode 
#' des K plus proches voisins (KNN). 5.7 présente la méthode naïf Bayes. 5.8 résume ces méthodes 
#' vues auparavant et essaie de les consolider.  
#'   
#' Dans la section 6 on s’occupe de l'autre problème de classification entre les sous-groupes 
#' schizophrènes et dépressifs, en reprenant brièvement les méthodes d’avant qui s’avère toujours valides ici.  
#'   
#' Comme toute analyse de données dans cet article est basée sur le logiciel R, 
#' la section 7 est consacrée au résumé des paquets de R utilisés dans la recherche.
#'   
#' ## 2. Traitement préliminaire et fouille exploratoire des données
#+ echo=FALSE, message=FALSE, results='hide'
set.seed(21007979)
setwd("C:/Users/ellie/Documents/R/stageCHR/")
load("jeunesV3.RData")
#'
#' ### 2.1 Description du jeu de données et traitement des valeurs manquantes
#' Ce jeu de données est issu d’une recherche psychomotrice au Centre Hospitalier de Rouffach, France. 
#' Il contient 77 patients psychiatriques et 71 témoins et les 77 patients appartiennent tous, 
#' selon leur pathologie diagnostiquée,  à une des 3 sous-classes "schizophrène", "dépressif" ou "autre". 
#' Chaque patient ou témoin (appelés ultérieurement sujet) est passé à 26 tests psychomoteurs (ce sont 
#' en effet 26 critères psychomoteurs extraits d’une série de tests complexes. Mais pour la simplification 
#' du langage, on les appelle simplement des tests sans endommager la nature de notre problématique statistique) 
#' et a reçu une note de 1, s’il l’a réussi (c’est bien) et 0 sinon (ce n’est pas bien). Les colonnes 
#' 7 à 32 de ce jeu de données cotées en binaire représentent ces résultats, lorsque la colonne 33 résume le nombre total de 
#' réussites pour chaque sujet. Donc en moyenne on peut s’attendre à ce que les témoins ont réussi 
#' plus de tests que les patients. La première colonne est leur identifiant qui n’a ici pas de sens. 
#' La deuxième colonne est un facteur ordonné à 2 niveaux "patient" et "témoin" lorsque la troisième 
#' est un facteur ordonné à 4 niveaux "schizophrène", "dépressif", "autre" et "témoin", toutes deux enregistrant
#' la classe définitive à laquelle appartient chaque sujet, où la dernière est juste un raffinement de la première. 
#' Les colonnes 4 à 6 reprennent les informations personnelles telles que l’âge, le sexe et l’obtention du bac. 
#' Dans toute analyse qui suit, _on ne va que faire usage de leurs notes dans les tests et 
#' leurs libellés de la classe pathologique et exclure toute autre information personnelle._ 
#' Puisque l’échantillon n’était pas constitué de façon aléatoire : la plupart des témoins est 
#' composée des stagiaires travaillant au Centre Hospitalier. Il n’aura donc simplement aucun sens 
#' d’étudier un classifieur qui prédira qu’un sujet est probablement un témoin parce qu’il a obtenu son bac, 
#' a entre 20 et 25 ans et est une femme. Une partie du jeu de données est présentée comme suit 
#' 
#+ echo=FALSE, message=FALSE, warning=FALSE
library(caret)
library(tree)

rbind(head(patients[, 1:8], n = 3L), head(temoins[, 1:8], n = 3L))
#' D'ailleurs, les psychométriciens qui ont fait la recherche et collecté ces données en ont fait une autre 
#' base de données avec plus de sujets et 24 critères psychomoteurs (i.e. variables). Et là-bas, pour
#' chacun des 24 critères, le sujet a été coté non pas en binaire mais avec une échelle de 0 à 4, 0 représentant
#' aucun trouble et 4 un trouble majeur. On souhaite que dans le futur, les analyses statistiques qu'on
#' fait ici pourront être transmises à cette base de données-là. Dans cette optique, il est très important
#' de remarquer
#' que _dans cet artcle, les méthodes statistiques non seulement valides pour les variables catégoriques, 
#' mais aussi généralisable au traitement des variables ordinales sont toujours préférées._  
#'   
#' Au total il n’y a que deux valeurs manquantes dans le jeu de données. Un témoin n’a pas d'information concernant son bac, un autre témoin n’a pas de saisie pour le test de contrôle respiratoire. L’obtention du bac, comme on l’avait dit plus haut, étant une variable sans doute déséquilibrée qui n'interviendra pas dans l'analyse du à la façon de la constitution de l’échantillon, il ne reste qu’une seule valeur manquante à traiter. Il en résulte que les approches compliquées de traitement de valeurs manquantes ne trouveront pas application ici. Ainsi, il suffira de remplacer cette valeur manquante en contrôle respiratoire par une valeur défavorable à ce témoin, c’est-à-dire un 0, signifiant un échec dans ce test, afin de rendre le résultat d’analyse le plus fiable possible dans ce cas.
#' 
#' ### 2.2 Colinéarité deux-à-deux parmi les variables explicatives
#' Bien que la colinéarité existe souvent parmi plusieurs variables explicatives et ne soit dans ce cas pas présentable en 2 dimensions, la constatation de la matrice de correlations est toujours utile pour détecter en premier temps des colinéarités deux à deux. On présente ici la matrice de correlations des 26 tests : 
#+ echo=FALSE, message=FALSE, warning=FALSE
library(plotly)
corSujets <- cor(sujets[, 7:32])
corHmp <- plot_ly(x = colnames(sujets)[7:32], y = colnames(sujets)[7:32], z = corSujets, type = "heatmap")
corHmp
#' Comme on le voit, il n’y a pas de correlation deux-à-deux très élevée (proche de 1) ou très basse (proche de -1). Par contre il y a quand-même quelques correlations en dessus de 0,5 :
#+ echo=FALSE, warning=FALSE, message=FALSE
for (i in 7:31){
  for (j in (i+1):32){
    if (corSujets[i-6, j-6] >= 0.5)
      cat (paste(colnames(sujets)[i], " et ", colnames(sujets)[j], " : ", corSujets[i-6, j-6]), "\n")
  }
}
#' La correlation peut faire l'objet de la sélection de carastéristiques (anglais : feature selection). On reviendra sur ce point plus tard.
#' 
#' ### 2.3 Analyse des correspondances multiples
#' 
#' L'analyse des correspondances multiples, qui sert souvant à visualiser la similarité à la fois 
#' entre les variables et entre les observations d'un jeu de données complètement catégorique, est un outil
#' efficace pour révéler les pistes des données.  
#+ echo=FALSE, warning=FALSE, message=FALSE
library("FactoMineR")
library("factoextra")
library(purrr)
library(dplyr)
# library(DiagrammeR)
sujetsPTfactor2 = sujets[, c(3, 5, 6, 7:32)] %>% mutate_each(funs(as.factor))
res.mca2 <- MCA(sujetsPTfactor2, 
                quali.sup = 1:3,  graph=FALSE)
fviz_mca_var(res.mca2, select.var = list(cos2 = 0.0), title = "Analyse en Correspondances Multiples") 
#' On peut lire dans le graphique non seulement les similarités entre les variables, mais aussi ce que l'obtention 
#' du bac est déséquilibrée entre les témoins et les patients (A représente sans bac, lorsque B le contraire). 
#' Le graphe suivant montre les régions des
#' 4 sous-groupes pathologiques projetées également sur ces 2 dimensions majeures, supposant qu'elles
#' soient gaussiennes.
#+ echo=FALSE, warning=FALSE, message=FALSE
fviz_mca_ind(res.mca2, label = "none", habillage="depression", addEllipses = TRUE, ellipse.level = 0.95,
             title = "Distributions des Sous-groupes Pathologiques Supposées Gaussiennes par ACM")
#' Cependant, la représentativité des 2 dimensions majeures est discutable, au régard du pourcentage
#' de la variance totale expliqué par celles-ci.
#+ echo=FALSE, warning=FALSE, message=FALSE
fviz_screeplot(res.mca2)
#' 
#' ### 2.4 Représentations graphiques des observations en 3D par analyse des composantes principales
#' L'analyse des composantes principales, une méthode plus largement applicable que l'ACM en termes de
#' visualisation de données, elle cherche d'abord à trouver la dimension qui représente le plus de variance,
#' puis la dimension orthogonale à toutes les anciennes qui représente le plus de variance qui reste, et ainsi
#' de suite. On trace les données avec les 3 premières dimensions par ACP.
#+ echo=FALSE, warning=FALSE, message=FALSE
library("RColorBrewer")
couleurs <- brewer.pal(n = 11, name = 'RdBu')

load("plot3D.RData")
plot3D
load("plot3D2.RData")
plot3D2
#' On voit que lorsque le groupe patients et le groupe témoins se dégagent assez bien, le sous-groupe schizophrènes et
#' le sous-groupe dépressifs produisent malheureusement un grand mélange. A noter toutefois que 
#' les 3 premières dimensions de
#' l'ACP ne représentent que 42,8% de la variance totale, comme dans l'ACM. Donc si ce n'est pas absolument impossible que 
#' le sous-groupe schizophrènes et le sous-groupe dépressifs soient par hasard bien séparables dans une autre
#' dimension mineure qui n'est pas graphiquement présentée dans la figure, on doit normalement s'attrendre à un
#' classifieur de beaucoup meilleure qualité pour patients et témoins que pour schizophrènes et dépressifs, quand
#' on fait de l'apprentissage statistique plus tard. Un tel grand mélange entre les sous-groupes schizophrènes 
#' et dépressifs peut d'ailleurs expliqué par le fait, comme le confirment les médecins, qu'un certain nombre de 
#' patients subissent à la fois les deux pathologies, lorsqu'on est obligé de les catégoriser dans l'un des deux
#' sous-groupes.
#' 
#' ## 3. Hiérarchisation des variables par sensibilité
#' De trouver les variables significatives fait souvent partie importante du traitement d'un problème de classification
#' et est indispensable pour la compréhension intuitive des données. On va faire deux hiérarchisations des variables explicatives
#' par la sensibilité de chacune en termes de qualité de discrimination des groupes, une fois pour patients contre
#' témoins et une fois pour schizophrènes contre dépressifs, suivies d'une comparaison des deux résultats. 
#' On préfère ici éviter le mot "sélection",
#' puisque premièrement les tests sont choisis et pratiqués par les psychomotriciens qui sont donc en quelques sortes
#' tous importants. Deuxièmement, dans un langage de l'apprentissage supervisé qu'on fera ultérieurement, deux 
#' variables individuellement peu importantes pourraient former par hasard un séparateur de qualité.  
#'   
#' La question est, que doit-on comprendre par qualité de discrimination des groupes et quel test choisir ? 
#' On peut considérer le test U de Man-Whitney unilatéral, qui est aussi la version simplifiée
#' du test de Kruskal–Wallis au cas de deux groupes. Sans aucune exigence sur les lois, il
#' consiste à jouer uniquement avec les ordres de chaque couple composé d'un individu du premier groupe et d'un
#' individu du second groupe. Son hypothèse nulle est que "il est également 
#' probable qu'un élément tiré au hasard du premier échantillon soit plus grand ou plus petit qu'un élément 
#' tiré au hasard du second échantillon". Cette affirmation répond donc exactement aux exigences de ce que nous cherchons 
#' en termes de discrimination des groupes par une variable : (dans un langage quotidien) 
#' quel est la chance que le test pyschomoteur en question n'est nullement utile pour distinguer un patient d'un témoin
#' (ou un schizophrène d'un dépressif), 
#' étant données les distributions empiriques des résultats de ces deux groupes dans ce test ?  
#'   
#' Le fonctionnement théorique
#' de ce test est comme suit. On observe tous les couples possibles composés d'un individu du permier groupe et d'un
#' individu du second groupe. Puis on en compte le nombre de victoires (si le membre venu du premier groupe est strictement
#' plus grand ce celui venu du second groupe) pour le premier groupe, où
#' une parité est notée 1/2, ce qu'on appellera la statistique U. Alors sous hypothèse nulle la statistique U suit, 
#' pour un effectif total assez grand, approximativement une loi normale
#' centrée à la moitié du nombre total des couples possibles. Donc une valeur de la statistique U peut délivrer sous 
#' hypothèse nulle une p-valeur, la probabilité avec laquelle une telle réalisation se produise. On fait d'ailleurs le test
#' à l'unilatéral, dû au simple fait qu'on sait a priori, l'ensemble des témoins
#' passent mieux à chaque test psychomoteur que l'ensemble des dépressifs, ainsi que celui des dépressifs que celui 
#' des schizophrènes.   
#'   
#' Dans le cas où les variables et la réponse sont toutes en binaire et donc catégoriques, Les tests d'indépendance 
#' (Khi-carré, Fisher exact, maximum de vraisemblance...) peuvent aussi être un candidat intéressant. 
#' Néanmoins, il faut remarquer 3 choses :  
#'    
#'   1) La nature du problème est ici plutôt une question d'ordre, 
#'   pas une question d'indépendance. En général ce n'est pas que plus la loi marginale d'une variable explicative 
#'   est différente de celle de la réponse, que plus il est facile de dire de quel groupe provient un 
#'   individu.
#'   
#'   2) Le test U de Mann-Whitney a pour avantage que la statistique U divisée par le nombre total des couples
#'   possibles est carrément l'AUC (aire sous la courbe de ROC) de cette variable, qui mesure sa qualité
#'   de distinction.
#'   
#'   3) Rappelons-nous notre envie de généraliser les méthodes et les tests utilisés sur un autre jeu de données 
#'   à une échelle de 5 niveaux. Là-bas, les variables seront ordinales. Le test U de Mann-Whitney sera toujours
#'   valable alors que les tests d'indépendance ne marcheront plus.
#'   
#'   4) Cela étant dit, dans le cas avec deux facteurs à deux modalités comme le nôtre, 
#'   ces deux genres de tests reviennent quasiment
#'   au même. Un test d'indépendance cherche à compter (c'est le bon terme pour le test exact de Fisher. 
#'   Sinon on peut dire
#'   calculer la probabilité en général) les réalisations possibles qui sont encore plus extrêmes que celle qui apparait.
#'   Une réalisation plus extrême dans le sens d'indépendance, est ici grosso modo aussi un cas plus extrême
#'   dans le sens du nombre de victoires dans  les couples, à quelques détails près. Donc ce n'est pas à tort
#'   d'adopter un test d'indépendance pour notre jeu de données.  
#'   
#' Du coup après avoir effectué les hiérarchisations, on peut s'intéresser aux variables qui y ont eu un grand changement
#' de rangs ($\geqslant 5$ par exemples). Il en résulte qu'il y en a 14 parmi toutes les 26. 
#' On peut conclure donc que les variables
#' sensibles pour les deux groupes ne sont pas les mêmes que pour les deux sous-groupes pathologiques.
#' Il convient de remarquer toutefois que la sensibilité d'une variable pour patients et témoins est en général beacoup
#' plus importante que celle pour schizophrènes et dépressifs. Ça veut dire qu'un rang très en arrière dans le premier cas
#' n'implique pas forcément une sensibilité plus faible qu'un rang très en 
#' avant dans le deuxième. 
#+ echo=FALSE, warning=FALSE, message=FALSE
testWM <- sapply(7:32, function(i) {
  return(wilcox.test(patients %>% pull(i), temoins %>% pull(i), alternative = "less")$p.value)
}, USE.NAMES = TRUE)
names(testWM) <- 7:32
testWMSorted <- sort(testWM)


testWM3 <- sapply(7:32, function(i) {
  return(wilcox.test(patients[patients$depression == "schizophrene", ] %>% pull(i), 
                     patients[patients$depression == "depressif", ] %>% pull(i), alternative = "less")$p.value)
}, USE.NAMES = TRUE)
names(testWM3) <- 7:32
testWMSorted3 <- sort(testWM3)
mess <- paste("presenceSensations", "\nSon rang d'importance dans la catégorisation témoins et patients est", 
              which(names(testWMSorted) == which(names(sujets) == "presenceSensations")), 
              "avec une p-valeur", signif(testWMSorted[names(testWMSorted) ==  which(names(sujets) == "presenceSensations")], digits = 3), 
              "\nSon rang d'importance dans la catégorisation dépressifs et schizophrènes est", 
              which(names(testWMSorted3) == which(names(sujets) == "presenceSensations")), 
              "avec une p-valeur", signif(testWMSorted3[names(testWMSorted3) ==  which(names(sujets) == "presenceSensations")], digits = 3))
cat(mess)
#' 
#'
#' ## 4. Etude des similarités des données : clustering
#' Le clustering, une grande famille de méthodes non-supervisées ayant d'allieurs qualques traductions différentes en
#' français dont partitionnement ou simplement classification (c'est là qu'on garde ici le mot anglais pour
#' éviter toute confusion possible), 
#' son but est de détecter des sous-groupes dans les données par leurs similarités en
#' nature et sans consulter leurs libellés. Ses variantes de base les plus courantes sont le clustering en k moyennes,
#' le clustering hiérarchique et le clustering souple par maximisation de l'espérance. Dans cette section, on va 
#' continuer à travailler sur la problématique qu'on a rencontrée lors de la visualisation des sous-groupes et fouiller
#' une partie de nos données constituée _uniquement des schizophrènes et des dépressifs_, pour voir
#' s'il y a quand-même des pistes parmi elles. Les méthodes adoptées seront le clustering en k moyennes et le clustering
#' hiérarchique. 
#' 
#' ### 4.1 Clustering en k moyennes
#' Cet algorithme, nécessitant un nombre de groupes à partitionner préalablement fixé (égal à 2 dans notre cas), 
#' a pour but de trouver un partionnement tel que la moyenne pondérée des variances intra-groupes soit minimale. 
#' Néanmoins, la méthode d'exhaustion ne marche sans doute plus une fois que le nombre d'observations devient un peu grand.
#' L'algorithme adopte une démarche intuitive :   
#'   
#'   Etape 1) On attribue à chaque observation l'un des deux groupes aléatoirement. On doit quand-même faire attention
#'   que'à la fin de cette étape aucun des groupes ne doit être vide ou contenir qu'une seule observation. 
#'   En pratique, on peut attribuer à exactement la moitié des points l'un des deux groupes et à l'autre moitié l'autre
#'   groupe.  
#'   
#'   Etape 2) On répète ce bloc jusqu'à ce que le partionnement ne change plus ou ce que tous les points appartiennent 
#'   à un groupe.  
#'       2.1) Pour chaque groupe ainsi constitué, on calcule son centre.   
#'       2.2) Pour chaque point, on regarde si un changement de groupes diminuera la moyenne pondérée des variances
#'       intra-groupes. Si oui, on lui change de groupes. Sinon on ne fait fait rien et passe au point suivant.   
#'       2.3) Si jamais l'évènement se produisait qu'un groupe contient un seul point, on élimine ce groupe et
#'       donne ce point à l'autre groupe.   
#'           
#' Il convient de remarquer que cet algorithme ne délivre pas de solution globalement optimale. 
#' Lors qu'on peut toujours l'effectuer quelques
#' fois pour avoir éventuellement de meilleures solutions, un partionnement localement optimal peut
#' aussi être intéressant, puisqu'enfin ce qu'on cherche c'est juste des similarités des données qu'on veut chercher, pas
#' forcément _ce genre_ de similarités globalement optimal.   
#'       
#' Un des résultats du clustering en k moyennes pour l'ensembles des schizophrènes et des dépressifs est présenté dans
#' la figure suivante.  
#+ echo=FALSE, warning=FALSE, message=FALSE
load("clusteringTemp.RData")
plot3DClustering
#' ### 4.2 Clustering hiérarchique
#' Le clustering hiérarchique, dont le concept est totalement différent de celui qui précède, propose un angle 
#' plus intuitif à regarder la similarité des données. Il commence par supposer que chaque point soit un groupe
#' tout seul. Puis, de façon récursive il fusionne à chaque fois deux groupes les plus proches, jusquà ce qu'il
#' n'en reste que deux. Donc la question qui reste c'est que comment peut-on définir "deux groupes les plus proches" ?
#' Ou en d'autres termes, que doit être la distance des deux groupes, lorsqu'ils ne contiennent pas chacun un seul point ?
#' Il y a plusieurs façon de le faire. Ici on adopte la distance dite "complète", qui signifie la plus grande
#' distance possible entre deux membres respectivement de chaque groupe. En parlant de la distance, on s'inquiète
#' peut-être de quelle sorte de distance prendre. Ici ce n'est pas la peine, la distance euclidienne et la distance 
#' de Manhattan reviennent au même. Il s'agit de la même façon de mesurer la dissimilarité de deux profiles.  
#'   
#' On présente un des résultats donnés par le clustering hiérarchique.   
#' 
#+ echo=FALSE, warning=FALSE, message=FALSE
load("plot3Dhc.RData")
plot3Dhc
#'
#' En comparer ces deux graghiques de clustering avec celui des schizophrènes et des dépressifs, on voit que bien aucune
#' des deux ne retrouvent la vrai répartition, ce qui n'est pas le but de faire le clustering non plus, les partitonnements
#' des toutes les deux méthodes suivent plus ou moins le nombre de réussites. Qu'ils révèlement des pistes des données
#' vraiment intéressantes ou pas, on peut présenter les résultats aux médecins pour faire une analyse plus profonde
#' avec leurs connaissances du domaine.
#' 
#' 
#' ## 5. Prédiction par méthodes d’apprentissage supervisé, patients et témoins
#' ### 5.1 But et mode de travail
#' Dans cette section, on s’intéresse aux classes patients et témoins et notre but est de trouver un classifieur pertinent
#' par des méthodes d’apprentissage statistique courantes. Pour le faire, on découpe de façon aléatoire 
#' le jeu de données en un jeu d’entraînement, composé de 52 patients et de 46 témoins, et un jeu de test, 
#' composé de 25 patients et de 25 témoins. Puis on entraîne notre classifieur d’abord sur le jeu d’entraînement 
#' et ceci toujours par une validation croisée à 5 ou 10 plis, tant que la méthode le permet 
#' (c’est donc la plupart des cas pour les méthodes choisies dans cet article, l’algorithme du naïf Bayes 
#' qu’on a adopté ici étant la seule exception). Ensuite on évalue la qualité de prédiction du classifieur 
#' sur le jeu de données en calculant 4 taux : exactitude (anglais : accuracy) = (VP + VN) / nombre total, 
#' sensibilité (ou rappel. anglais : sensitivity ou recall) = VP/P, spécificité (anglais : specificity) = VN/N, 
#' précision (anglais : precision) = VP / (VP + FP).   
#'   
#' Dans beaucoup de publications d’algorithmes statistiques, l’évaluation du classifieur s’arrête là. Attention, 
#' ici nous considérons que ceci est insuffisant, pour la simple raison que le découpage en jeux d’entraînement 
#' et de test est purement aléatoire. Ainsi, l’évaluation du classifieur sur ce dernier à une seule fois peut 
#' contenir trop de hasard, sans parler de ce que cette mode de travail laisse beaucoup d’espace à l’auteur 
#' de manipuler son résultat, de manière à choisir exprès un découpage du jeu en faveur de son argument. 
#' Par conséquent, pour éviter ce problème tout en gardant le temps de calcul dans un cadre raisonnable, 
#' on répète l’entier procédé 5 fois et calcule pour les 4 indicateurs leurs moyennes sur le jeu de test de chaque essai. 
#' Une autre façon de voir ça est que ce qu’on effectue est en fait une validation croisée des validations croisées 
#' sur le jeu de données complet. Mais pas comme ces dernières validations croisées qui servent à évaluer 
#' et choisir un paramètre de contrôle spécifique au sein d’un algorithme donné, la première s’intéresse en revanche 
#' à la performance de l’algorithme lui-même (donc à chaque essai un paramètre de contrôle différent). 
#' En raison de justice, les différents découpages en jeux d’entraînement et de test aux 5 essais sont toujours 
#' les mêmes pour chaque algorithme testé, afin de minimiser au mieux le hasard.  
#'    
#' A la fin, chaque méthode est accompagnée d’une courbe ROC avec l’AUC (aire sous la courbe) et le ou les points 
#' optimisant l’exactitude (accuracy) marqué(s). Elle a été faite uniquement avec le jeu de test du premier essai.
#' 
#' 
#' ### 5.2 Régression logistique
#' Les modèles linéaires sont, quand on n’a pas de connaissances profondes sur les données, toujours 
#' au premier rang de nos choix. Dans la régression logistique la plus simple (abrégée OLS pour ordinary least squares), 
#' on code d’habitude les classes à expliquer avec 0 (sain) et 1 (malade). Une fois une observation donnée, 
#' la probabilité conditionnelle que celle-là soit un patient suit une loi de Bernoulli dont la logit du paramètre est 
#' une fonction linéaire de toutes ses variables. En formule 
#' \begin{align} 
#' &\mathrm{Pr}(Y = 1 \mid x = x) \sim \mathrm{Ber}(p(x)) \quad \text{où}\\
#' 
#' & p(x) = \dfrac{1}{1 + e^{-h(x)}} \quad \text{où}\\
#' 
#' & h(x) = \beta^T x + \beta_0 . \\
#' \end{align}
#' Cela nous amène, quand on ajuste le modèle par la méthode du maximum vraisemblance, à résoudre le problème 
#' d’optimisation suivante
#' \[
#' \mathrm{argmin}_{\beta, \beta_0} \left\{ \sum_{i=1}^n \left[ -y_i(\beta^T x_i + 
#' \beta_0) + \ln(1 + e^{\beta^T  x_i + \beta_0})   \right] \right\} . 
#' \]
#' On lira toutefois dans le tableau plus bas, que dans notre cas avec une vingtaine de variables il y a 
#' visiblement un problème de sur-apprentissage avec la régression logistique. La première méthode 
#' de rétrécissement ridge, proposée par Hoerl et Kennard (1970a, 1970b), ajoute un terme de régularisation constituée
#' de la somme des coefficients en norme $L_2$, qui vise à limiter la taille totale de ceux-ci, afin d'éviter la
#' variance trop élevée de la régression linéaire et logistique dans un contexte de haute dimension. 
#' La méthode bridge (Frank and Friedman, 1993) 
#' a généralisé ce terme de régularisation en norme $L_{\gamma}$ avec $\gamma \geqslant 0$ quelconque, 
#' sans néanmoins étudier l’effet de régression des valeurs de $\gamma$ spécifiques.  
#' 
#' Lorsque ridge n’annule jamais les coefficients du modèle ajusté avant que le paramètre de contrôle $\lambda$
#' n’atteint l’infini, Tibshirani (1996) a introduit une autre méthode de rétrécissement lasso, qui possède un effet 
#' de sélection de variables (anglais : sparsity) avec un terme de pénalité en norme $L_1$. 
#' Néanmoins, un des désavantages de lasso c’est que quand deux variables sont hautement corrélées, 
#' lasso va en choisir une seule et en plus n’importe laquelle. D’ailleurs la qualité de prédiction de ridge 
#' semble de dépasser lasso au cas de $n > p$ lorsqu’il y a des corrélations élevées parmi les variables. 
#' Le filet élastique (anglais : elastic net), proposé par Zou et Hastie (2004) a efficacement surmonté ces limites. 
#' Il a surtout pour avantage l’effet de regroupement, i.e. un ensemble de variables hautement corrélées 
#' vont recevoir à la fin des coefficients très proches (ce qu’on ne voit pas directement dans la table de 
#' coefficients du modèle ajusté plus bas car il n’y a pas de corrélations deux à deux hautement élevées 
#' parmi toutes les variables), tout en gardant l’avantage de lasso en termes de sparsity (ce qu’on voit bien 
#' dans la table de coefficients plus bas, du fait que le filet élastique a effectivement sélectionné plus de 
#' variables que lasso, lorsqu’il en a toujours gardé un certain nombre à 0). Le modèle mathématique du 
#' filet élastique est, dans le contexte de classification, comme suit
#' \[
#' (\hat{\beta}, \hat{\beta}_0) = \mathrm{argmin}_{\beta, \beta_0} 
#' \left\{ \sum_{i=1}^n \left[ -y_i(\beta^T x_i + 
#' \beta_0) + \ln(1 + e^{\beta^T  x_i + \beta_0})   \right] + \lambda_1 \lvert \beta \rvert
#' + \lambda_2 \lvert \beta \rvert^2 \right\} ,
#' \]
#' où $\lambda_1$ et $\lambda_2$ sont des paramètres de contrôle à déterminer par p. ex. validation croisée.
#' Quand on pose $\lambda_1 = 0$ le modèle réduit à ridge et quand on pose $\lambda_2 = 0$ il réduit à lasso. Donc
#' le filet élastique est une autre géréralisation de ridge et de lasso.  
#'   
#' Il est remarquable que lorsque la pénalité de ridge est équivalent à une loi gaussienne a priori et que 
#' la pénalité de lasso est équivalent à une loi de Laplace a priori sous l’aspect bayésien, tous deux étant
#' des prieures conjuguées dans le contexte 
#' de régression, le filet élastique ne possède pas ce genre de simple interprétation. Ce défaut n’est cependant pas 
#' si important dans le problème de classification car l’interprétation bayésienne chez ridge et lasso n’y donnera 
#' plus de prieures conjuguées avec la fonction de perte de la régression logistique non plus. 
#' Il est également à remarquer que bien que dans l’article de Zou et Hastie l’effet 
#' de regroupement n’ait été que démontré rigoureusement pour le cas des moindres carrés, en pratique 
#' cette propriété du filet élastique reste toujours valide pour la classification.  
#'   
#' Avec notre jeu de données, on ajuste 5 différents modèles de régression logistique : régression logistique classique,
#' ridge où la valeur optimale de $\lambda$ est choisie par validation croisée, 
#' lasso où la valeur optimale de $\lambda$ est choisie par validation croisée, 
#' mélange - filet élastique où $\lambda_1$ et $\lambda_2$ sont supposés égaux et leur valeur optimale 
#' est choisie par validation croisée, 
#' et filet élastique où $\lambda_1$ et $\lambda_2$ sont librement déterminés 
#' par validation croisée sur le jeu d'entraînement.
#' On peut voir l'effet de sélection de variables chez lasso et le filet élastique aux modèles finaux.
#' Les coefficients sont tous pris sur le jeu de données d'entraînement du premier essai. 
#+ echo=FALSE, warning=FALSE, message=FALSE
load(file = "coeffReg")
coeffReg
#' Puis on peut constater le tableau de performances de ces variantes de la régression logistique.
#' Tous les indicateurs sont respectivement la moyenne des valeurs correspondantes évaluées sur le jeu de test 
#' dans 5 essais répétés indépendamment.
#+ echo=FALSE, warning=FALSE, message=FALSE
load(file = "fractptRDARegSVM.RData")
colnames(accuracyRDARegSVM) <- c("Exactitude", "Sensibilité", "Spécificité", "Précision")
colnames(perfoSchizoDeprTout) <- c("Exactitude", "Sensibilité", "Spécificité", "Précision")
accuracyRDARegSVM[c("Regression.classique", "Regression.ridge", 
                    "Regression.lasso", "Regression.melange", "Regression.elasticNet"), ]
#' On voit que la régression logistique sans pénalité subit effectivement un sur-apprentissage. Ridge est 
#' le vainqueur suivi du filet élastique avec deux pénalités supposées égales. 
#+ echo=FALSE, warning=FALSE, message=FALSE
oldPar <- par(mfrow = c(2, 2))
plot(rocPlotPT$Regression.ridge, print.auc = TRUE, auc.polygon = TRUE, grid = c(0.1, 0.2), 
     grid.col = c("green", "red"),
     max.auc.polygon = TRUE, auc.polygon.col = "skyblue", print.thres = TRUE)
title (main = "Ridge", line = -6, cex.main = 0.85)
plot(rocPlotPT$Regression.lasso, print.auc = TRUE, auc.polygon = TRUE, grid = c(0.1, 0.2), 
     grid.col = c("green", "red"),
     max.auc.polygon = TRUE, auc.polygon.col = "skyblue", print.thres = TRUE)
title (main = "Lasso", line = -6, cex.main = 0.85)
plot(rocPlotPT$Regression.melange, print.auc = TRUE, auc.polygon = TRUE, grid = c(0.1, 0.2), 
     grid.col = c("green", "red"),
     max.auc.polygon = TRUE, auc.polygon.col = "skyblue", print.thres = TRUE)
title (main = "M\u{E9}lange", line = -6, cex.main = 0.85)
plot(rocPlotPT$Regression.ENET, print.auc = TRUE, auc.polygon = TRUE, grid = c(0.1, 0.2), 
     grid.col = c("green", "red"),
     max.auc.polygon = TRUE, auc.polygon.col = "skyblue", print.thres = TRUE)
title (main = "Filet \u{E9}lastique", line = -6, cex.main = 0.85)
par(oldPar)
#' ### 5.3 Machines à vecteurs de support
#' Étant la version la plus simple du SVM (Boser, Guyon, and Vapnik 1992; Cortes and Vapnik 1995), 
#' le classifieur à vecteurs de support (abrégé SVC) ne dispose pas de noyau, ou en d'autres termes son noyau 
#' est juste le produit scalaire au sens habituel. Dans ce paragraphe on suppose que la variable à expliquer 
#' prenne la valeur 1 (si le sujet est malade) ou -1 (s’il est sain). Alors l'approche du SVC, qui consiste à 
#' chercher le meilleur hyperplan séparateur avec une violation la moindre possible des marges, 
#' revient à résoudre le problème d’optimisation suivant
#' \begin{align}
#' &\mathrm{argmin}_{\beta, \beta_0} \left\{\frac{1}{2} \lvert \beta \rvert ^2 + C \sum_{i=1}^N \xi_i \right\}
#' &\text{soujette à} \quad \xi_i \geqslant 0, \, y_i(x_i^T \beta + \beta_0) \geqslant 1 - \xi_i \, \forall i = 1,\dots,N .
#' \end{align}
#' Ici, $C$ signifie l’autorisation totale de la violation des marges par les "vecteurs de support". 
#' Si on désire une séparation parfaite de manière que chaque point soit du bon côté de la marge de sa classe, 
#' $C$ peut être considérer comme étant égale à $\infty$. Mais ceci va souvent entraîner un sur-apprentissage. 
#' En résolvant ce système avec le multiplieur de Lagrange et en suivant l’algorithme d’optimisation minimale 
#' séquentielle, on obtient un classifieur sous la forme
#' \[
#' h(x) = \sum_{i=1}^N \alpha_i y_i \langle x, x_i \rangle + \beta_0
#' \]
#' 
#' où il est à remarquer qu'en effet, seuls les produits scalaires avec les points sur ou du mauvais côté de la marge 
#' sont procédés d'un coefficient non nul. 
#' D’où vient d’ailleurs le nom de la méthode machines à vecteurs de support. En outre, avec un peu d’algèbre 
#' on peut encore reformuler le SVC sous forme de fonction de perte (appelée Hinge) + pénalité en norme $L_2$
#' \[
#' \mathrm{argmin}_{\beta, \beta_0} \left\{ \sum_{i=1}^N \max [0, 1 - y_i (\beta^T x_i + \beta_0)]
#' + \frac{\lambda}{2} \lvert\beta\rvert^2 \right\}
#' \]
#' qui est en effet très proche de la régression ridge. C’est donc la raison pour laquelle le SVC présente 
#' une bonne résistance au problème de sur-apprentissage.  
#'   
#' Lors de la recherche du classifieur optimal du SVC on n’a vu que la solution ne dépendait en effet 
#' que des produits scalaires des points d’observation. De même, pour déterminer la classe d’une nouvelle observation, 
#' on n’a que besoin de savoir ses produits scalaires avec tous les autres points, voire seulement 
#' ses produits scalaires avec les vecteur de support. Ainsi, en posant artificiellement les produits scalaires 
#' raisonnables intra-jeu de données (i.e. la matrice de noyau), on arrivera à lever l’espace de caractéristiques 
#' dans un espace de plus haute dimension, afin d’obtenir un classifieur non linéaire et plus flexible. 
#' Les 3 noyaux les plus souvent utilisés sont les suivants
#' \[
#' \kappa (x, x') = 
#' \begin{cases}
#'     \exp \left( -\gamma \lvert x - x' \rvert^2\right) \quad \text{noyau gaussien} \\
#'     (\gamma x^T x' + c)^d \quad \text{noyau polynomial} \\
#'     \tanh (\gamma x^T x' + c) \quad \text{noyau sigmoïde}.
#'\end{cases}
#' \]
#' Il convient de remarquer que premièrement, dans les premières recherches historiques du SVM, 
#' il était naturellement stipulé, d’après le Théorème de Mercer, que la matrice de noyau doive semi-définie positive. 
#' Néanmoins, le noyau sigmoïde, provenant des réseaux de neurones, ne remplit pas toujours cette condition 
#' avec n’importe quelles valeurs de a et gamma (Vapnik 1995 ; Lin et Lin 2003). Deuxièmement, 
#' le SVM ressemble à la régression par processus gaussien avec la même matrice de noyau fixé avec un terme 
#' de pénalité en norme $L_2$. Leur relation est analogue à celle entre le SVC et la régression logistique de ridge, 
#' comme il est mentionné ci-dessus.  
#'   
#' On peut maintenant comparer la performance de ces 4 différents noyaux du SVM
#+ echo=FALSE, warning=FALSE, message=FALSE
accuracyRDARegSVM[c("SVM.radial", "SVM.polynomial", 
                    "SVM.sigmoid", "SVM.linear", "Regression.ridge"), ]
#' Comme le tableau le montre, le modèle du SVC possède une petite variance grâce à sa simplicité, d’où sa robustesse 
#' et une bonne performance dans notre problème, ce qui est effectivement comparable avec celles de la régression ridge. 
#' Les noyaux gaussien et polynomial en revanche, dû à la flexibilité de leurs modèles qui entraîne un sur-apprentissage, 
#' se trompent relativement fort l’un chez les témoins et l’autre chez les patients, lorsque ce premier délivre 
#' la meilleure sensibilité du groupe quand-même. 
#+ echo=FALSE, warning=FALSE, message=FALSE
oldPar <- par(mfrow = c(2, 2))
plot(rocPlotPT$SVM.radial, print.auc = TRUE, auc.polygon = TRUE, grid = c(0.1, 0.2), 
     grid.col = c("green", "red"),
     max.auc.polygon = TRUE, auc.polygon.col = "skyblue", print.thres = TRUE)
title (main = "Noyau gaussien", line = -6, cex.main = 0.85)
plot(rocPlotPT$SVM.polynomial, print.auc = TRUE, auc.polygon = TRUE, grid = c(0.1, 0.2), 
     grid.col = c("green", "red"),
     max.auc.polygon = TRUE, auc.polygon.col = "skyblue", print.thres = TRUE)
title (main = "Noyau polynomial", line = -6, cex.main = 0.85)
plot(rocPlotPT$SVM.sigmoid, print.auc = TRUE, auc.polygon = TRUE, grid = c(0.1, 0.2), 
     grid.col = c("green", "red"),
     max.auc.polygon = TRUE, auc.polygon.col = "skyblue", print.thres = TRUE)
title (main = "Noyau sigmoide", line = -6, cex.main = 0.85)
plot(rocPlotPT$SVM.linear, print.auc = TRUE, auc.polygon = TRUE, grid = c(0.1, 0.2), 
     grid.col = c("green", "red"),
     max.auc.polygon = TRUE, auc.polygon.col = "skyblue", print.thres = TRUE)
title (main = "SVC", line = -6, cex.main = 0.85)
par(oldPar)
#' ### 5.4 Analyse discriminante linéaire, analyse discriminante quadratique et analyse discriminante régularisée
#' Cette approche classique pouvant remonter à Fisher (1936), son esprit consiste à supposer que les distributions 
#' des observations de chaque classe suivent toutes une loi gaussienne, i.e.
#' \[
#' p_k(x) = \frac{1}{(2\pi)^{\frac{p}{2}} \lvert\Sigma\rvert^{\frac{1}{2}}} 
#' \exp \left\{-\frac{1}{2} (x - \mu_k)^T \Sigma_k^{-1} (x - \mu_k)\right\}
#' \]
#' où $p_k(x)$ représente la fonction de densité de la $k$-ième classe, $\Sigma_k$ sa matrice de 
#' variance-covariance et $\mu_k$ son centre.
#' L’ADL suppose en plus que toutes les classes partagent une covariance commune $\Sigma$, 
#' auquel cas le classifieur devient un hyperplan
#' \[
#' h_k(x) = x^T\Sigma^{-1}\mu_k - \frac{1}{2} \mu_k^T \Sigma^{-1}\mu_k
#' + \ln \pi_k ,
#' \]
#' avec $\pi_k$ la proportion de l'effectif de la classe $k$ par rapport au nombre total d'observations.
#' On peut montrer que dans le cas avec seulement deux classes l’hyperplan classifieur résolu par ADL est 
#' parallèle à celui obtenu par la régression logistique classique. Ils sont voire les mêmes lorsque les effectifs 
#' des deux classes sont équilibrés. Donc c’est pour cela que nous laissons l’ADL hors considération puisque 
#' la régression logistique classique subit déjà du sur-apprentissage, sans parler de l’analyse discriminante 
#' quadratique qui enlève l’hypothèse de la covariance commune intra-groupe et est donc encore plus flexible que 
#' l’ADL. En effet, l’ADQ sans aucune modification ne peut même pas marcher dans notre exemple puisqu’il y a 
#' des dépendance linéaires au sein de la classe témoins, ce qui rend impossible d’inverser 
#' sa propre matrice de variance-covariance dans le calcul de la surface classifieur quadratique.  
#'   
#' Heureusement, Friedman (1989) a proposé l’analyse discriminante régularisée. 
#' Il complète la matrice de variance-covariance combinée par une matrice scalaire, 
#' puis la matrice de variance-covariance intra-groupe par cette matrice combinée modifiée. Soit en formule
# \begin{align}
# & \hat{\Sigma}(1 - \gamma) = \gamma \hat{\Sigma} + (\gamma) \hat{\sigma}^2 \mathrm{I} ,\\
# & \hat{\Sigma}_k(\lambda) = (1 - \lambda) \hat{\Sigma}_k + \lambda \hat{\Sigma}(\gamma) , \\
# \end{align}
#' où $\gamma$ et $\lambda$ sont des paramètres de contrôle entre 0 et 1, à déterminer par validation croisée et
#' $\hat{\sigma}$ est la variance numérique empirique du jeu de données. On voit dans le tableau de performance que,
#' l'analyse discriminante régularisée fonctionne assez bien dans notre exemple. A noter que les paramètres de contrôle
#' $\gamma$ et $\lambda$ dans la fonction ```rda``` du paquet ```klaR``` de R 
#' n'ont pas tous à fait les mêmes définitions qu'ici, malgré les mêmes noms.
#+ echo=FALSE, warning=FALSE, message=FALSE
accuracyRDARegSVM["RDA", ]
#'
#' ### 5.5  Arbres de décision avec post-élagage
#' Le grand avantage des méthodes d'abres décision c'est qu'elles imitent la façon de penser de l'être humain et
#' que les résultats sont ainsi assez faciles à expliquer. Dans notre cas de classification où toutes les variables 
#' sont binaires, cet algorithme glouton fonctionne comme suit. A la première étape on cherche une variable parmi 
#' toutes les 26 pour ensuite découper toutes les observations en deux sous-groupes par leurs valeurs en cette
#' variable, de manière que
#' \[
#' 2 p_1(1 - p_1) + 2 p_2(1 - p_2)
#' \]  
#' soit minimisé, si on adopte l'indice de Gini comme mesure d'impureté, ou
#' \[
#' - p_1\ln p_1 - (1 - p_1) \ln (1 - p_1) - p_2 \ln p_2 - (1 - p_2) \ln (1 - p_2)\, ,
#' \]
#' si on adopte l'entropie croisée, $p_1$ et $p_2$ étant la proportion des patients (ou des témoins) dans respectivement
#' les deux sous-groupes. Une fois la variable optimale trouvée, on garde le découpage ainsi obtenu 
#' (s'appelle ici un noeud) et répète le procédé
#' de façon à chercher un nouveau découpage dans un des deux sous-groupes, qui minimisera l'impureté globale à son tour
#' et ainsi de suite. On procède de façon récursive jusqu'à ce que un critère d'arrêt soit atteint, p. ex. quand 
#' le cardinal de tous les sous-groupes soit inférieur à 5. Finalement on fait représenter chaque petit sous-groupe 
#' (s'appelle ici une feuille) par la classe de majorité de ses membres, puis applique, pour prédire sa classe, 
#' la même itnéraire de découpages binaires à une nouvelle observation et lui attribue la classe représentative de 
#' la feuille dans laquelle elle tombe. 
#' 
#' Un arbre ainsi obtenu est souvent trop large, peut avoir une variance élevée et est susceptible 
#' de subir du sur-apprentissage. Pour éviter ça, Une des possibilités c'est de faire un élagage "du maillon faible"
#' (anglais : weakest link pruning). L'idée c'est de considérer chaque sous-arbre possible de celui-ci et calculer
#' son nombre de classifications érronées (ou taux de classifications érronées, c'est pareil) sur le jeu
#' de validation, pénalisée par un terme $\alpha T$, $T$ étant le nombre total de feuilles
#' du sous-arbre et $\alpha$ le paramètre de contrôle à ajuster par validation croisée. Heureusement, on n'a même
#' pas besoin de considérer tous les sous-arbres : on peut montrer que les sous-arbres otpimaux pour chaque valeur
#' de $\alpha$ sont en fait emboîtés. Donc en faisant parcourir $\alpha$ de 0 à $\infty$, on élague vraiment l'arbre
#' feuille par feuille, jusqu'à sa racine. D'où le nom post-élagage. Le sous-arbre optimal global est donc le
#' sous-arbre optimal par rapport à une certaine valeur de $\alpha$ qui a joué le mieux dans la validation croisée.  
#'    
#' Ici on montre l'arbre élagué obtenu par cet algorithme du premier essai
#+ echo=FALSE, warning=FALSE, message=FALSE
tree.mod <- tree(patient ~ ., jeuEntraine[[1]])
plot(tree.mod)
text(tree.mod, pretty=0)
#' Et le tableau de performances pour l'ensemble des 5 essais
#+ echo=FALSE, warning=FALSE, message=FALSE
accuracyRDARegSVM["tree", ]
#' ### 5.6 Naïf Bayes
#' Cette approche, facile à appliquer au cas discret, elle considère à la fois les différents caractéristiques et
#' l'appartenance à une classe de l'individu comme des variables aléatoires et se base sur l'hypothèse que
#' la loi marginale d'un caractéristique est conditionnellement indépendant de tous les autres, sachant la classe.
#' Reprenons dans notre exemple les notations de classe 1 (malade) et -1 (sain), alors l'hypothèse que fait le naïf Bayes
#' revient à dire
#' \[
#' \Pr(Y = k \mid x = x) = \frac{1}{\Pr(x = x)}
#' \Pr(Y = k) \prod_{i=1}^p \Pr(x_i = x_i \mid Y = k) , \, k = -1, 1
#' \]
#' ce qui entraîne un classifieur en marginalisant la loi globale de $x$
#' \[
#' h(x) = \sum_{i=1}^p \ln\Pr(x_i = x_i \mid Y = 1) - 
#'  \sum_{i=1}^p \ln\Pr(x_i = x_i \mid Y = -1) + \ln\Pr(Y = 1) - \ln\Pr(Y = -1)
#' \]
#' qui est constitué ici juste des comptages et attribue à une nouvelle observation la classe $\mathrm{sgn}h(x)$.
#' 
#' Or, cette hypothèse d'indépendance conditionnelle n'est quasiment jamais vraie. Est-ce que cette 
#' indépendance hypothétique entraîne un résultat
#' qui aura très tort ? Malheureusement, dans le contexte qu'on suppose que la loi marginale que suit chaque 
#' variable sachant la classe est de Bernoulli, comme le semble la nature intrinsèque des choses, oui. On le voit
#' dans la table d'erreurs obtenu au premier essai par ce modèle.
#+ echo=FALSE, warning=FALSE, message=FALSE
tbnB01
#' Le classifieur a refusé de libeller aucun témoin ! La raison pour cela est que les variables sont en fait
#' hautement liées les unes avec les autres, et ceci _uniquement chez les témoins_. Rappelons-nous la densité
#' des témoins par rapport à celle des patients dans la figure d'ACP. Empiriquement parlant, quand les témoins
#' ont très rarement raté un test, le ratio du nombre d'échecs par rapport au 
#' nombre de réussites pour prèsque tous les tests
#' sont très bas chez les témoins. Il y aura alors un grand "prix" à payer de libeller un sujet comme un témoin,
#' lorsqu'il est effectivement sain mais a raté par hasard un test, même juste un seul. En revanche, le taux de 
#' réussites pour les patients est modéré partout. Ça ne coûte pas cher de libeller à tort un témoins.  
#'   
#' Quels recours ? On peut faire recours au naïf Bayes gaussien, qui suppose que la loi marginale de chaque 
#' variable est non pas de Bernoulli, mais cette fois-ci gaussienne. Cette idée semble dans un premier temps 
#' extrêmement ridicule, puisque les variables ne prennent que 0 ou 1 comme valeurs. Néanmoins, l'impact de 
#' cette inexactitude reste mineur à la haute dimension. En plus, ce qui est encore plus important, c'est que
#' quand les distributions d'une variable entre les deux groupes sont différentes mais pas extrêmement
#' différentes, la loi gaussienne peut efficacement modérer le "prix relatif" à payer pour libeller un sujet
#' dans le groupe défavorable aux lois échantionnales de cette variable. Pour illustrer cet effet, constatons
#' les probabilités conditionnelles de la variable ```abstractionEspace``` par la méthode naïf Bayes de Bernoulli
#+ echo=FALSE, warning=FALSE, message=FALSE
nB.fit2$tables$abstractionEspace
#' Si un sujet a raté ce test, il sera `r with(nB.fit2$tables, abstractionEspace[2, 1] / abstractionEspace[1, 1])`
#' fois autant cher de le catégoriser comme un témoin que comme un patient. Par contre quand on emploi la variante
#' gaussienne (la table suivante doit être lue par ligne de façon ```moyenne, variance```), ça donne
#+ echo=FALSE, warning=FALSE, message=FALSE
nB.fit$tables$abstractionEspace
#' Maintenant si un sujet a raté ce test, le prix relatif à payer sera `r with(nB.fit$tables, 
#' dnorm(0, abstractionEspace[2, 1],  sqrt(abstractionEspace[2, 2])) /
#' dnorm(0, abstractionEspace[1, 1],  sqrt(abstractionEspace[1, 2])))`, beaucoup moins qu'avant !
#' L'hypothèse gaussienne du naïf Bayes est dans ce cas en ligne avec la règle d'or des modèles additifs, dont fait 
#' partie la méthode naïf Bayes, ce qui dit d'"apprendre à petits pas".
#+ echo=FALSE, warning=FALSE, message=FALSE
accuracyRDARegSVM["Naif.Bayes", ]
#' ### 5.7 k plus proches voisins
#' La méthode des k plus proches voisins (abrégé KNN) est une méthode complètement non-paramétrique dont 
#' le concept est cependant très simple. Elle est basée sur l'hypothèse que toutes les classes se regroupent 
#' entre elles et qu'il suffit donc de regarder les proportions des classes des $k$ plus proches voisins d'un
#' objet, puis lui attribuer la classe de la majorité. Ainsi, cette méthode délivre souvent
#' une frontière de classification assez irrégulière. Le nombre de voisins $k$ prend d'habitude un nombre impair
#' lorsqu'il y a seulement 2 classes et est déterminé par le taux de classifications érronées sur le jeu
#' de vérifications lors de la validation croisée. La fonction ```train``` du paquet ```caret``` 
#' commence par essayer $k = 5$ 
#' pour la raison qu'elle considère que 1 et 3 
#' sont trop volatiles comme nombre de voisins. 
#+ echo=FALSE, warning=FALSE, message=FALSE
accuracyRDARegSVM["KNN", ]
#' la fonction ```train``` a opté $k = 5$ comme le montre la figure suivante
#+ echo=FALSE, warning=FALSE, message=FALSE
cctrl2 <- trainControl(method="repeatedcv", repeats = 3) 
knnFit <- train(patient ~ ., data = jeuEntraine[[1]], method = "knn", 
                trControl = cctrl2, preProcess = c("center","scale"), tuneLength = 12) 
plot(knnFit)
#' On voit que bien que cette méthode subisse un taux d'erreurs élevé, elle ne se trompe jamais chez les témoins. 
#' Cela peut être expliqué par le fait que les témoins se regroupent de façon beaucoup plus dense que les patients.
#' Du coup les voisins plus proches d'un témoin sont pour la plupart toujours des témoins, alor que ce n'est pas le cas
#' chez les patients, qui sont répartis dans un espace beaucoup plus large, d'après le graphe d'ACP.  
#'   
#' D'ailleurs il convient de remarquer une chose. Le fléau de la dimension présente souvent un obstacle majeur pour
#' KNN. Quand la dimension monte, les points vont devenir rapidement assez isolés et un "voisin" 
#' peut être en réalité très loin en termes de distance euclidienne. 
#' Cependant, notre jeu de données binaire ne subit par hasard point ce problème. Les individus sont en effet répartis
#' aux sommets d'une hypercube de 26 dimensions. Deux points proches en distance euclidienne sont aussi deux points 
#' proches en distance de Manhattan (i.e. quand on passe de métrique de $L_2$ en métrique de $L_1$, l'ordre de
#' proximité ne change pas pour ce genre de jeu de données). Ainsi, de calculer la distance des deux points 
#' revient à regarder le nombre de résultats différents obtenus par les deux sujets q'ils représentent, ce qui revient
#' à dire que les points proches sont effectivement des profiles semblables.
#+ echo=FALSE, warning=FALSE, message=FALSE
oldPar <- par(mfrow = c(2, 2))
plot(rocPlotPT$RDA, print.auc = TRUE, auc.polygon = TRUE, grid = c(0.1, 0.2), 
     grid.col = c("green", "red"),
     max.auc.polygon = TRUE, auc.polygon.col = "skyblue", print.thres = TRUE)
title (main = "Analyse discriminante r\u{E9}gularis\u{E9}e", line = -6, cex.main = 0.85)
plot(rocPlotPT$Regression.logistique, print.auc = TRUE, auc.polygon = TRUE, grid = c(0.1, 0.2), 
     grid.col = c("green", "red"),
     max.auc.polygon = TRUE, auc.polygon.col = "skyblue", print.thres = TRUE)
title (main = "R\u{E9}gression logistique classique", line = -6, cex.main = 0.85)
plot(rocPlotPT$NaifBayes, print.auc = TRUE, auc.polygon = TRUE, grid = c(0.1, 0.2), 
     grid.col = c("green", "red"),
     max.auc.polygon = TRUE, auc.polygon.col = "skyblue", print.thres = TRUE)
title (main = "Naif Bayes Gaussien", line = -6, cex.main = 0.85)
plot(rocPlotPT$KNN, print.auc = TRUE, auc.polygon = TRUE, grid = c(0.1, 0.2), 
     grid.col = c("green", "red"),
     max.auc.polygon = TRUE, auc.polygon.col = "skyblue", print.thres = TRUE)
title (main = "KNN", line = -6, cex.main = 0.85)
par(oldPar)
#'
#' ## 6. Prédiction par méthodes d’apprentissage supervisé, schizophrènes et dépressifs 
#' Dans cette section on s’occupe du problème de classification schizophrènes et dépressifs. 
#' On reprend toutes les méthodes dans la section précédente à l’exception de la régression logistique classique, 
#' avec la même mode de travail sauf que maintenant, on a au total 25 schizophrènes et 25 dépressifs. 
#' Donc lorsqu’on fait toujours 5 essais, à chaque fois la taille de l’échantillon ne permet que de le découper 
#' en un jeu d’entraînement composé de 20 schizophrènes et 20 dépressifs, ainsi qu’un jeu de test composé de 
#' 5 schizophrènes et 5 dépressifs. On laisse tomber la courbe ROC pour cause de la très petite taille du jeu de test. 
#' Voici un aperçu de la performance des méthodes.
#+ echo=FALSE, warning=FALSE, message=FALSE
perfoSchizoDeprTout
#' On voit tout de suite que la tâche est devenue plus difficile pour schizophrènes et dépressifs qu'avant, non seulement 
#' parce qu'on a beacoup moins de données, mais aussi parce qu'il y a visiblement un grand mélange de ces deux sous-groupes 
#' pathologiques, comme on pouvait le constater dans les graphes d'ACP et d'ACM auparavant.  
#'   
#' Toutefois, deux choses méritent notre attention :  
#'   
#'   1) Le filet élastique avec deux pénalités supposées égales a battu tous les autres modèles pour tous les indicateurs. 
#'   Il a fait encore une fois preuve de sa robustesse. Les autres modèles linéaires ont aussi pas mal joué, surtout
#'   ceux du SVC et de lasso. Notre expérience que dans un problème de classification avec un nombre d'observation
#'   relativement petit par rapport au nombre de variable, les modèles linéaires doivent toujours au premier rang 
#'   de nos essais, a été donc renforcée.    
#'     
#'   2) La méthode KNN a eu une relativement bonne performance. Cela peut être dû à l'irrigularité des données. Donc
#'   quand il n'y a pas de frontière régulière visible entre les classes, cett méthode vaut la peine d'essayer.
#'   
#' ## 7. Application numérique avec le logiciel R
#' R est une communauté en pleine croissance qui offre un très grand nombre de paquets pour faciliter l'application
#' numérique des algorithmes statistiques. On en cite quelques uns qui ont été utilisés dans la production des résultats 
#' de cet article.  
#'   
#' _Traitement des données_ :  
#' Le paquet ```readxl``` offre des functions comme ```read_excel```, outils efficaces et pratiques pour 
#' l'importation de haute performance des données depuis un fichier Excel, ainsi que pour le traitement des en-têtes etc.  
#' Le paquet ```tibble```, contenant la nouvelle classe ```tibble``` héritée de la classe ```data.frame```, 
#' permet un traitement plus strict et cohérent des jeu de données.  
#' Le paquet ```dplyr``` permet une écriture classe ```%>%``` méthode, similaire à celle encouragée dans Java et Python.
#' Le codage sera plus efficace une fois on en a l'habitude.  
#'   
#' _Visualisation des données_ :  
#' ```ggplot2``` est sans doute un outil incontournable pour des déssins avancés en 2D.  
#' Le paquet ```plotly```, qui est en même temps un logiciel indépendant ayant également des interfaces pour Python
#' etc., propose plein de possibilités pour créer des dessins intéractifs et des graphes en 3D.  
#' La fonction  ```aggr``` du paquet ```VIM``` permet la visualisation facile des valeurs manquantes lors du traitement
#' préliminaire des données.   
#' Le paquet ```pROC``` propose la fonction ```roc```, ce qui rend la trace du graphe ROC plus simple qu'avec le 
#' paquet ```ROCR```.   
#' Avec le paquet ```RColorBrewer``` on peut automatiser la création d'une série de couleurs cohérentes.  
#'    
#' _ACM, ACP et clustering_ :   
#' Le paquet ```FactoMineR``` offre un grand nombre d'outils pour faire l'analyse factorielle des correspondances et
#' l'analyse des correspondances multiples.  
#' ```factoextra```, basé sur ce dernier, est un paquet intéressant qui facilite la visualisation de l'ACM.  
#' La fonction ```prcomp``` intégré dans le paquet ```stat``` sert à faire l'analyse des composantes principales.  
#' Le clustering en k moyennes peut être effectué avec la fonction ```kmeans``` intégrée dans le paquet ```stat```,    
#' lorsque la fonction ```hclust``` du même paquet s'occupe du clustering hiérarchique.  
#' Le paquet ```amap``` propose une autre fonction ```Kmeans``` pour faire du clustering en k moyennes, avec plus 
#' de sortes de distance. Il ne les explique cependant pas dans sa fiche d'aide.  
#'   
#' _Méthodes supervisées_ :   
#' ```glm``` est la fonction de base pour ajuster un modèle linéaire généralisé, sont petit défaut étant de ne pas 
#' pouvoir faire renvoyer par la fonction ```predict``` directement la réponse en facteur, mais seulement une probabilité.  
#' Une fonction un peu plus avancée c'est ```glmnet``` du paquet du même nom, qui permet d'effectuer en plus ridge,
#' lasso et le filet élastique. Avec ```cv.glmnet``` on peut faire les mêmes choses en validation croisée. Le petit
#' défaut de ces fonctions est qu'elles exigent que la partie des variables explicatives soit une matrice et que les 
#' variables explicatives et la réponse lui soient données impérativement séparément. Ce n'est pas pratique lorsqu'on
#' en crée des applications compliquées avec d'autres fonctions qui acceptent l'écriture formule + jeu de données.  
#'   
#' ```e1071``` est un paquet qui propose entre autres le SVM, le naïf Bayes et des variantes de clustering.  
#' Le paquet ```klaR``` propose une autre fonction pour le naïf Bayes, ainsi que la fonction ```rda``` pour faire
#' l'analyse discriminante régularisée.  
#' La fonction```knn``` du paquet ```class``` ainsi que la fonction ```kNN``` du parquet ```VIM``` permettent de
#' travailler avec la méthode des k plus proches voisins.   
#' Le paquet ```tree``` met à disposition des méthodes d'arbres de décisions.  
#'   
#' En plus de tout ça, il convient de mentionner encore le paquet extraordinaire ```caret```. Il propose avec la fonction
#' ```train``` une fammille très complète des méthodes supervisées avec beaucoup de variantes. Lorsqu'il met en place
#' une interface uniforme pour entraîner n'importe quelle méthode, ce qui s'avère assez efficace, ce paquet demande 
#' cependant une étude profonde afin de maîtriser ses riches fonctionnalités.
#'  
#' ## 8. Remerciements
#' Grand merci à Laurent Treillet, Isabelle Siegrist et Nathalie Anneheim, psychomotriciens au Centre Hospitalier
#' de Rouffach, qui ont supervisé mon stage, m'ont offert beaucoup de connaissances au sujet de la psychométrie,
#' m'ont accueilli très chaleureusement et m'ont donné de l'espace au niveau de l'emploi du temps lorsqu'il me manquait
#' du temps pour la rédaction du rapport dans une si courte durée de stage qui n'a que commencé le 3 juillet 2017.  
#'   
#' Grand merci à Marwen Bouaziz, camarade de classe et stagiaire qui me précède au même projet, pour qu'il m'avait montré
#' l'ensemble de son travail pour que j'aie pu comprendre le sujet du stage plus rapidement.
#' 
#' ## 9. Références
#' R. A. Fisher (1936). The Use of Multiple Measurements in Taxonomic Problems. _Annals of Eugenics 7 (2)_, 179-188.  
#' A. E. Hoerl et R. W. Kennard. Ridge Regression: Applications to Nonorthogonal Problems. _Technometrics, 
#' Vol. 12, No. 1. (Feb., 1970)_, 69-82.    
#' J. H. Friedman. Regularized Discriminant Analysis. _Journal of the American Statistical Association, 
#' Vol. 84, No. 405 (Mar., 1989)_, 165-175.  
#' R. Tibshirani. Regression shrinkage and selection via the lasso. _Journal of the Royal Statistical Society, Series B,
#' (1996) 58, Issue 1_. 267-288.  
#' I. Guyon et A. Elisseeff. An Introduction to Variable and Feature Selection. _Journal of 
#' Machine Learning Research 3 (2003)_, 1157-1182.  
#' H.-T. Lin et C.-J. Lin (2003). A Study on Sigmoid Kernels for SVM and the Training of
#' non-PSD Kernels by SMO-type Methods.  
#' H. Zou et T. Hastie. Regularization and Variable Selection via the Elastic Net. 
#' _Journal of the Royal Statistical Society, Series B (2005) 67_, Part 2, 301-320.  
