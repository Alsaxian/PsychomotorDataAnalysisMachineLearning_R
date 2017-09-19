#' ---
#' title: Taille d'échantillon 5 points
#' author: Xian YANG
#' date: 16 août 2017
#' output:
#'    html_document:
#'      toc: true
#'      theme : lumen
#'      highlight: kate
#' ---
#' 
#' ### Description du jeu de données
#' 
#' Ce jeu de données contient 300 patients et 86 témoins qu'on appellera
#' groupe patients et groupe témoins. Chaque sujet s'est fait noter pour un des 24 critères psychomoteur (qu'on appellera
#' variable) par un nombre entier entre 0 et 4, où 0 signifie
#' aucun trouble et 4 un trouble majeur (un patient est exceptionnellement notée 5 pour le critère 12).   
#'    
#' C'est donc un jeu de données à entrées ordinales. On veut savoir si chaque variable est significative pour les deux 
#' groupes et on veut plus loins les hiérarchiser par leur significativité. Le test pertinent pour ce genre de jeu 
#' de données et le test U de Mann-Whitney (Wilcoxon-Mann-Whitney) Ici on veut étudier, pour chaque variable, la 
#' taille d'échantillon minimum pour que le niveau du test soit de 5% et sa puissance de 90%
#' et on considère les données
#' déjà collectées comme étant une étude préliminaire qui délivre les proportions des notes attendues dans 
#' l'étude officielle
#' (qui est ici la même), auxquels on peut faire confience. On utilise une méthode descrite dans l'article [1].  
#'   
#' ### Méthode de travail
#' Sans mentionner tous les détails mathématiques, l'article propose de procéder, pour chaque variable, comme suit    
#'    
#'   1) Calculer les proportions cumulées de chaque groupe, c'est-à-dire $C_{P0},...,C_{P4},C_{T0},...,C_{T4}$. Puis
#'   les odds ratios \[OR_0 = \frac{C_{P0}/(1-C_{P0})}{C_{T0}/(1-C_{T0})},..., OR_4 = 
#'   \frac{C_{P4}/(1-C_{P4})}{C_{T4}/(1-C_{T4})}.\] Au final on aura besoin d'une seule valeur d'OR.
#'   L'article dit qu'en théorie, les OR ainsi calculés sont supposés égaux,
#'   tout en proposant de regarder pour un certain genre de moyenne si ce n'est pas le cas. Mon observation
#'   est que même si on opte pour la moyenne pondérée, ça sera difficile de décider les proportions duquel groupe
#'   prendre comme poids. Donc j'ai pris toujours la valeur d'OR la plus défavorable (c'est-à-dire la plus petite
#'   quand tous les OR sont supérieurs à 1 et la plus grande quand inférieurs) pour avoir un résultat 100% sûr.   
#'   D'ailleurs, si les deux groupes n'occupent pas les mêmes intervalles d'échelle, e.g. l'ensemble de l'un se fait
#'   noter de 0 à 4 lorsqu'aucune note de 4 se trouve dans l'autre, je propose d'ignorer simplement la proportion isolée
#'   puisque c'est une manipulation défavorable par rapport à ce que je fusionne cette proportion isolée dans celle
#'   précédente, et qui est gagnante en termes de simplicité. Par défavorable on doit comprendre le fait que
#'   cette manipulation délivre en fin du compte un résultat exigeant une taille d'échantillon plus grande. Donc 
#'   si celle taille plus grande est aussi remplie par notre jeu de données, on est plus sûr de la qualité du test.   
#'       
#'   2) A partir de cet OR, on fixe les proportions cumulées du groupe patients comme étant les proportions cumulées
#'   attendues et calcule
#'   les proportions cumulées attendues du groupe témoins, avec la formule $C_{Ti}=C_{Pi}/(C_{Pi} + OR (1-C_{Pi})),
#'   \, i \in 0,...,4$. Puis on remet toutes ces proportions cumulées attendues en proportions simples, et en calcule
#'   les proportions moyennes des deux groupes, i.e. $\bar{P}_i = (P_{Pi} + P_{Ti})/2$.  
#'       
#'   3) Dans le cas des effectifs équilibrés, la taille minimum du chacun des groupes se calcule selon la formule
#'   suivante \[
#'   m = \frac{6 (z_{1-\alpha/2} + z_{1-\beta})^2/(\ln OR)^2}{1 - \sum_{i=0}^k \bar{P}_i^3}.
#'   \]
#'   Où k est normalement 4, peut se réduire à 3 voire 2 si aucun des groupes n'a obtenu de notes en dessus. Ici,
#'   $\alpha$ est le niveau du test, $1 - \beta$ est la puissance et $z$ est la quantile de la loi normale. Le calcule
#'   est fait pour le test U de Mann-Whitney bilatéral. Dans notre cas, un test unilatéral suffit. Mais comme je 
#'   ne suis pas 100% sûr si c'est OK de remplacer dans ce cas $\frac{\alpha}{2}$ par $\alpha$ car j'ai pas vu les
#'   mathématiques concrètes dernière,  je garde $\alpha/2$ tel quel. En tout cas, la taille calculée 
#'   pour un test bilatéral
#'   sera plus que suffisant que pour un test unilatéral. Donc on est tranquille.    
#'       
#'   4. On doit encore faire une correction, quand le nombre de niveaux est inférieur à 5, en multipliant la
#'   taille $m$ par un facteur, qui est égal à $1,125$, $1.067$ où $1.042$ quand le nombre de niveaux est égal 
#'   respectivement à 3, 4, ou 5.   
#'        
#'   5. L'article propose encore la possibilité de calculer une taille d'échantillon pour chacun des deux groupes, 
#'   au cas des effectifs déséquilibrés, dans l'esprit que si l'un des groupes ne remplit pas la condition de
#'   taille, on peut la compenser par une taille plus grande de l'autre groupe. Mais comme d'après les résultats
#'   l'effectif le plus petit des deux groupes, c'est-à-dire celui du groupe témoins, remplit déjà l'exigeance de taille
#'   minimum pour toutes les variables, on n'a par conséquent pas besoin de profiter de cette étape.   
#'   
#' ### Taille minimum par critère
#' La taille minimum s'entend pour chacun des deux groupes.
#+ echo=FALSE, warning=FALSE, message=FALSE
load(file = "cotation5.RData")
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
names(sampleSize) <- names(patients5)[4:27]
sampleSize
#' Comme on a 300 patients et 86 témoins, la condition de la taille d'échantillon minimale est donc remplie pour un 
#' test U de Mann-Whitney uni- ou bilatéral, d'un niveau de 5% et d'une puissance de 90%, pour toutes les variables.
#' 
#' ### Remarque
#' On a adopté à trois endroits un critère plus strict que démandé (unilatéral remplacé par bilatéral, ignoration
#' des proportions isolées, l'adoptation de l'odds ratio le plus défavorable au lieu de leur moyenne). Donc on peut
#' s'attendre à ce qu'en réalité, la taille d'échantillon minimum doit 
#' être encore plus petite. Ce qui assure encore la fiabilité de notre analyse d'ici.
#' 
#' ### Référence
#' [1] M. J. Campbell, S. AJulious et D. G. Altman. Estimating sample sizes for binary, ordered categorical, and
#' continuous outcomes in two group comparisons. _British Medical Journal (28 OCT 1995), VOLUME 311_, 1146-1148.