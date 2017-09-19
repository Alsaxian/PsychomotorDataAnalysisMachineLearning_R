?prcomp
prusa <- prcomp(USArrests, scale. = TRUE)
X <- apply((USArrests - prusa$center) / prusa$scale * prusa$rotation[,'PC1'], 1, sum)
Y <- apply((USArrests - prusa$center) / prusa$scale * prusa$rotation[,'PC2'], 1, sum)
plot(X, Y)
biplot(prusa, scale=0)
str(prusa)
# USreal <- (USArrests - prusa$center) / prusa$scale
(USArrests - prusa$center) / prusa$scale

myCenter <- apply(USArrests, 2, mean)
myCenter
prusa$center
mySd <- apply(USArrests, 2, sd)
mySd
prusa$scale
install.packages("ggfortify")
library(ggfortify)
autoplot(prusa)
plot(prusa$x[,c("PC1", "PC2")])

prusa$x
str(prusa$x)


diamonds
diamonds %>% count(cut, clarity)
str(objets[2])
names(objets)
count(objets, patient, names(objets)[7])
p <- table(objets[7], objets$patient) %>% plot_ly(x = ~names(objets)[7], y = ~n, color = ~patient)
p

tb <- table(objets[c(2, 7)])
resul <- c("0", "1")
is.matrix(tb)
colnames(tb) <- resul
tb2 <- table(objets[c(7, 2)])
tb2
dt <- data.frame(resul, Patients = tb2[,1], Temoins = tb2[,2])
dt
p <- plot_ly(dt, x = ~resul, y = ~Temoins, type = 'bar', name = 'Temoins', hoverinfo = 'y') %>%
  add_trace(y = ~Patients, name = 'Patients') %>%
  layout(yaxis = list(title = ""), xaxis = list(title = names(objets)[7]), barmode = 'group')
p


install.packages("FactoMineR")
library(FactoMineR)
data(tea)
head(tea)
table(tea$Sport)

library(MASS)
str(Boston)
install.packages("ISLR")
library(ISLR)
str(Smarket)



counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
glm.D93 <- glm(counts ~ outcome + treatment, family=quasipoisson())
glm.D93$resid

#working
resid(glm.D93,type="working")
(counts - glm.D93$fitted.values)/exp(glm.D93$linear)

#deviance
resid(glm.D93,type="dev")
fit <- exp(glm.D93$linear)
poisson.dev <- function (y, mu) 
  sqrt(2 * (y * log(ifelse(y == 0, 1, y/mu)) - (y - mu)))
poisson.dev(counts,fit) * ifelse(counts > fit,1,-1)

#response
resid(glm.D93,type="resp")
counts - fit

#pearson
resid(glm.D93,type="pear")
(counts - fit)/sqrt(fit)


# CA
mat <- matrix(c(1,0,1,1,0,0,0,1,0), nrow = 3)
mat
prop.table(mat, 1)
prop.table(mat, 2)
install.packages("ca")
library(ca)
fitt <- ca(mat)
fitt



## test de l'indépendance marche-t-elle aussi ?
install.packages("exact2x2")
library(exact2x2)

V1 <- 31
nbPatients <- sum(sujets$patient == "patient")
nbTemoins <- sum(sujets$patient == "temoin")
nb0P <- length(which(sujets[, V1] == 0 & sujets$patient == "patient"))
nb0T <- length(which(sujets[, V1] == 0 & sujets$patient == "temoin"))
boschloo(x1 = nb0P, n1 = nbPatients, x2 = nb0T, n2 = nbTemoins, alternative = "less")


## wilcoxon to cota5
Month <- factor(airquality$Month, labels = month.abb[5:9])
pairwise.wilcox.test(airquality$Ozone, Month)
pairwise.wilcox.test(airquality$Ozone, Month, p.adj = "bonf")


head(airquality)


mtcars$am[which(mtcars$am == 0)] <- 'Automatic'
mtcars$am[which(mtcars$am == 1)] <- 'Manual'
mtcars$am <- as.factor(mtcars$am)

library(plotly)
p4 <- plot_ly(mtcars, x = ~wt, y = ~hp, z = ~qsec, color = ~am, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>% add_surface(x = c(0,1), y = c(0,1), z = matrix(c(1,0,0,-1), nrow=2)) %>%
  layout(scene = list(xaxis = list(title = 'Weight'),
                      yaxis = list(title = 'Gross horsepower'),
                      zaxis = list(title = '1/4 mile time')))
save(p4, file = "tryplot.RData")
p4
?add_surface

# Vérifier si certains points sont superposés (en par. un patient avec un témoin)
for (i in 1:(nrow(sujets) - 1)) {
  for(j in (i+1):nrow(sujets)) {
    if (all(sujets[i, ] == sujets[j, ])) {
      print(paste(i, "et", j, "sont identiques", 
                                "le premier étant un", sujets[i, "depression"], "et le dernier un",
                                sujets[j, "depression"]))
    }
  }
} # aucun.

library(Matrix)



install.packages("FactoMineR")
install.packages("factoextra")
install.packages("purrr")
library("FactoMineR")
library("factoextra")
library(purrr)
library(dplyr)
sujetsPTfactor = sujets[, c(2, 7:32)] %>% mutate_each(funs(as.factor))

res.mca <- MCA(sujetsPTfactor,  
    quanti.sup = NULL, quali.sup = 1,  graph=FALSE)
plot(res.mca,  choix ="var")
plot(res.mca)
fviz_mca_var(res.mca, select.var = list(cos2 = 0.4))
fviz_mca_ind(res.mca, label ="none")
fviz_mca_ind(res.mca, label = "none", habillage="patient", addEllipses = TRUE, ellipse.level = 0.95)
names(sujetsPTfactor)
fviz_screeplot(res.mca)

sujetsPTfactor2 = sujets[, c(3, 5, 6, 7:32)] %>% mutate_each(funs(as.factor))
res.mca2 <- MCA(sujetsPTfactor2, 
               quali.sup = 1:3,  graph=FALSE)
fviz_mca_var(res.mca2, title = "biplot ACM") + theme_minimal()
fviz_mca_ind(res.mca2, label ="none")
fviz_mca_ind(res.mca2, label = "none", habillage="depression", addEllipses = TRUE, ellipse.level = 0.95)
fviz_mca_ind(res.mca2, label = "none", habillage="depression", addEllipses = FALSE, ellipse.level = 0.95)

tryF <- function() {
  x <- data.frame(A = c(1, 2), B = c(3, 4))
  y <- 2
  for (i in c(1, 2)){
    y <- i + 3
    x$C <- c(i+3, i+4) 
  }
  print(x)
  print(y)
}
## This shows that for loop works in the outer environment.
appLY <- apply(matrix(1:4, nrow = 2), 2, sum)
