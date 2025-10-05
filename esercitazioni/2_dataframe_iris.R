## Esplorare relazioni nel dataframe iris##

# Caricare il dataframe iris #

data(iris)

# Esplorare il dataframe
str(iris)
summary(iris)
head(iris)
tail(iris)
table(iris$Species)

plot(iris)

boxplot(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width, 
        names = c("SL", "SW", "PL", "PW"))

plot(iris$Sepal.Length, col = iris$Species)
plot(iris$Sepal.Width, col = iris$Species)
plot(iris$Petal.Length, col = iris$Species)
plot(iris$Petal.Width, col = iris$Species)

# Calcolare, per ciascun individuo nel dataset, l'area dei Petali e quella dei
# Sepali tramite la formula Area = Lunghezza x Larghezza.
# Aggiungere le due nuove aree ottenute come nuove colonne del dataframe

iris$Sepal.Area <- iris$Sepal.Length * iris$Sepal.Width
iris$Petal.Area <- iris$Petal.Length * iris$Petal.Width

# Mostrare graficamente le distribuzioni delle aree ottenute per le 3 specie
boxplot(Sepal.Area ~ Species, data = iris, main = "Area sepalo per specie")
boxplot(Petal.Area ~ Species, data = iris, main = "Area petalo per specie")

# Posso provare diversi breaks per vedere la distribuzione dei dati
par(mfrow = c(1,1))
hist(iris$Sepal.Area[which(iris$Species == "setosa")], breaks = 10, 
     main = "Area Sepalo setosa")
hist(iris$Sepal.Area[which(iris$Species == "setosa")], breaks = 20, 
     main = "Area Sepalo setosa")
hist(iris$Sepal.Area[which(iris$Species == "setosa")], breaks = 30, 
     main = "Area Sepalo setosa")

# Posso plottare tutte le distribuzioni insieme
par(mfrow = c(2,3))
hist(iris$Sepal.Area[which(iris$Species == "setosa")], breaks = 20, 
     main = "Area Sepalo setosa")
hist(iris$Sepal.Area[which(iris$Species == "versicolor")], breaks = 20, 
     main = "Area Sepalo versicolor")
hist(iris$Sepal.Area[which(iris$Species == "virginica")], breaks = 20, 
     main = "Area Sepalo virginica")

hist(iris$Petal.Area[which(iris$Species == "setosa")], breaks = 20, 
     main = "Area Petalo setosa")
hist(iris$Petal.Area[which(iris$Species == "versicolor")], breaks = 20, 
     main = "Area Petalo versicolor")
hist(iris$Petal.Area[which(iris$Species == "virginica")], breaks = 20, 
     main = "Area Petalo virginica")

par(mfrow = c(1,1))

# Calcolare i valori medi, per ciascuna specie, di tutte le variabili
# (Sepal.Length, Sepal.Width, Petal.Length, Petal.Width e le due aree
# precedentemente calcolate) e rappresentare graficamente i risultati
# (colorare in rosso tutte le barre che si riferiscono a sepali e in blu
# quelle che si riferiscono a petali)

## 1. metodo rapido
means <- lapply(split(subset(iris, select = -Species), iris$Species), colMeans)

## 2. metodo meno rapido
setosa <- iris[which(iris$Species == "setosa"), ]
means_setosa <- colMeans(subset(setosa, select = -Species))

versicolor <- iris[which(iris$Species == "versicolor"), ]
means_versicolor <- colMeans(subset(versicolor, select = -Species))

virginica <- iris[which(iris$Species == "virginica"), ]
means_virginica <- colMeans(subset(virginica, select = -Species))

means <- data.frame(setosa = means_setosa, versicolor = means_versicolor, 
                    virginica = means_virginica)

means

# una volta ottenuti i valori per le medie li plotto
# rinomino le categorie sull'asse x per una migliore visualizzazione
# coloro ogni colonna
barplot(means$setosa, main = "setosa", 
        names.arg = c("SL", "SW", "PL", "PW", "SA", "PA"),
        col = c("red", "red", "blue", "blue","red", "blue"))

barplot(means$versicolor, main = "versicolor", 
        names.arg = c("SL", "SW", "PL", "PW", "SA", "PA"),
        col = c("red", "red", "blue", "blue","red", "blue"))

barplot(means$virginica, main = "virginica",
        names.arg = c("SL", "SW", "PL", "PW", "SA", "PA"),
        col = c("red", "red", "blue", "blue","red", "blue"))


# Per una specie a scelta fare un istogramma dei valori di area dei sepali, e su ognuno 
# plottare una retta verticale per media, mediana, primo e terzo quartile, colorandole
# diversamente
hist(setosa$Sepal.Area)
# media
abline(v = mean(setosa$Sepal.Area), col = "red", lwd = 2)

q <- quantile(setosa$Sepal.Area)
q
# mediana
abline(v = q[3], col = "blue", lwd = 2)
abline(v = q[2], col = "green", lwd = 2)
abline(v = q[4], col = "green", lwd = 2)

# (dopo la lezione del 1/04)
# Studiare la relazione esistente tra le due aree:
# è lineare?
# è significativa?
# è possibile trovare relazioni tra le aree ed altre variabili?
# provare a fare una predizione con nuovi valori

plot(iris, cex = 0.5)

plot(iris$Petal.Area, iris$Sepal.Area, pch = 19, cex = 0.8)
plot(iris$Sepal.Area, iris$Petal.Area, pch = 19, cex = 0.8)

# modello lineare
lm_areas <- lm(Petal.Area ~ Sepal.Area, data = iris )
summary(lm_areas)
abline(lm_areas, col = "red", lwd = 2, lty = 1)
summary(lm_areas)$adj.r.squared

# valutazione residui
boxplot(lm_areas$residuals)
max(lm_areas$residuals)
min(lm_areas$residuals)

hist(lm_areas$residuals, breaks = 10)
abline(v = mean(lm_areas$residuals), col = "red", lty = 2, lwd = 1.5)
abline(v = median(lm_areas$residuals), col = "blue", lty = 2, lwd = 1.5)

plot(lm_areas)
plot(lm_areas, which = 1)
plot(lm_areas, which = 2)

summary(lm_areas)

# uso i pacchetti visreg ed effects
library(visreg)
library(effects)

visreg(lm_areas)
plot(effect("Sepal.Area", lm_areas))

# nuova predizione
boxplot(iris$Sepal.Area)
new_sepalareas <- data.frame(Sepal.Area = c(11, 17,19,22,27))
pred_petalareas <- predict(lm_areas, new_sepalareas)
pred_petalareas

df_pred <- data.frame(new_sepalareas, pred_petalareas)

plot(iris$Sepal.Area, iris$Petal.Area, pch = 19, cex = 0.8)
abline(lm_areas, col = "red", lwd = 2, lty = 1)
points(df_pred$Sepal.Area, df_pred$pred_petalareas, 
       pch = 19, col = "blue", cex = 1.2)

summary(lm_areas)

# vediamo che l'insieme dei dati varia molto a seconda della specie
# potrebbe essere un motivo per cui la relazione non è strettamente lineare
# per tutto l'insieme di dati
plot(iris$Sepal.Area, iris$Petal.Area, col = as.numeric(iris$Species),
     pch = as.numeric(iris$Species))
boxplot(iris$Sepal.Area ~ iris$Species)
boxplot(iris$Petal.Area ~ iris$Species)

# verifichiamo se la specie influenza l'area del petalo
# e la relazione tra le variabili
lm_areas2 <- lm(data = iris, Petal.Area ~ Species)
summary(lm_areas2)
boxplot(lm_areas2$residuals)
plot(effect("Species", lm_areas2))

# verifichiamo se la specie influenza l'area del sepalo
# e la relazione tra le variabili
lm_areas3 <- lm(data = iris, Sepal.Area ~ Species)
summary(lm_areas3)
boxplot(lm_areas3$residuals)
plot(effect("Species", lm_areas3))
