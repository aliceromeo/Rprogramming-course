# Testare l'idea che due variabili abbiano una relazione lineare

# x: variabile indipendente
# y: variabile dipendente
# se x e y hanno una relazione lineare, un cambiamento nell'asse x 
# porta ad un cambiamento concomitante nell'y, ovvero y dipende da x

# Domande da porsi di fronte a due variabili:
# 1. c'è una relazione di dipendenza tra le due variabili?
# 2. quale può essere la variabile dipendente e quale/i indipendente/i?
# 3. quanta della variazione della y (variabile dipendente) può essere 
# spiegata da variazioni di x (variabile indipendentei)? Dipende anche 
# da altre variabili?
# 4. possiamo PREDIRE quale valore di y possiamo aspettarci per un 
# certo nuovo valore di x?

# Risultati del modello:

# - intercetta y
# - slope: quanto si muove y per ogni unità di incremento di x
# Avendo intercetta e slope possiamo disegnare una retta
# - p-value (significatività statistica)
# -- ipotesi H0: se la slope fosse 0, quindi non ci fosse relazione tra
# le due variabili, e prendessimo un campione di punti, quale è la 
# probabilità che per caso questi punti generino questa stessa retta?
# -- ipotesi H1: la retta che vediamo è statisticamente
# significativa, ovvero ha una probabilità bassissima di essere ottenuta
# per caso
# > p-value basso (< 0.005): la probabilità che questo risultato
# sia dovuto al caso è bassissima e possiamo rigettare l'ipotesi H0
# - Adjusted R^2: quanta variazione dei dati y può essere spiegata
# da cambiamenti nell'asse x (assume valori da 0 a 1)

## ESEMPIO 1

####################
## DATAFRAME CARS ##
####################

data(cars)
str(cars)

# relazione tra le variabili
# x: velocità (variabile indipendente)
# y: distanza di frenata (variabile dipendente)
# la distanza di frenata dipende linearmente dalla velocità 
# della macchina?
plot(cars$speed, cars$dist, pch = 19, cex = 0.8)

# applichiamo un modello lineare semplice
linear_model <- lm(dist ~ speed, data = cars)
linear_model
summary(linear_model)

# 1. c'è una relazione di dipendenza tra le due variabili?
# 2. quanta della variazione della y può essere spiegata da
# variazioni di x? o dipende da altre variabili?

# controlliamo il fit del modello 
plot(cars$speed, cars$dist, xlab = "speed", ylab = "dist",
     pch = 19, cex = 0.8)
abline(linear_model, col = "red", lwd = 2)

# salviamo l'immagine del fit
png(filename = "fit.png")
plot(cars$speed, cars$dist, xlab = "speed", ylab = "dist",
     pch = 19, cex = 0.8)
abline(linear_model, col = "red", lwd = 2)
dev.off()

# controlliamo i residui
boxplot(linear_model$residuals)
hist(linear_model$residuals)
# controlliamo che la media sia sullo 0
abline(v = mean(linear_model$residuals), col = "red", lwd = 2)

# riportiamo i residui sui dati
plot(cars$speed, cars$dist, pch = 19, cex = 0.8)
abline(linear_model, col = "red", lwd = 2)
segments(x0 = cars$speed, y0 = cars$dist,
         x1 = cars$speed, y1 = predicted_dist,
         col = "darkgreen", lwd = 1.8, lty = 3)

plot(linear_model) # mettere INVIO nella console
plot(linear_model, which = 1)
plot(linear_model, which = 2)
plot(linear_model, which = 3)
plot(linear_model, which = 4)
plot(linear_model, which = 5)
plot(linear_model, which = 6)

# confrontiamo il fit delle distanze predette con i dati originali
# se il fit fosse perfetto tutti i punti sarebbero esattamente su
# una retta
predicted_dist <- predict(linear_model)
predicted_dist
plot(cars$dist, predicted_dist, pch = 19)

# 3. possiamo PREDIRE quale valore di y possiamo
# aspettarci per un certo valore di x?
# vogliamo predire i valori di distanza rispetto a tre nuovi
# valori di velocità
newspeeds <- data.frame(speed = c(10,15,20))
dist_predictions <- predict(linear_model, newspeeds)
df_pred <- data.frame(newspeeds, dist_predictions)
df_pred
plot(cars$speed, cars$dist, pch = 19, cex = 0.8)
abline(linear_model, col = "red", lwd = 2)
points(df_pred$speed, df_pred$dist_predictions, col = "blue", pch = 19)

# facciamo delle linee verticali ed orizzontali che collegano i punti 
# predetti agli assi x e y
for (i in 1:nrow(df_pred)) {
  # Linea verticale (dall'asse x al punto)
  segments(df_pred$speed[i], 0, df_pred$speed[i],
           df_pred$dist_predictions[i], col = "darkgreen", lty = 2)
  
  # Linea orizzontale (dall'asse y al punto)
  segments(0, df_pred$dist_predictions[i], df_pred$speed[i],
           df_pred$dist_predictions[i], col = "darkgreen", lty = 2)
}



## ESEMPIO 2

####################
## DATAFRAME IRIS ##
####################

plot(iris)
plot(iris$Petal.Width, iris$Petal.Length, pch = 19, cex = 0.8)

# x (variabile indipendente): petal width
# y (variabile dipendente): petal length

linear_model2 <- lm(Petal.Length ~ Petal.Width, data = iris)
summary(linear_model2)
# la retta di regressione
abline(linear_model2, col = "red", lwd = 2)

# controlliamo i residui
boxplot(linear_model2$residuals)
hist(linear_model2$residuals)
abline(v = mean(linear_model2$residuals), col = "red", lwd = 2)

# visualizziamo i residui
plot(iris$Petal.Width, iris$Petal.Length, pch = 19, cex = 0.8)
abline(linear_model2, col = "red", lwd = 2)
segments(x0 = iris$Petal.Width, y0 = iris$Petal.Length, 
         x1 = iris$Petal.Width, y1 = pred_PW, 
         col = "darkgreen", lwd = 1.5)

# visualizziamo la retta di regressione e gli intervalli di confidenza
# con il pacchetto visreg
#install.packages("visreg")
library(visreg)
visreg(linear_model2)

# controllo del fit delle predizioni con i valori ossevati
pred_PW <- predict(linear_model2)
plot(iris$Petal.Length, pred_PW, pch = 19, cex = 0.8)

# facciamo delle nuove predizioni di lunghezza dei petali
# sulla base di nuovi valori di ampiezza dei petali
newPW <- data.frame(Petal.Width = c(0.7, 1.4, 2.3))
PL_predictions <- predict(linear_model2, newPW)
PL_predictions

df_pred <- data.frame(newPW, PL_predictions)
df_pred

# rappresentiamo le predizioni sul grafico
points(df_pred$Petal.Width, df_pred$PL_predictions,
       col = "blue", pch = 19)

for (i in 1:nrow(df_pred)){
  segments(df_pred$Petal.Width[i], 0, 
           df_pred$Petal.Width[i], df_pred$PL_predictions[i],
           col = "darkgreen", lty = 2, lwd = 1.5)
  segments(0, df_pred$PL_predictions[i], 
           df_pred$Petal.Width[i], df_pred$PL_predictions[i], 
           col = "darkgreen", lty = 2, lwd = 1.5)
}
