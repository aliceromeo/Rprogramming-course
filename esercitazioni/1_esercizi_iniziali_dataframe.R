## Esercizi riadattati da (soluzioni al link):
# https://www.w3resource.com/r-programming-exercises/dataframe/index.php

# Creare un dataframe vuoto
df <- data.frame()
df

# Creare un dataframe a partire da questi quattro vettori:
# vettore 'names' di tipo character
# vettore 'score' di tipo numeric
# vettore 'attempts' di tipo integer
# vettore 'qualify' di tipo character
name = c('Anastasia', 'Dima', 'Katherine', 'James', 'Emily', 'Michael', 'Matthew', 'Laura', 'Kevin', 'Jonas')
score = c(12.5, 9, 16.5, 12, 9, 20, 14.5, 13.5, 8, 19)
attempts = c(1, 3, 2, 3, 2, 3, 1, 1, 2, 1)
qualify = c('yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes')

df <- data.frame(name, score, attempts, qualify)
df

# Visualizza la struttura del dataframe creato
str(df)

# Visualizza statistiche sul dataframe creato
summary(df)

# Estrarre la colonna 'score' dal dataframe e salvarla in una variabile
sc <- df$score
sc

# Controllare la classe di questa colonna
class(score)

# Cambiare la classe della colonna nel dataframe in integer
df$score <- as.integer(df$score)
df

# Estrarre le prime due righe dal dataframe
df[1:2,]

# Estrarre la terza e quinta riga e prima e terza colonna dal dataframe
df[c(3,5), c(1,3)]

# Aggiungere una nuova colonna 'country' (tipo character) al dataframe
# (inventare i valori da inserire)
df$country <- c("Italy", "USA", "UK", "Italy", "USA", "UK", "Italy", "USA", "UK", "Italy")

# Estrarre i nomi delle colonne
names(df)

# Aggiungere una nuova riga al dataframe
# (anche qui inventare dei valori)
# N.B. potete usare str(df) per controllare la classe di ogni colonna e che valori mettere
str(df)
riga <- data.frame(name = "Jack", score = 10, attempts = 4, qualify = "no", country = "UK")
riga
df <- rbind(df, riga)

# Estrarre solo le ultime 3 righe del dataframe
tail(df)

str(df)

# Eliminare la colonna 'attempts'
df <- subset(df, select = -attempts)
names(df)
str(df)

# Eliminare la terza riga del dataframe
dim(df)
df <- df[-3, ]
dim(df)

# Cambiare i nomi alle prime due colonne del dataframe
names(df)[1:2] <- c("NOMI", "PUNTEGGI")
names(df)

str(df)

# Cambiare il nome all'ultima colonna del dataframe
names(df)[4] <- c("STATO")
names(df)

str(df)

# Inserire dei valori NA in posizioni casuali del dataframe
# nelle colonne 'score' e 'attempts'
df$PUNTEGGI[which(df$PUNTEGGI < 10)] <- NA
summary(df)

# Contare il totale dei valori NA per riga
rowSums(is.na(df))

# Contare il totale dei valori NA per colonna
colSums(is.na(df))


# Sostituire i valori NA con 0
df$PUNTEGGI[is.na(df$PUNTEGGI)] <- 0
df
summary(df)

#####

# Creare un dataframe a partire da questi vettori

Firstname <- c("Alice", "Paul", "Jerry", "Thomas", "Marguerite", "Linda")
Lastname <- c("Ryan", "Collins", "Burke", "Dolan", "Black", "McGrath")
Age <- c(37, 34, 26, 72, 18, 24)
Gender <- c("F", "M", "M", "M", "F", "F")
Points <- c(278, 242, 312, 740, 177, 195)

df <- data.frame(Firstname, Lastname, Age, Gender, Points)
df

# Salva i valori di 'Points' in un vettore
# Calcola la media dei punti
pts <- df$Points

# Salva i dati corrispondenti alle donne in un altro dataframe 
# chiamato 'fpoints' e poi visualizzarne le statistiche principali
fpoints <- df[which(df$Gender == "F"), ]
fpoints

# Sostituire l'eta di Paul Collins con 48
df
df$Age[which((df$Firstname == "Phil") & (df$Lastname == "Collins"))] <- 48
df

# Calcolare l'età massima degli uomini
max(df$Age[which(df$Gender == "M")])


# Estrarre le righe corrispondenti alle persone che hanno ottenuto più di
# 100 punti e che hanno più di 30 anni
df[which((df$Points > 100)) & (df$Age > 30), ]


# Fare un grafico delle distribuzioni delle età per genere
boxplot(df$Age ~ df$Gender)

# Fare un grafico delle distribuzioni dei punti per genere
boxplot(df$Points ~ df$Gender)

# Fare un istogramma della distribuzione dei punteggi
# (provare diversi breaks e vedere come varia)
hist(df$Points, breaks = 3)
hist(df$Points, breaks = 5)
hist(df$Points, breaks = 10)

# Calcolare la media dei punteggi
avg <- mean(df$Points)

# Plottare una linea verticale colorata e tratteggiata sul barplot creato
# in corrispondenza della media
abline(v = avg, col = "red", lty = 2, lwd = 1)

# Dividere in due plot affiancati la finestra Plots
par(mfrow = c(1,2))

# Plottare la distribuzione dei punteggi con un boxplot
# Mettere come estremi dell'asse y del grafico il minimo ed il massimo
# della distribuzione
boxplot(df$Points, ylim = c(min(df$Points), max(df$Points)))

# Calcolare i quantili della distribuzione
q <- quantile(df$Points)
q

# Plottare i quantili sul grafico come linee orizzontali tratteggiate e colorate
abline(h = q[1], col = "blue", lty = 2, lwd = 1)
abline(h = q[2], col = "blue", lty = 2, lwd = 1)
abline(h = q[3], col = "blue", lty = 2, lwd = 1)
abline(h = q[4], col = "blue", lty = 2, lwd = 1)
abline(h = q[5], col = "blue", lty = 2, lwd = 1)

# Rimuovere il/i valore/i outlier dal dataframe e salvare
# il dataframe ridotto in una nuova variabile
df_reduced <- df[-which(df$Points > 400),]
df_reduced

# Plottare la nuova distribuzione dei punteggi con un boxplot
# Mettere come estremi dell'asse y del grafico il minimo ed il massimo
# della distribuzione iniziale in modo da avere i grafici in scala
boxplot(df_reduced$Points, ylim = c(min(df$Points), max(df$Points)))

# Calcolare i quantili della nuova distribuzione e plottarli
q2 <- quantile(df_reduced$Points)
abline(h = q2[1], col = "blue", lty = 2, lwd = 1)
abline(h = q2[2], col = "blue", lty = 2, lwd = 1)
abline(h = q2[3], col = "blue", lty = 2, lwd = 1)
abline(h = q2[4], col = "blue", lty = 2, lwd = 1)
abline(h = q2[5], col = "blue", lty = 2, lwd = 1)

# Riportare la finestra grafica ad un solo plot
par(mfrow = c(1,1))

# Fare uno scatterplot dei punteggi 
# utilizzare come simbolo i cerchi pieni di dimensione 1.5 e colorare in base al genere
plot(df$Points, col = factor(df$Gender), pch = 19, cex = 1.5)

# fare una legenda per questo plot, posizionata in alto a destra
legend("topright", legend = unique(df$Gender), fill = factor(df$Gender))
