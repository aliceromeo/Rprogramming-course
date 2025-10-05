#########################
### rbind() e cbind() ###
#########################

# creiamo due dataframe
df1 <- data.frame(x = 1:10, y = sample(c("A","B"), 10, replace = T))
df2 <- data.frame(x = 11:20, y = sample(c("C", "D"), 10, replace = T))

# struttura dei due dataframe
str(df1)
str(df2)

# uniamo i dataframe per riga
df_rbind <- rbind(df1, df2)
# uniamo i dataframe per colonna
df_cbind <- cbind(df1, df2)
head(df_cbind, 5)

# plot della colonna x nel dataframe unito
# e nei due dataframe originali
boxplot(df_rbind$x)
boxplot(df1$x)
boxplot(df2$x)

# plot della colonna x nei tre dataframe
# specificando gli estremi dell'asse y
# per mettere tutti i dati su degli assi di riferimento uguali
# dividiamo la finestra grafica in tre riquadri
par(mfrow = c(1, 3))
boxplot(df_rbind$x, ylim = c(0,20))
boxplot(df1$x, ylim = c(0,20))
boxplot(df2$x, ylim = c(0,20))

#
# riportiamo la finestra grafica a un riquadro
par(mfrow = c(1, 1))
# boxplot dei valori della colonna x del dataframe df_rbind
# rispetto ai valori della colonna y
boxplot(df_rbind$x ~ df_rbind$y)

# tabella di contingenza di df_rbind
table(df_rbind)
# barplot
barplot(table(df_rbind))

# boxplot della colonna x di df_cbind
# la seconda colonna 'x' viene ignorata
# perchè ci sono due colonne con lo stesso nome
boxplot(df_cbind$x)
# rinominiamo le colonne del dataframe
names(df_cbind) <- c("x", "y", "z", "w")
names(df_cbind)
# boxplot della colonna x e colonna z affiancate
boxplot(df_cbind$x, df_cbind$z)

# barplot della colonna x colorato in rosso
barplot(df_rbind$x, col = "red")

# coloriamo in rosso i valori che appartenevano
# a df1 (valori da 1 a 10), e in blu quelli che 
# appartenevano a df2 (da 11 a 20).
# selezioniamo gli indici delle righe di df_rbind con valori
# nella colonna x <= 10
condizione <- which(df_rbind$x <= 10)
# creiamo una nuova colonna 'color' in df_rbind
# estraiamo le righe corrispondenti agli indici selezionati
# (sono nella variabile 'condizione') e assegniamo il valore
# "red" alla nuova colonna
df_rbind$color[condizione] <- "red"
head(df_rbind)
# stessa cosa ma per i valori della colonna x > 10
condizione <- which(df_rbind$x > 10)
df_rbind$color[condizione] <- "blue"
str(df_rbind)

# barplot dei valori di 'x' in df_rbind
# il colore della barre, per ciascuna barra, è preso dalla colonna
# 'color' in df_rbind
barplot(df_rbind$x, col = df_rbind$color)

########
## NA ##
########

# Creiamo un vettore con un NA
x <- c(1,2,3,NA)
sum(x) # se c'è un NA la somma viene NA
sum(x, na.rm = TRUE) # opzione per escludere i valori NA
                      # dal calolo della somma

# Mettiamo un 0 al posto dell'NA e ricalcoliamo la somma
x[4] <- 0
x
sum(x)

# Mettiamo tutti NA in ogni colonna delle righe
# 3, 5, 15 e 20 del dataframe iris
iris[c(3,5,15,20),] <- NA
summary(iris) # summary ci dice quanti NA ci sono per ogni colonna

# sintassi di apply()
# X: dataset su cui lavorare
# MARGIN: 1 -> la funzione viene applicata su ogni riga
#         2 -> la funzione viene applicata su ogni colonna
# FUN: la funzione da applilcare
# apply(X, MARGIN, FUN)

## Togliere le righe con NA dal dataframe iris
# 1. Metodo "esteso" che sfrutta la funzione apply()
# (non comodo da usare, utile solo per capire il
# funzionamento di apply() )
# funzione apply() per trovare le righe di iris dove ci sono NA
iris[which(apply(iris, 1, function(x) any(is.na(x)))), ]

# applico la funzione apply() sul dataframe iris (X),
# su ogni riga (MARGIN = 1), e applico la funzione 
# any(is.na) che mi controlla per ogni riga se c'è un NA.
# ogni riga del dataframe è indicata con la 'x' nella funzione.
# salvo tutto nella variabile condizione
condizione <- apply(X = iris, MARGIN = 1, 
                    FUN = function(x) any(is.na(x)))

# estraggo dal dataframe le righe che rispettano la condizione (ovvero che 
# contengono NA) usando which()
iris[which(condizione), ]

# dimensione (righe colonne) del dataframe iris
dim(iris)

# Tolgo le righe che rispettano la condizione fatta prima
# (ovvero che contengono NA)
# Per togliere va usato il " - "
# Risalvo il dataframe in una nuova variabile 'iris_subset'
iris_subset <- iris[-(which(condizione)), ]

# controllo le dimensioni del nuovo dataframe e 
# se ci sono ancora NA
dim(iris_subset)
summary(iris_subset)

# 2. Metodo più rapido
iris_subset2 <- na.omit(iris)
dim(iris_subset2)

#####################################
## REGOLA GENERALE PER USARE WHICH ##
#####################################
# 1. Definire una condizione di selezione con 
# >, <, == ecc... e gli operatori booleani &, | o !, 
# che ci restituisca una serie di TRUE o FALSE
# es. condizione <- ....
# 2. usare which() per estrarre gli indici delle righe che
# rispettano la condizione
# es. indici_estratti <- which(condizione)
# 3. uso gli indici per estrarre da un dataframe le righe
# che rispettano una condizione
# es. iris[which(condizione)]


####
# Ricaricare il dataframe iris
data(iris)
str(iris)
# Togliere la colonna 'Species' da iris
# metodo 1 (tramite indici)
iris_nosp <- iris[ , 1:4]
str(iris_nosp)
# metodo 2 (tramite la funzione subset)
iris_nosp2 <- subset(iris, select = -Species)
str(iris_nosp2)

# somma di tutti i valori per colonna
colSums(iris_nosp2)
# somma di tutti i valori per riga
rowSums(iris_nosp2)

## Trovare la media per specie della lunghezza dei petali ##
## Media per la specie "setosa"
# estraiamo tutte le righe della specie "setosa"
condizione <- iris$Species == "setosa"
df_setosa <- iris[which(condizione), ]
# calcoliamo la media della colonna Petal.Length
# solo per queste righe estratte e la salviamo in
# una variabile
avg_setosa_PL <- mean(df_setosa$Petal.Length)

# stessa cosa per le altre due specie
condizione <- iris$Species == "versicolor"
df_vers <- iris[which(condizione), ]
avg_vers_PL <- mean(df_vers$Petal.Length)

condizione <- iris$Species == "virginica"
df_virg <- iris[which(condizione), ]
avg_virg_PL <- mean(df_virg$Petal.Length)

avg_setosa_PL
avg_vers_PL
avg_virg_PL

# creiamo un vettore con tutte le medie
averages_PL <- c(avg_setosa_PL, avg_vers_PL, avg_virg_PL)
averages_PL
# plot delle medie come punti
plot(averages_PL)

# creiamo un dataframe con i valori delle medie
# e con l'indicazione della rispettiva specie
# e dei colori da assegnare a ciascuna specie
df_averages <- data.frame(specie = c("SET", "VER", "VIR"), 
                          avg = averages_PL, 
                          colori = c("red","blue","green"))
df_averages
# barplot del dataframe appena creato, colorato per specie
barplot(df_averages$avg, col = df_averages$colori)
# diamo un titolo al grafico
barplot(df_averages$avg, col = df_averages$colori, 
        main = "Medie")
# assegniamo i nomi degli assi x per ogni barra
barplot(df_averages$avg, col = df_averages$colori, 
        main = "Medie", names.arg = df_averages$specie)

# assegniamo una legenda al grafico
# posizione in alto a sinistra (topleft)
# i valori da scrivere nella legenda sono i nomi delle specie (legend = )
# il colore dei simboli nella legenda sono nella colonna 'colori' (fill = )
# cex controlla la dimensione del testo nella legenda
legend("topleft", legend = df_averages$specie,
       fill = df_averages$colori, cex = 0.5)
?legend()

# stessa cosa ma con uno scatterplot
plot(df_averages$avg, col = df_averages$colori, pch = 19)
legend("topleft", legend = df_averages$specie,
       fill = df_averages$colori, cex = 0.6)

#####################
# quantili, mediane #
#####################

boxplot(iris$Sepal.Length)
# calcoliamo quantili e mediane
# di default ogni 25% dei dati
# valori che dividono la distribuzione 4 parti ogni 25%
q <- quantile(iris$Sepal.Length)
q
# Tracciamo sul grafico delle linee orizzontali che dividono
# i dati in base ai quantili (estraimo i quantili dal vettore 'q')
# lty = 2 fa una linea tratteggiata
# lwd = 2 aumenta lo spessore della linea
abline(h = q[1], col = "blue", lty = 2, lwd = 2)
abline(h = q[2], col = "blue", lty = 2, lwd = 2)
abline(h = q[3], col = "blue", lty = 2, lwd = 2)
abline(h = q[4], col = "blue", lty = 2, lwd = 2)
abline(h = q[5], col = "blue", lty = 2, lwd = 2)

# facciamo un istogramma dei valori di Sepal.Length
hist(iris$Sepal.Length, breaks = 20)
# plottiamo delle linee verticali in corrispondenza del
# quantile al 25%, mediana e quantile al 75% per delimitare
# la distribuzione dei dati
abline(v = q[2], col = "red", lty = 2, lwd = 2)
abline(v = q[3], col = "red", lty = 2, lwd = 2)
abline(v = q[4], col = "red", lty = 2, lwd = 2)
q
