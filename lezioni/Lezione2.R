#############
## MATRICI ##
#############

m <- matrix(sample(1:20, 10), ncol = 5, nrow = 2)
m

# vettore TRUE o FALSE per ogni valore in base
# alla condizione
a <- c(1,2,3,4)
a > 2

# matrice TRUE o FALSE per ogni valore in base
# alla condizione
m >= 10

# Trovare in una matrice gli indici dei valori
# che rispettano una certa condizione
# la matrice è "srotolata" e gli indici sono consecutivi
which(m >= 10)
# Estrarre valori da una matrice con which()
m[which(m >= 10)]
# arr.ind restituisce indici di riga e colonna
which(m >= 10, arr.ind = TRUE)
m[which(m >= 10, arr.ind = TRUE)]

m[1,1]

# Condizioni multiple
m[which((m >= 10) & (m < 15))]
m

# Help della funzione which()
?which()


## ARRAY ##
a <- array(sample(1:20, 30, replace = TRUE), dim = c(5,2,3))
?array
a

# indicizzare array
# valore nella prima riga, prima colonna della 
# prima sottomatrice
a[1,1,1]
# tutta la prima colonna della seconda sottomatrice
a[,1,2]
# tutta la seconda riga della terza sottomatrice
a[2,,3]
# tutto l'array
a[,,]

###########
## LISTE ##
###########

sequenza <- "ACTGACCGGGACCCCCATCACA"
# la funzione strsplit restituisce una lista
nucleotidi <- strsplit(sequenza, "")
# estrarre la sequenza dalla lista
nucleotidi[[1]]

# Creare una lista contenente un vettore e una matrice
v <- c(1,2,3,4,5)
m <- matrix(sample(1:20, 10), ncol = 5, nrow = 2)
lista <- list(v, m)
lista
# creare una lista contenente un vettore e una matrice
# dando un nome agli oggetti
lista <- list("vettore" = v, "matrice" = m)
lista

# estrarre oggetti da una lista tramite nome
lista$vettore
# ... o tramite indice
lista[[1]]

###############
## DATAFRAME ##
###############

# Creare un dataframe avente come colonne un vettore di numeri,
# un vettore di caratteri e un vettore factor
v <- c(4, 9, 1, 0, 7)
c <- c("A","A","B","B","C")
h <- factor(c("L1","L2","L2","L2","L1"), levels = c("L1", "L2"))

df <- data.frame(v, c, h)
df

# rinominiamo le colonne
df <- data.frame("vettore" = v, "char" = c, "factor" = h )
df

# estraiamo la colonna "vettore" dal dataframe
df$vettore
# classe della colonna "vettore"
class(df$vettore)
# somma degli elementi nella colonna "vettore"
sum(df$vettore)

# assegniamo la colonna "vettore" ad una variabile
# e applichiamo varie funzioni
v2 <- df$vettore
class(v2)
sum(v2)
mean(v2)

# Esploriamo un dataframe
# struttura del dataframe
str(df)
# statistiche base e contenuto del dataframe
summary(df)

# Visualizziamo il dataframe iris
iris
# Carichiamo il dataframe iris nell'Environment
data("iris")

# Esplorare il dataframe
# Prime 10 righe
head(iris, 10)
# Ultime 2 righe
tail(iris, 2)

summary(iris)
str(iris)

# Dimensioni (righe colonne) del dataframe
dim(iris)
# Numero di righe
ncol(iris)
# Numero di colonne
nrow(iris)

# Livelli della colonna factor Species
levels(iris$Species)

# Alcuni plot

# Plot di tutto il dataframe 
# Ogni colonna vs ogni colonna
plot(iris)

# Per resettare lo spazio Plots
dev.off()

# BOXPLOTS
# Boxplot di ogni variabile rispetto a (~) un'altra
boxplot(iris$Sepal.Length ~ iris$Species)
boxplot(iris$Petal.Length ~ iris$Species)
boxplot(iris$Sepal.Width ~ iris$Species)
boxplot(iris$Petal.Width ~ iris$Species)

# ISTOGRAMMI
# breaks indica il numero di barre
hist(iris$Sepal.Length, breaks = 20)
?hist

# separiamo la finestra in due colonne
# e plottiamo due grafici affiancati
par(mfrow = c(1, 2))
hist(iris$Sepal.Length, breaks = 20)
hist(iris$Sepal.Length, breaks = 30)

# separiamo la finestra in due colonne e due righe
# e plottiamo quattro grafici affiancati
par(mfrow = c(2, 2))
# se ricevete l'errore "Errore in plot.new() : figure margins too large"
# bisogna ridurre i margini della figura 2/o ingrandire
# la finestra plot
# per ridurre manualmente i margini (es. margine 1 per lato)
par(mar = c(1,1,1,1))

hist(iris$Sepal.Length, breaks = 10)
hist(iris$Sepal.Length, breaks = 20)
hist(iris$Sepal.Length, breaks = 30)
hist(iris$Sepal.Length, breaks = 40)

# Resettiamo lo spazio plots 
dev.off()
# oppure per tornare alla visualizzazione di un solo grafico per volta
par(mfrow = c(1, 1))
# torniamo a margini più stretti di default
par(mar = c(5.1, 4.1, 4.1, 2.1))
# coloriamo ogni barra di darkblue
hist(iris$Sepal.Length, breaks = 40, 
     col = "darkblue")

# BARPLOT
# Bisogna fare una tabella di contingenza con la 
# funzione table() per contare le occorrenze di
# ogni specie, da dare in input a barplot
t <- table(iris$Species)
# un colore diverso per ogni colonna
barplot(height = t, col = c("red","green","blue"))
?barplot

# plot() su un vettore factor ci da un barplot
plot(iris$Species)

# cex controlla la dimensione dei simboli in plot()
plot(iris$Sepal.Length, cex = 5)
plot(iris$Sepal.Length, cex = 2)

# pch controlla la forma dei simboli
plot(iris$Sepal.Length, pch = 3)

# tutti i simboli utilizzabili
# valori pch da 1 a 25
points_pch <- 1:25
plot(1:length(points_pch), 1:length(points_pch),
     pch = points_pch, cex = 2)







