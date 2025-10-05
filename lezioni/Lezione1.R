######################################################################
### 1. Ottenere informazioni sul sistema e sull'ambiente di lavoro ###
######################################################################

# Elenca i file e le directory nella directory di lavoro corrente.
dir()

# Restituisce la data corrente del sistema.
Sys.Date()

# Controllare la working directory
getwd()

# Settare una nuova working directory
setwd("C:\\Users\\romeo\\Desktop\\Metodi informatici per la Biologia\\Script")

##############################################################################
### 2. Assegnamento di variabili, ambito di variabili e operazioni di base ###
##############################################################################

# Assegnamento variabili
a = 4
a <- 4

# Calcola la somma di due numeri
sum(6, 7)

# Calcola la somma di due numeri assegnando i valori ai parametri `a` e `b` (= e <-)
# Controllare valore a
sum(a = 6, b = 7)
a
sum(a <- 6, b <- 7)
a

# Crea un vettore numerico
c(1, 2, 3)

# Crea un vettore numerico assegnando i valori con =
c(a = 1, b = 2, c = 3)
a

# Crea un vettore numerico assegnando i valori con <-
c(a <- 1, b <- 2, c <- 3)
a

#######################
### 3. Tipi di dati ###
#######################

# Numeric
x <- numeric(5)
x
class(x)

y <- as.numeric(x)
class(y)
y

# Integer
x <- integer(5)
x
class(x)

# Character
x <- character(10)
x
class(x)

# Logical
x <- logical(6)
x
class(x)

# Factor
caratteri <- c("M","F","F")
caratteri

x <- factor(c("M","F","F", "M", "M", "M"))
x
class(x)

## L'utilità di factor nei grafici

# Plot con un vettore di caratteri
plot(caratteri) 
plot(x)  # da errore!

# Bisogna prima fare una tabella di contingenza
counts <- table(caratteri)
counts

# Poi fare un barplot
barplot(counts)

# Plot con un vettore factor
x <- factor(x, levels = c("M", "F"))
x
plot(x)

# Ordinare i levels di un vettore factor in base
# all'elemento con la quantità maggiore
# tabella di contingenza del vettore factor
counts <- table(x) 
counts
# ordino le categorie dalla più numerosa alla meno numerosa
sorted <- sort(counts, decreasing = TRUE)  
# Prendo i nomi delle categorie ordinate
nomi <- names(sorted)
# Ricreo il vettore factor con i livelli ordinati
# dalla categoria più numerosa alla meno numerosa
x <- factor(x, levels = nomi )
x
# Rifaccio il plot
plot(x)


## Controllo degli errori con vettori character e factor

# Vettore character di condizione Trattato e Non trattato
# C'è un errore: un valore è scritto in modo diverso ("trattato")
treatment_char <- c("Trattato", "Non trattato", "Trattato","Trattato","Trattato", "Non trattato",
                    "Non trattato","Non trattato","Non trattato","Non trattato","Non trattato",
                    "trattato")
# Tabella di contingenza
counts <- table(treatment_char)
# Plot sbagliato, c'è una barra in più
barplot(counts)

# Vettore factor
# C'è un livello sbagliato: "trattato"
treatment_factor <- as.factor(treatment_char)
treatment_factor

# Vettore factor specificando i livelli Trattato e Non trattato
treatment_factor <- factor(treatment_char,
                    levels = c("Trattato", "Non trattato"))
# Al posto di "trattato" mi mette NA
treatment_factor

# Funzione per cercare la presenza di NA in un vettore
is.na(treatment_factor)


############################
### 4. Strutture di dati ###
############################

#####################
### 4.1 I vettori ###
#####################

# Crea vettori mediante funzione generatrice
numeric(5)
character(5)

# Crea vettori mediante la funzione c()
c(5, 6, 7, 8)
c("a", "b", "c")

# Cambia la classe di un vettore
x <- c(5, 6, 7, 8)
char_x <- as.character(x)
char_x

# Controlla la classe di un vettore
class(x)
class(char_x)

# Crea un vettore 'misto' numerico e di caratteri (stringhe)
# Il numero viene forzato a stringa perchè non ci possono essere
# due tipi di dato diverso in uno stesso vettore
x <- c("a", 1)
x
class(x)

# Creare vettori di caratteri (unendo vettori di lunghezza uguale o diversa)
# mediante la funzione paste()
x <- c("a", "b", "c")
y <- c("c", "d", "e")
paste(x, y)

x <- c("a", "b", "c", "d", "e")
y <- c("f", "g", "h")
paste(x, y, sep = "-")

## Visualizzazione di vettori
# Creiamo un vettore di numeri casuali da 1 a 10, composto da 7 numeri,
# con replacement (uno stesso numero può comparire più volte)
x <- sample(c(1:10), 7, replace = TRUE)
# type indica come rappresentare i dati
# punti
plot(x, type = "p")
# linee
plot(x, type = "l")
# punti e linee
plot(x, type = "b")

## Manipolazione di vettori
# Accedere ad un elemento del vettore
x[1]

# Accedere ad elementi contigui del vettore 
x[1:2]
x[1:3]

# Accedere ad elementi non contigui del vettore 
x[c(1,4)]

# Restituire tutti gli elementi di un vettore tranne quello in posizione 3
x[-3]

# Restituire tutti gli elementi di un vettore tranne quelli in posizione 1, 2 e 3
x[-(1:3)]

# Restituire tutti gli elementi di un vettore tranne quelli in posizione 1 e 4
x[-c(1,4)]

# Sovrascrivo l'elemento in posizione 1 nel vettore x con il valore 0
x[1] <- 0
x

# Sovrascrivo gli elementi in posizione 2 e 3 nel vettore x con il valore 0
x[2:3] <- 0
x


## I vettori logici

# Generare un vettore logico tramite funzione generatrice
logical(5)

# Generare un vettore logico verificando una condizione o più condizioni
x
# Vettore logico con il risultato della valutazione x >= 5 
# per ogni elemento di x
result <- x >= 5
# Estraggo da x solo i valori >= 5
x[result]
# Corrisponde a mettere esplicitamente gli indici dei valori >= 5
x[c(3,5)]

# Estraggo da x solo i valori < 5
# (valori che non rispettano la condizione in result)
x[result == FALSE]

# Creo un'altra condizione
result2 <- x < 10
result2

# Estraggo da x i valori che rispettano
# la condizione x >= 5 OPPURE (operatore OR) x < 10
x[x >= 5 | x < 10]

# Estraggo da x i valori che rispettano
# la condizione x >= 5 E (operatore AND) x < 10
x[x >= 5 & x < 10]
# Equivalente a scrivere
x[result & result2]

## Operazioni e statistiche di base sui vettori

# Genera un campione casuale di 100 numeri interi compresi tra 1 e 100
# con rimpiazzo  
x <- sample(c(1:100),100, replace = TRUE)
x

# Calcola la somma degli elementi nel vettore
sum(x)

# Calcola la media aritmetica degli elementi nel vettore
mean(x)
sd(x)

# Calcolo della media con un vettore di caratteri
# Restituisce `NA` (Not Available) e un avvertimento perché non è possibile calcolare
# la media di un vettore contenente una stringa.
x <- c("a", "b", "c")
mean_result <- mean(x)
mean_result

## NA e NaN

# Funzione che cerca se in un vettore c'è un NA
is.na(mean_result)

# Generiamo un NaN: un numero non reale (radice quadrata di un numero negativo)
sqrt_result <- sqrt(-1)
sqrt_result

# Funzione che cerca se in un vettore c'è un NaN
is.nan(sqrt_result)


######################
### 4.2 Le Matrici ###
######################

# Crea una matrice
m <- matrix(data = 1:9, nrow = 3, ncol = 3)
m

# Crea una matrice riempendola per riga
m <- matrix(data = 1:9, nrow = 3, ncol = 3, byrow = TRUE)
m

# Accedo ad un elemento nella matrice m
# elemento che si trova nella riga 3, colonna 2
m[3, 2]
# elementi che si trovano nella riga 3, colonne 2 e 3
m[3, 2:3]
# elementi che si trovano nella riga 3, colonne 1 e 3
m[3, c(1, 3)]

# Accedere ad una riga o colonna della matrice m
# Accedo a tutta la prima riga
m[1,]
# Accedo a tutta la prima colonna
m[,1]
# Accedo a tutta la matrice (equivalente a scrivere solo m)
m[,]

# Crea una matrice "vuota" (riempita solo da 1)
# Matrice di 1 di dimensione 1x1 (default se non specifichiamo nrow e ncol)
m <- matrix(1)
m

# Matrice di 1 di dimensione 3x1 (non abbiamo specificato ncol)
m <- matrix(1, 3)
m

# Matrice di 1 di dimensione 1x3 (non abbiamo specificato nrow)
m <- matrix(1, ncol = 3)
m

# Matrice di 1 di dimensione 3x3 in cui abbiamo cambiato l'ordine 
# degli argomenti (in questo caso vanno esplicitati con l'assegnazione)
m <- matrix(nrow = 3, data = 1, ncol = 3)
m

# Crea una matrice vuota (riempita di NA)
m <- matrix(NA, 3, 3)
m

# Creo una matrice 3x3 fatta di 9 numeri casuali da 1 a 20
m <- matrix(data = sample(1:20, 9, replace = TRUE), nrow = 3, ncol = 3)
m
# Sovrascrivo l'elemento in posizione 1,1 della matrice m con 0
m[1,1] <- 0
m

# Sovrascrivo tutta la riga 3 della matrice m con 0
m[3, ] <- 0
m

# Sovrascrivo tutta la colonna 2 della matrice m con -1
m[, 2] <- -1
m

# Sovrascrivo tutta la matrice con il valore -1
m[,] <- -1
m

# Crea una matrice con input non coerente
# Se la lunghezza dell'input è MINORE del numero di righe x colonne, 
# R ricicla i valori dell'input
m <- matrix(1:9, 3, 6, byrow = TRUE)
m

# Se la lunghezza dell'input è MAGGIORE del numero di righe x colonne, 
# R da un warning e tronca l'input
m <- matrix(1:9, 3, 2, byrow = TRUE)
m


