## PRIMA PARTE
# Crere un veettore numerico con elementi 40, NA, 10, 5, e NA
x = c(40, NA, 10, 5, NA)
# Calcolare la somma ignorando gli NA e salvarla in una variabile
somma <- sum(x, na.rm = TRUE)
somma
# Calcolare la media ignorando gli NA e salvarla in una variabile
media <- mean(x, na.rm = TRUE)
media

# Moltiplicare somma e media e rappresentare poi le tre variabili con un
# barplot. Mettere label asse x per ogni barra, dare un titolo
# e impostare l'asse y da 0 a 1200
prodotto <- media * somma
prodotto

barplot(c(somma, media, prodotto), names.arg = c("somma", "media", "prodotto"), 
        main = "Grafico", ylim = c(0,1200))

## SECONDA PARTE
## DATAFRAME ChickWeight

#weight
# a numeric vector giving the body weight of the chick (gm).
#Time
# a numeric vector giving the number of days since birth when the measurement 
# was made.
#Chick
#an ordered factor with levels 18 < … < 48 giving a unique identifier for the 
# chick. The ordering of the levels groups chicks on the same diet together 
# and orders them according to their final weight (lightest to heaviest)
# within diet.
# Diet
# a factor with levels 1, …, 4 indicating which experimental diet the chick 
# received.

# Esplorare il dataframe e individuare relazioni
ChickWeight
str(ChickWeight)
summary(ChickWeight)

head(ChickWeight)

table(ChickWeight$Diet)

plot(ChickWeight)

boxplot(ChickWeight$weight ~ ChickWeight$Diet)
boxplot(ChickWeight$Time ~ ChickWeight$Diet)
boxplot(ChickWeight$weight ~ ChickWeight$Time)
boxplot(ChickWeight$weight ~ ChickWeight$Chick )

# Chick è un factor ordered, ordinato per peso e separato per dieta
plot(as.numeric(ChickWeight$Chick), ChickWeight$weight, 
     col = ChickWeight$Diet)

plot(ChickWeight$weight ~ as.numeric(ChickWeight$Diet))
plot(ChickWeight$weight ~ ChickWeight$Time)

## 2. Analizzare se esiste una relazione lineare tra weigth, diet e time

# modello lineare solo per diet
model_diet=lm(data=ChickWeight, weight ~ Diet)
summary(model_diet)
# diffrenze significative tra diete
# non spiega per niente i dati
boxplot(ChickWeight$weight ~ ChickWeight$Diet)
# controllo i residui: curva più ampia
hist(model_diet$residuals)

# modello lineare solo per time
model_time=lm(data=ChickWeight, weight ~ Time)
summary(model_time)
# spiega molto bene i dati e relazione lineare significativa
boxplot(ChickWeight$weight ~ ChickWeight$Time)
# controllo i residui: molto più centrati su 0
hist(model_time$residuals)

model_timediet=lm(data=ChickWeight,
                   weight~ Time + Diet)

summary(model_timediet)$coefficients
summary(model_time)$coefficients
summary(model_diet)$coefficients

summary(model_timediet)$adj.r.squared
summary(model_time)$adj.r.squared
summary(model_diet)$adj.r.squared

## proviamo a mettere time in logaritmo
model_timediet_log=lm(data=ChickWeight,
                   weight~log(Time) + Diet) #per la relazione di von B.
# errore -> logaritmo di 0
min(ChickWeight$Time)

# proviamo a metterlo esponenziale
model_timediet_exp=lm(data=ChickWeight,
                   weight~exp(Time) + Diet)
# peggiora r-squared : non è più un modello lineare
summary(model_timediet_exp)

summary(model_timediet)$adj.r.squared
summary(model_timediet_exp)$adj.r.squared

boxplot(data=ChickWeight,
        weight~exp(Time) + Diet)

par(mfrow=c(1,2))
hist(model_timediet$residuals)
hist(model_timediet_exp$residuals) # peggiorano gli errori

par(mfrow=c(1,1))
