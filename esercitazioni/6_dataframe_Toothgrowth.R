## ESEMPIO PROVA ESAME 

## PRIMA PARTE
# Creare un vettore di numeri casuali con dei valori rappresentanti i punteggi
# ottenuti ad un test
# I punteggi sono stati ottenuti da 50 studenti, e possono variare da 0 a 100
set.seed(123)
punteggi <- sample(0:100, 50, replace = TRUE)
punteggi
# fare un plot della distribuzione dei punteggi ottenuti
hist(punteggi)
# Estrarre da questo vettore gli indici dei valori >= 70 e i valori stessi
indici <- which(punteggi >= 70)
punteggi[indici]

## SECONDA PARTE
# Il dataframe ToothGrowth
# Effetto della vitamina C sulla crescita dei denti delle cavie
# Il dataset contiene i risultati di un esperimento in cui è stata
# analizzata la risposta a due diversi metodi di trattamento con vitamina C
# e tre diverse dosi (0.5, 1 e 2 mg al giorno).
# La risposta corrisponde alla lunghezza degli odontoblasti (cellule 
# responsabili della crescita dei denti) in 60 cavie
# I metodi di trattamento sono succo di arancia (OJ) oppure
# acido ascorbico (VC).

# 1. Utilizzare strumenti grafici e statistici per esplorare il dataframe
# e determinare se uno dei due trattamenti è più efficace,
# ovvero determina la lunghezza maggiore degli odontoblasti, e se ci 
# sono differenze dovute alla concentrazione usata

# Carico il dataframe ToothGrowth.
data("ToothGrowth")
str(ToothGrowth)
summary(ToothGrowth)
# Numero di osservazioni per dose e supplemento
table(ToothGrowth$dose, ToothGrowth$supp)

# distribuzione della lunghezza rispetto alla dose
boxplot(ToothGrowth$len ~ ToothGrowth$dose)
# la dose 2 sembra più efficace

# distribuzione della lunghezza rispetto al supplemento
boxplot(ToothGrowth$len ~ ToothGrowth$supp)
# OJ sembra leggermente più efficace ma non ci sono grandi differenze
# nelle distribuzioni

# distribuzione della lunghezza rispetto a supplemento + dose
boxplot(ToothGrowth$len ~ ToothGrowth$supp + ToothGrowth$dose)
# Dose 2 ha la stessa efficacia, che sembra simile tr OJ e VC
# A Dosi minori OJ è più efficace

table(ToothGrowth$supp)
table(ToothGrowth$dose)

# posso raggruppare i dati
group = character(nrow(ToothGrowth))
group[which(ToothGrowth$supp == "OJ" & ToothGrowth$dose == 0.5)] <- "OJ05"
group
group[which(ToothGrowth$supp == "OJ" & ToothGrowth$dose == 1)] <- "OJ1"
group
group[which(ToothGrowth$supp == "OJ" & ToothGrowth$dose == 2)] <- "OJ2"
group
group[which(ToothGrowth$supp == "VC" & ToothGrowth$dose == 0.5)] <- "VC05"
group
group[which(ToothGrowth$supp == "VC" & ToothGrowth$dose == 1)] <- "VC1"
group
group[which(ToothGrowth$supp == "VC" & ToothGrowth$dose == 2)] <- "VC2"
group

boxplot(data = ToothGrowth, len ~ group)

# 2. Calcolare per ciascuna coppia di supplemento + dose
# la lunghezza media degli odontoblasti e poi graficare i risultati 
# e determinare la coppia supplemento + dose che determina la lunghezza maggiore

# aggiungo la colonna group al dataframe
ToothGrowth$group <- group
head(ToothGrowth)

table(ToothGrowth$group)
# due alternative
# ... 1
OJ05 <- ToothGrowth[which(ToothGrowth$supp == "OJ" & ToothGrowth$dose == "0.5"), ]
mean_OJ05 <- mean(OJ05$len)
mean_OJ05

# ... 2
OJ05 <- ToothGrowth[which(ToothGrowth$group == "OJ05"), ]
mean_OJ05 <- mean(OJ05$len)
mean_OJ05

# usiamo il secondo metodo...
OJ1 <- ToothGrowth[which(ToothGrowth$group == "OJ1"), ]
mean_OJ1 <- mean(OJ1$len)
OJ2 <- ToothGrowth[which(ToothGrowth$group == "OJ2"), ]
mean_OJ2 <- mean(OJ2$len)

VC05 <- ToothGrowth[which(ToothGrowth$group == "VC05"), ]
mean_VC05 <- mean(VC05$len)
VC1 <- ToothGrowth[which(ToothGrowth$group == "VC1"), ]
mean_VC1 <- mean(VC1$len)
VC2 <- ToothGrowth[which(ToothGrowth$group == "VC2"), ]
mean_VC2 <- mean(VC2$len)

# metto i risultati in un vettore, do dei nomi e faccio plot
allMeans <- c("OJ05" = mean_OJ05, "OJ1" = mean_OJ1, "OJ2" = mean_OJ2,
              "VC05"= mean_VC05, "VC1" = mean_VC1, "VC2" = mean_VC2)
allMeans

barplot(allMeans)

# oppure... creo un dataframe per i risultati
dfMeans <- data.frame(suppDose = factor(c("OJ05", "OJ1", "OJ2", "VC05", "VC1", "VC2")), 
                      means = c(mean_OJ05, mean_OJ1, mean_OJ2, mean_VC05, mean_VC1, mean_VC2))

dfMeans

barplot(dfMeans$means, names.arg = dfMeans$suppDose)
# Commento: anche con i valori medi osserviamo lo stesso risultato di prima:
# a dose 2 OJ e VC hanno lo stesso effetto, a dosi minori OJ è un pò più efficace

# 3. 
#  3.1 Studiare, a parità di dosaggio, quale metodo di somministrazione è più 
# efficace utilizzando il modello lineare.
#  3.2 Verifica la significatività del modello, l'adattamento ai dati e 
# la distribuzione dei residui.

# Y = b0 + b1*supp + b2*dose
# Y = b0 + b1*group

lm_tooth <- lm(data = ToothGrowth, len ~ group)
summary(lm_tooth)

library(visreg)
visreg(lm_tooth)

# Commento: rispetto al gruppo OJ05, il gruppo OJ1 ha una lunghezza degli
# odontoblasti di 9.470 volte maggiore, mentre OJ2 di 12.830 volte maggiore.
# Entrambe le relazioni sono molto significative.
# Il gruppo  VC05 rispetto a OJ05 ha una lunghezza di 5.250 volte minore,
# il gruppo VC1 di 3.540 volte maggiore e il gruppo VC2 di 12.910 volte maggiore.
# La differenza tra VC1 e OJ05 è significativa, ma "per poco".

# Il modello in generale è significativo, guardando il p-value e
# l'R-squared

# anche la distribuzione dei residui e la media intorno a 0 ci suggerisce
# che il modello è significativo
hist(lm_tooth$residuals)
mean(lm_tooth$residuals)
boxplot(lm_tooth$residuals)

# 3.3 Controllare tramite comparazione diretta se la differenza tra "OJ2" e "VC2" è 
# significativa (uno dei due gruppi deve essere messo come gruppo di 
# riferimento)

# Riordiniamo i livelli di group in modo da ordinare le comparazioni
# così che il riferimento sia "OJ2"
ToothGrowth2 <- ToothGrowth
ToothGrowth2$group <- factor(ToothGrowth$group, levels = c("OJ2", "OJ1", "OJ05", "VC2", "VC1", "VC05"))

# Rifacciamo il modello lineare
lm_tooth2 <- lm(data = ToothGrowth2, len ~ group)
summary(lm_tooth2)

visreg(lm_tooth2)

# La differenza tra OJ2 (riferimento) e VC2 non è significativa

# 3.4 Controllare, separando i gruppi OJ e VC, se le differenze tra 
# le varie dosi di "OJ" e "VC" sono significative

dfOJ <- ToothGrowth[which(ToothGrowth$supp == "OJ"), ]
lmOJ <- lm(data = dfOJ, len ~ group)
summary(lmOJ)

dfVC <- ToothGrowth[which(ToothGrowth$supp == "VC"), ]
lmVC <- lm(data = dfVC, len ~ group)
summary(lmVC)

#  3.5 Fare una predizione, usando il primo modello ottenuto, della lunghezza
# degli odontoblasti usando una dose di 1.5 ed entrambi i tipi di supplemento
# Per aggiungere nuovi valori di "supp" e "dose", abbiamo bisogno di
# un modello lineare dove supp e dose sono separati

lm_tooth <- lm(data = ToothGrowth, len ~ supp + dose)
summary(lm_tooth)

library(effects)
plot(effect(c("dose", "supp"), lm_tooth))

newdata <- data.frame(supp = c("OJ", "VC"), dose = c(1.5, 1.5))
newdata
prediction <- predict(lm_tooth, newdata)
prediction

df_pred <- data.frame(newdata, prediction)
df_pred

boxplot(ToothGrowth$len ~ ToothGrowth$supp + ToothGrowth$dose)
abline(h = 23.9, col = "red") # OJ
abline(h = 20.2, col = "blue") # VC

# 3.6 Rimuovere le osservazioni a dose = 2 e ricalcolare l'ultimo modello
# (quello con supp + dose)
indiciRigheDaTenere <- which(ToothGrowth$dose != 2)
dfRidotto <- ToothGrowth[indiciRigheDaTenere, ]
lm_tooth_ridotto <- lm(data = dfRidotto, len ~ supp + dose)
summary(lm_tooth)$coefficients
summary(lm_tooth_ridotto)$coefficients
summary(lm_tooth)$adj.r.squared
summary(lm_tooth_ridotto)$adj.r.squared
# Commento: migliora il p-value di supp ma peggiora quello di dose
# Ha senso dato che, a parità di supplemento, osserviamo risultati diversi tra
# le tre dosi (relazione lineare), mentre la valutazione del supplemento 
# è molto più influenzata dalle osservazioni a dose 2, dove il comportamento
# dei supplementi è simile. Quindi rimuovere l'ultima dose fa sì che ora 
# per ogni coppia supp+dose, l'effetto quando il supp è OJ è sempre maggiore di VC
boxplot(data = dfRidotto, len ~ supp + dose)

library(effects)
plot(effect(c("dose", "supp"), lm_tooth))
plot(effect(c("dose", "supp"), lm_tooth_ridotto))








