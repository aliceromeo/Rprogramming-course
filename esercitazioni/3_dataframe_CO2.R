## ESEMPIO 3

## PRIMA PARTE
# Crea una matrice 10x5 con numeri interi casuali (punteggi).
# Calcola la media dei punteggi per ogni riga.
# Identifica gli indici delle medie superiori a 35
# Visualizza le medie con un grafico a barre
set.seed(456)
score <- matrix(sample(1:50, 50, replace = TRUE), nrow = 10, ncol = 5)
print("Matrice dei punteggi:")
print(score)

medie <- rowMeans(score)
print("Medie dei punteggi:")
print(medie)

indexHighMeans <- which(medie > 35)
print("Indici righe con media superiore a 35:")
print(indexHighMeans)

barplot(medie, names.arg = "Medie", 
        main = "Medie degli score", col = "skyblue")

## SECONDA PARTE
# DATAFRAME CO2

# CO2 uptake of six plants from Quebec and six plants from Mississippi
# was measured at several levels of ambient concentration. 
# Half the plants of each type were chilled overnight before the 
# experiment was conducted.

# Caricare ed esplorare il dataframe
data(CO2)

str(CO2)
summary(CO2)

table(CO2$Type)
table(CO2$Treatment)
table(CO2$Plant)
table(CO2$Treatment,CO2$Type)
table(CO2$Treatment,CO2$Type, CO2$Plant) 
CO2
dim(CO2)

# piante esposte alle diverse concentrazioni
unique(CO2$Plant)
length(unique(CO2$Plant))

# visto che abbiamo diverse variabili facciamo un plot generale
plot(CO2)

#indago la relazione tra uptake e conc
# simbolo diverso per tipo diverso di pianta
plot(x= CO2$conc, y=CO2$uptake,
     pch=as.numeric(CO2$Type))
legend("topleft", cex = 0.4, legend = unique(CO2$Type), 
       pch = unique(CO2$Type))
#i triangoli appaiono tutti nella parte bassa del grafico 
#anche all'aumentare delle conc di CO2. La risposta dipende 
#dall'origine geografica

# rappresento lo stesso grafico ma stavolta coloro anche per trattamento
plot(x= CO2$conc, y=CO2$uptake,
     pch=as.numeric(CO2$Type),
     col=CO2$Treatment)

type_labels <- unique(CO2$Type)
type_pch <- unique(CO2$Type)
treatment_labels <- unique(CO2$Treatment)
treatment_colors <- unique(CO2$Treatment)

# Aggiungi la legenda
legend("topleft",
       legend = c(type_labels, treatment_labels),
       pch = c(type_pch, rep(NA, length(treatment_labels))), 
       col = c(rep("black", length(type_labels)), treatment_colors), 
       lty = c(rep(NA, length(type_labels)), rep(1, length(treatment_labels))), 
       cex = 0.4) 

table(CO2$Type) 
table(CO2$Treatment) 

#triangoli rossi tutti in basso, triangoli neri a metà
#cerchi neri sono sempre i massimi, i cerchi rossi un po' a metà

#i cerchi neri sono quelli che prendono più CO2 a parità di conc.
#l'uptake dipende dalla conc di CO2 ma dipende anche dall'origine.
#organismi che rispondono meglio al trattamento sperimentale sono quelli
#sottoposti al trattamento nonchilled provenienti dal Quebec
#quelli che mostrano una meno chiara reazione a maggiori conc di CO2
#sono individui dal Mississippi chilled

# genero boxplot per uptake contro gruppi definti da type, treatment e conc
# e confronto i dati 

## 2. Utilizzare il modello lineare per verificare la relazione tra le variabili
# conc, Type e Treatment da sole riescono a spiegare i dati?
# Un modello con più variabili indipendenti spiega meglio il modello?

boxplot(data = CO2, uptake ~ conc)
boxplot(data = CO2, uptake ~ Type)
boxplot(data = CO2, uptake ~ Treatment)
boxplot(data = CO2, uptake ~ Type + Treatment)

boxplot(data = CO2, uptake ~ Type)
lmodel <- lm(data=CO2, uptake~Type)
summary(lmodel)
# spiega poco i dati solo il tipo

# vediamo come influisce la concentrazione su uptake 
min(CO2$uptake)
max(CO2$uptake)
table(CO2$conc)
boxplot(data=CO2[CO2$conc==95,], uptake~Type, ylim = c(0,50))
boxplot(data=CO2[CO2$conc==250,], uptake~Type, ylim = c(0,50))
boxplot(data=CO2[CO2$conc==500,], uptake~Type, ylim = c(0,50))
boxplot(data=CO2[CO2$conc==1000,], uptake~Type, ylim = c(0,50))

boxplot(data = CO2, uptake ~ conc)
lmodel2 <- lm(data=CO2, uptake~conc)
summary(lmodel2)
# spiega poco i dati solo la concentrazione

boxplot(data = CO2, uptake ~ Treatment)
lmodel3 <- lm(data=CO2, uptake~Treatment)
summary(lmodel3)
# spiega poco i dati solo il trattamento


# vediamo uptake vs treatment
boxplot(data=CO2, uptake~Treatment)
# vediamo anche qui influenza della conc
boxplot(data=CO2[CO2$conc==95,], uptake~Treatment, ylim = c(0,50))
boxplot(data=CO2[CO2$conc==250,], uptake~Treatment, ylim = c(0,50))
boxplot(data=CO2[CO2$conc==500,], uptake~Treatment, ylim = c(0,50))
boxplot(data=CO2[CO2$conc==1000,], uptake~Treatment, ylim = c(0,50))

# raggruppiamo i dati di due variabili
group = character(nrow(CO2))
group
group[which((CO2$Type=="Quebec")&
              (CO2$Treatment== "nonchilled"))] ="Q-N"
group
group[which((CO2$Type=="Quebec")&
              (CO2$Treatment=="chilled"))] = "Q-C"
group
group[which((CO2$Type=="Mississippi")&
              (CO2$Treatment=="chilled"))] = "M-C"
group
group[which((CO2$Type=="Mississippi")&
              (CO2$Treatment=="nonchilled"))]= "M-N"
group

# distribuzione dei valori dei 4 gruppi
table(group)
boxplot(data=CO2, CO2$uptake~group)
# vediamo una relazione lineare

# MODELLO LINEARE
# aggiungiamo tutte le variabili indipendenti
lCO2= lm(data=CO2,
         uptake~conc + Type + Treatment)
summary(lCO2)

boxplot(data = CO2, uptake~ conc + Type + Treatment)

# possiamo semplificare usando la colonna group
CO2$group = group # aggiungiamo la colonna group al dataframe
lCO2= lm(data=CO2, uptake~conc + group)
summary(lCO2)

boxplot(data = CO2, uptake ~ conc + group)

# analisi dei residui
res = lCO2$residuals
hist(res)
abline(v = mean(res), col = "red", lwd = 2)
#curva molto schiacciata e spostata a destra, centrata su 0 come media

plot(lCO2, which = 1)
plot(lCO2, which = 2)

# analisi dei valori fittati
fit <- predict(lCO2)
fit
# oppure...
fit = lCO2$fitted.values

#lo confronto con il dato vero per vedere come si comportano 
plot(x=CO2$uptake, y=fit)
# è abbastanza lineare
abline(0,1) # faccio una retta

# stessa curva ma colorata per gruppo
# group deve essere una variabile factor per usarla in col =
group=factor(group, levels=unique(group))
group
plot(x=CO2$uptake, y=fit,
     pch=19, col=as.numeric(group))

# guardiamo meglio i dati
library(visreg)
visreg(lCO2)
library(effects)
plot(effect(c("group", "conc"), lCO2))

## N.B. se non utilizzate il vettore group ma le tre variabili separate
# avreste visualizzato così la relazione tra le tre variabili:
lCO2= lm(data=CO2,
         uptake~conc + Type + Treatment)
summary(lCO2)
boxplot(data = CO2, uptake~ conc + Type + Treatment)

library(effects)
plot(effect(c("Type", "Treatment", "conc"), lCO2))

