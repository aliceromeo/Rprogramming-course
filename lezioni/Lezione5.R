## ESERCIZIO 3

#############################
## Di nuovo dataframe IRIS ##
#############################

plot(iris)

# guardiamo due variabili visivamente non correlate tra loro
plot(iris$Sepal.Length, iris$Sepal.Width)
plot(iris$Sepal.Width, iris$Sepal.Length)

# modello lineare semplice (una sola variabile indipendente)
lm_simple <- lm(Sepal.Length ~ Sepal.Width, data = iris)
summary(lm_simple) # slope quasi 0!

# controlliamo i residui
boxplot(lm_simple$residuals)
hist(lm_simple$residuals)
abline(v = mean(lm_simple$residuals), col = "red", lwd = 2)

# retta di regressione -> è quasi orizzontale!
plot(iris$Sepal.Width, iris$Sepal.Length, 
     col = iris$Species, pch = 19) # i punti sono colorati per specie
abline(lm_simple, col = "red", lwd = 2)

#################

# proviamo a fare un modello lineare multiplo (più variabili indipendenti)
# vediamo se miglioriamo la predizione di Sepal length considerando 
# l'iinfluenza di tutte e tre le altre variabili continue

lm_multi <- lm(Sepal.Length ~ Sepal.Width + Petal.Width + Petal.Length,
                    data = iris)
summary(lm_multi)

# confronto dei residui tra modello semplice e multiplo
boxplot(lm_simple$residuals, lm_multi$residuals, 
        names = c("Simple lm", "Multiple lm"))

# confronto tra gli R^2 (guardiamo l'adjusted R^2 per modelli multipli
# perchè è "aggiustato" per considerare la presenza di più variabili
# indipendenti)
summary(lm_simple)$adj.r.squared
summary(lm_multi)$adj.r.squared

# confrontiamo, per i due modelli, i risultati dei plot di tutti i
# parametri che possiamo valutare
# li guardiamo affiancati nella finestra grafica
par(mfrow = c(1,2))

# 1. RESIDUALS Vs FITTED
# asse x: valori predetti
# asse y: residui (valore reale - valore predetto)
# se la relazione è lineare i residui dovrebbero:
# 1. distribuzione dei residui: dovrebbero essere uniformemente distribuiti
# sopra e sotto lo zero
# 2. devono essere distribuiti randomicamente (non forme precise, tipo curve, imbuti...)
# 3. meno outlier possibili (indicati da label)
# 4. linea rossa: rappresenta la tendenza generale dei dati (più è 0, 
# più sono distribuiti senza una tendenza particolare)
# se non si osserva questo i dati dovrebbero essere trasformati (es. log), 
# oppure devono essere applicati altri modelli
plot(lm_simple, which = 1)
plot(lm_multi, which = 1)

# Q-Q RESIDUALS
# serve a valutare se i residui del modello seguono una distribuzione
# normale
# controllano se l'assunzione di normalità dei residui è verificata
# asse x: quantili teorici di una distribuzione normale standardizzata
# (es. quantile 0.5 è mediana, quantile 0.25 primo quartile ecc)
# asse y : quantili dei residui standardizzati del modello, che dividono
# i residui del modello in parti uguali

plot(lm_simple, which = 2)
plot(lm_multi, which = 2)

# SCALE-LOCATION
# come RESIDUALS Vs FITTED ma i residui sono sotto radice e in valore
# assoluto, migliore visualizzazione
plot(lm_simple, which = 3)
plot(lm_multi, which = 3)

# COOK'S DISTANCE
# Ci da informazioni sull'influenza che ogni singola osservazione ha
# sul modello di regressione nel suo complesso.
# Quantifica quanto tutti i valori previsti dal modello cambierebbero se
# una particolare osservazione venisse rimossa dal set di dati di
# addestramento.
# E' una funzione dei residui (l'errore tra i valori osservati e previsti)
# e della leva (leverage) dell'osservazione (quanto "estremo" è il 
# valore delle variabili indipendenti dell'osservazione).
# Le osservazioni con valori di distanza di Cook elevati sono quelle
# che, se rimosse, hanno un impatto maggiore sui coefficienti di 
# regressione.
# Le osservazioni più influenti sono indicate da label.
# Un numero eccessivo di osservazioni influenti può indicare che 
# il modello non è molto stabile, cioè che le stime dei coefficienti
# cambierebbero notevolmente se alcuni punti dati venissero esclusi
# (diminuisce la generalizzabilità del modello)
plot(lm_simple, which = 4)
plot(lm_multi, which =4)

# RESIDUALS Vs LEVERAGE
# Individua le osservazioni che hanno un'influenza significativa
# sul modello.
# asse x: leva (leverage), compresa tra 0 e 1. Misura quanto un'osservazione 
# è insolita oestrema nello spazio delle variabili indipendenti (predittori).
# Indica quanto i valori delle variabili indipendenti di un'osservazione
# si discostano dai valori medi delle variabili indipendenti.
# Una leva elevata significa che i valori dei predittori di 
# quell'osservazione sono distanti dalla media.
# asse y: residui standardizzati per migliorare il confronto tra 
# residui su scale diverse.
# Residui standardizzati elevati indicano che il modello non prevede
# bene il valore della variabile dipendente per quell'osservazione.
# Linea di soglia della distanza di cook (non si osserva sempre nei
# grafici): le osservazioni la cui distanza di Cook supera questa soglia
# sono considerate potenzialmente influenti e andrebbero indagate 
plot(lm_simple, which = 5)
plot(lm_multi, which = 5)

# COOK'S DISTANCE Vs LEVERAGE
# Identificare i punti dati che hanno un'influenza significativa
# sul modello di regressione.
# asse x: leverage. misura l'influenza di una osservazione sui valori predetti.
# i punti con leverage elevata tirano la retta verso di sè
# asse y: distanza di cook. misura quanto cambierebbero i valori predetti
# se una certa osservazione venisse rimossa.
# Ogni cerchio rappresenta una osservazione. I punti che cadono sulle 
# linee tratteggiate hanno una combinazione alta di leva e distanza di cook, 
# quindi sono i più influenti e sono etichettati.
# La linea rossa mostra la tendenza generale per i punti con alta leva 
plot(lm_simple, which = 6)
plot(lm_multi, which = 6)

par(mfrow = c(1,1))

# pacchetti utili per la visualizzazione
# install.packages("visreg")
library(visreg)
visreg(lm_simple)
visreg(lm_multi) # dare INVIO sulla console
# per visualizzare i tre grafici insieme (uno per ogni variabile indipendente):
par(mfrow = c(1,3))
visreg(lm_multi)
par(mfrow = c(1,1))

# install.packages("effects")
# per visualizzare relazioni che includono variabili indipendenti multiple
library(effects)
# le linee in basso indicano la densità dei punti.
# Nel modello semplice guardiamo l'effetto di Sepal.Width su Sepal.Length
# La variabile dipendente non va specificata esplicitamente, solo quella
# indipendente
plot(effect("Sepal.Width", lm_simple))

# Nel modello multiplo possiamo guarda l'effetto delle singole variabili
# indipendenti sulla variabile dipendente:
# > effetto di Sepal.Width su Sepal.Length
plot(effect("Sepal.Width", lm_multi))
# > effetto di Petal.Width su Sepal.Length
plot(effect("Petal.Width", lm_multi))
# < effetto di Petal.Length su Sepal.Length
plot(effect("Petal.Length", lm_multi))

# Oppure l'effetto combinato di più variabili indipendenti sulla variabile
# dipendente.
# In questo caso ogni pannello rappresenta un valore specifico di una 
# delle variabili predittive, mentre i valori dell'altra sono rappresentati
# sull'asse x. 
# L'asse Y rappresenta i valori predetti della variabile di risposta 
# (Sepal.Width).
# Cosa guardare:
# 1. Effetto di Petal.Width: come varia Sepal.Width all'aumentare di 
# Petal.Width. La linea è inclinata verso l'alto, quindi 
# all'aumentare di Petal.Width, anche Sepal.Width aumenta. 
# 2. Effetto di Petal.Length: l'altezza delle linee tra i diversi 
# pannelli. Se le linee in pannelli diversi sono a livelli di Y diversi,
# significa che Petal.Length ha un effetto su Sepal.Width. In questo caso
# Sepal.Width aumenta se Petal.Length aumenta.
# 3. Intervalli di Confidenza: La larghezza dell'area ombreggiata 
# indica l'incertezza nella previsione. Aree più ampie significano 
# maggiore incertezza.

# Grazie all'analisi dei risultati del modello lineare multiplo sappiamo
# che:
# > SL e PL : direttamente proporzionali
# > SL e PW : inversamente proporzionali
# > SL e SW : direttamente proporzionali
lm_multi$coefficients
summary(lm_multi)
summary(lm_multi)$coefficients

# Andiamo a vedere graficamente come più aumenta petal length 
# più aumenta sepal Length (altezza della retta rispetto alla y), 
# mentre petal width è inversamente proporzionale a sepal length (pendenza
# negativa della retta)
plot(effect(c("Petal.Width","Petal.Length"), lm_multi))

# similmente ma al contrario:
plot(effect(c("Petal.Length","Petal.Width"), lm_multi))

# altre comparazioni
# vediamo come cambia la dispersione dei valori (aumentano gli intervalli
# di confidenza) a seconda della combinazione dei valori delle due 
# variabili indipendenti
plot(effect(c("Sepal.Width","Petal.Width"), lm_multi))
plot(effect(c("Sepal.Width","Petal.Length"), lm_multi))
plot(effect(c("Petal.Width","Sepal.Width"), lm_multi))






