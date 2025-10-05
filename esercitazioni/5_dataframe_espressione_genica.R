################################
# Dataframe espressione genica #
################################

# carico il dataframe da un file
df <- read.csv(file = "df_espressione_genica.csv")

# Esploro il dataframe
head(df)
tail(df)
str(df)

# Trasformare la colonna Gene e la colonna Replicate 
# in factor per facilitare le analisi successive
df$Gene <- as.factor(df$Gene)
df$Replicate <- as.factor(df$Replicate)
str(df)
summary(df)

boxplot(data = df, Expression ~ Concentration)
boxplot(data = df, Expression ~ Replicate)
boxplot(data = df, Expression ~ Concentration + Replicate)
boxplot(data = df, Expression ~ Gene)
boxplot(data = df, Expression ~ Gene + Concentration)

plot(df$Concentration, df$Expression, col = df$Gene, 
     pch = as.numeric(df$Replicate))

# calcolare medie di espressione nelle tre repliche, per ogni
# concentrazione e poi graficare i risultati ottenuti

group = character(nrow(df))
group

group[which((df$Gene=="GeneA")&
              (df$Concentration== 0))] ="A0"
group[which((df$Gene=="GeneA")&
              (df$Concentration== 0.5))] ="A05"
group[which((df$Gene=="GeneA")&
              (df$Concentration== 1))] ="A1"

group[which((df$Gene=="GeneB")&
              (df$Concentration== 0))] ="B0"
group[which((df$Gene=="GeneB")&
              (df$Concentration== 0.5))] ="B05"
group[which((df$Gene=="GeneB")&
              (df$Concentration== 1))] ="B1"

group[which((df$Gene=="GeneC")&
              (df$Concentration== 0))] ="C0"
group[which((df$Gene=="GeneC")&
              (df$Concentration== 0.5))] ="C05"
group[which((df$Gene=="GeneC")&
              (df$Concentration== 1))] ="C1"
group

## Alternativa...
# (ma andrebbero cambiati i nomi dei gruppi nelle righe di codice successive, 
# quindi non la usiamo in questo esempio)
#group <- character(nrow(df))

#for (gene in df$Gene) {
#  for (conc in df$Concentration) {
#    indici <- which(df$Gene == gene & df$Concentration == conc)
#    if (length(indici) > 0) {
#      group[indici] <- paste0(gene, conc)
#    }
#  }
#}
#group


table(group)
table(group, df$Gene)

df$group <- group
boxplot(data=df, Expression ~ group)

meanexpr.A0 <- mean(df$Expression[which(df$group == "A0")])
meanexpr.A0
meanexpr.A05 <- mean(df$Expression[which(df$group == "A05")])
meanexpr.A05
meanexpr.A1 <- mean(df$Expression[which(df$group == "A1")])
meanexpr.A1

meanexpr.B0 <- mean(df$Expression[which(df$group == "B0")])
meanexpr.B0
meanexpr.B05 <- mean(df$Expression[which(df$group == "B05")])
meanexpr.B05
meanexpr.B1 <- mean(df$Expression[which(df$group == "B1")])
meanexpr.B1

meanexpr.C0 <- mean(df$Expression[which(df$group == "C0")])
meanexpr.C0
meanexpr.C05 <- mean(df$Expression[which(df$group == "C05")])
meanexpr.C05
meanexpr.C1 <- mean(df$Expression[which(df$group == "C1")])
meanexpr.C1

par(mfrow = c(1,3))
barplot(c(meanexpr.A0, meanexpr.A05, meanexpr.A1), 
        names = c(0,0.5,1), main = "Gene A", 
        ylim = c(0,15))
barplot(c(meanexpr.B0, meanexpr.B05, meanexpr.B1), 
        names = c(0,0.5,1), main = "Gene B", 
        ylim = c(0,15))
barplot(c(meanexpr.C0, meanexpr.C05, meanexpr.C1), 
        names = c(0,0.5,1), main = "Gene C", 
        ylim = c(0,15))

# calcolare differenze medie tra espressione a conc 0 e 1
# per ogni gene
meandiff.A <- mean(df$Expression[which(df$group == "A1")] - 
  df$Expression[which(df$group == "A0")])
meandiff.A
meandiff.B <- mean(df$Expression[which(df$group == "B1")] - 
                     df$Expression[which(df$group == "B0")])
meandiff.B
meandiff.C <- mean(df$Expression[which(df$group == "C1")] - 
                     df$Expression[which(df$group == "C0")])
meandiff.C

differenze <- c(meandiff.A, meandiff.B, meandiff.C)

meandiff.A
meandiff.B
meandiff.C

par(mfrow = c(1,1))
# rappresentarle graficamente
barplot(differenze, 
        names = c("A","B","C"), main = "Differenze")

# calcolare, separatamente per ogni gene, 
# se esiste una relazione lineare tra espressione e 
# concentrazione

# GeneA
df_geneA <- df[which(df$Gene == "GeneA"), ]
model_A <- lm(Expression ~ Concentration,
              data = df_geneA)
summary(model_A)

meandiff.A

plot(df_geneA$Concentration, df_geneA$Expression,
     col = df_geneA$Replicate, pch = 19)
abline(model_A, col = "blue", lty = 2)

# GeneB
df_geneB <- df[which(df$Gene == "GeneB"), ]
model_B <- lm(Expression ~ Concentration, 
              data = df_geneB)
summary(model_B)
meandiff.B
plot(df_geneB$Concentration, df_geneB$Expression,
     col = df_geneB$Replicate, pch = 19)
abline(model_B, col = "blue", lty = 2)

# GeneC
df_geneC <- df[which(df$Gene == "GeneC"), ]
model_C <- lm(Expression ~ Concentration, 
              data = df_geneC)
summary(model_C)
meandiff.C

plot(df_geneC$Concentration, df_geneC$Expression,
     col = df_geneC$Replicate, pch = 19)
abline(model_C, col = "blue", lty = 2)

boxplot(model_A$residuals, model_B$residuals,
        model_C$residuals, ylim = c(-1,1))
abline(h = mean(model_A$residuals), col = "red", lwd = 2)
abline(h = mean(model_B$residuals), col = "blue", lwd = 2)
abline(h = mean(model_C$residuals), col = "green", lwd = 2)

library(visreg)
visreg(model_A)
visreg(model_B)
visreg(model_C)

library(effects)
plot(effect("Concentration", model_A))
