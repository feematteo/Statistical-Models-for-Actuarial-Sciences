#visualizzo il dataset
str(ds)

#verifico se ci sono osservazioni = 0 visto che non comportano info utile
summary(ds$ExposTotal)

#convertendo i valori in scala giornaliera
ds$ExposTotal <- ds$ExposTotal/365

#elimino le esposizioni pari a 0
ds <- subset(ds, ExposTotal != 0)

#verifico se l'ooservazione minima è != 0
summary(ds$ExposTotal)   

nrow(ds)

summary(ds$VehYear)

summary(ds$SumInsAvg)
# discretizziamo la variabile VehYear utilizzando il criterio dei quartili
ds$VehYear <- as.numeric(ds$DrivAge)

ds$VehYear <- cut(ds$VehYear, 
                       breaks = quantile(ds$VehYear, probs = seq(0, 1, 0.25), na.rm = TRUE), labels = c("< 2003","2003-2007","2007-2009",">2009"),
                       
                       include.lowest = TRUE)
levels(ds$VehYear)

# rimuovo na
ds<-na.omit(ds)
summary(ds)


library(MASS)

#convertiamo le colonne 'ClaimNb' e 'ClaimAmount' del dataset in interi
ds$ClaimNb <- as.integer(ds$ClaimNb)
ds$ClaimAmount <- as.integer(ds$ClaimAmount)

#otteniamo questa struttura del dataset
str(ds)

levels(ds$Gender)
levels(ds$DrivAge)
head(levels(ds$VehModel))
summary(ds$DrivAge)

#Istogramma della variabile 'ClaimAmount' per valori maggiori di 0
hist(ds$ClaimAmount[ds$ClaimAmount>0], breaks = 20,main =  "Istogramma della variabile 'ClaimAmount'",
     xlab = "ammontare dei claims",xlim = c(0,200000) )

#numero totale di sinistri, l'esposizione totale per riuscire a trovare la frequenza dei sinistri

TotaleSx <- sum(ds$ClaimNb)
TotaleExposure <- sum(ds$ExposTotal)
Freq <- TotaleSx / TotaleExposure #correzione
Freq

#aggregato il numero di sinistri per area
aggregate(ClaimNb ~  Area, data=ds, FUN=sum)

# tabelle per il numero di sinistri e per l'esposizione totale per area, 
# attraverso tabFreq -> calcolato la frequenza dei sinistri per area

tabClaimNb <- aggregate( ClaimNb ~  Area, data=ds, FUN=sum)
tabExposure <- aggregate( ExposTotal ~  Area, data=ds, FUN=sum)
tabFreq <- tabClaimNb$ClaimNb/tabExposure$ExposTotal
tabFreq

#Creazione delle celle tariffarie aggregando numero di sinistri ed esposizione totale per area celle:
celle <- aggregate( cbind(ClaimNb, ExposTotal) ~  Area, data=ds, FUN=sum)

#verifico presenza NA
na_positions <- which(is.na(celle))
if (length(na_positions) > 0) {
  print(paste("NA found at positions:", toString(na_positions)))
} else {
  print("No NA values found.")
}

celle$Freq <- celle$ClaimNb / celle$ExposTotal
celle


#analisi di regressione sul numero di risarcimenti e quindi, sul numero di sinistri, 
#utilizzando le variabili Gender+DrivAge+State

model_1 <- glm(ClaimNb ~ Gender+DrivAge+State+offset(log(ExposTotal)), family = poisson(link="log"),
                        data = ds, subset=ExposTotal>0, x = TRUE)
summary(model_1)

#Sivariabile Gender è poco significativa
# altro modello senza var Gender
model_2 <- glm(ClaimNb ~  DrivAge+State+offset(log(ExposTotal)), family = poisson(link="log"),
                          data = ds, subset=ExposTotal>0, x = TRUE)
summary(model_2)
#regressione stepwise utilizzando il criterio di selezione AIC
model_step <- step(model_2, direction = "both")
summary(model_step)


# Definizione del modello iniziale con tutte le variabili per ClaimAmount
ds_1 <- subset(ds, ds$ClaimNb != 0)
model_3 <- glm(ClaimAmount ~  DrivAge+State+offset(log(ClaimNb)), family =Gamma(link='log'),
                          data = ds_1, x = TRUE)
summary(model_3)
#variabile State poco significativa
modello_step_2 <- step(model_3, direction = "both")

summary(modello_step_2)

# per le celle scegliamo come variabili DrivAge + State. 
# creiamo la colonna frequenza nelle celle tariffarie come rapporto tra ClaimNb ed ExposTotal

celle <- aggregate(cbind(ClaimNb, ExposTotal, ClaimAmount) ~  DrivAge+State, data=ds, FUN = sum)
celle$Frequenza <- celle$ClaimNb / celle$ExposTotal
celle
ds$Frequenza <- ds$ClaimNb / ds$ExposTotal

#media e la varianza del numero di sinistri:
media_sinistri <- sum(celle$ClaimNb)/sum(celle$ExposTotal)
varianza_sinistri <- sum((celle$ClaimNb - media_sinistri*celle$ExposTotal)^2)/sum(celle$ExposTotal)
cat("average =",media_sinistri," variance =",varianza_sinistri,"phi =",varianza_sinistri/media_sinistri,"\n")

