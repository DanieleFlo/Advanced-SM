library(ggplot2)
library(dplyr)
library(gamlss)
library(lme4)


#ANALISI DEL DATASET PRELIMINARE
Y=data.frame(RiceFarms)
str(Y)
summary(Y)
View(Y)
which(is.na(Y)) #non ci sono missing value.
names(Y)
unique(Y$pphosph)
table(Y$price)
#id: identificativo univoco dell'azienda agricola.


#CARATTERISTICHE DEL TERRENO: SUDDIVISE IN ETTARI COLTIVATI E PROPRIETA' DEL TERRENO

#size: area totale coltivata a riso (in ettari).
#status: stato della terra coltivata, che può essere:
#owner: agricoltori proprietari o affittuari (non mezzadri).
#share: mezzadri.
#mixed: combinazione delle due categorie precedenti.


Y$varieties
#varieties: tipo di varietà di riso coltivate:
#trad: varietà tradizionali.
#high: varietà ad alta resa.
#mixed: combinazione delle due varietà.
#bimas: partecipazione al programma di intensificazione BIMAS:
#no: non partecipante.
#yes: partecipante.
#mixed: solo una parte del terreno è registrata nel programma.

#FATTORI DI INPUT PRODUTTIVI: COSTO DELLE MATERIE PRIME E TIPOLOGIA

#seed: quantità di semi utilizzati (kg).
#urea: quantità di urea utilizzata (kg).
#phosphate: quantità di fosfato utilizzata (kg).
#pesticide: costo dei pesticidi (in Rupiah).
#pseed: prezzo del seme (in Rupiah per kg).
#purea: prezzo dell'urea (in Rupiah per kg).
#pphosph: prezzo del fosfato (in Rupiah per kg).


#INPUT: COSTI DEL PERSONALE E ORE DI LAVORO
#hiredlabor: ore di lavoro salariato.
#famlabor: ore di lavoro familiare.
#totlabor: totale ore di lavoro (escludendo il raccolto).
#wage: salario della manodopera (in Rupiah per ora).

#PRODUZIONE LORDA E NETTA 
#goutput: produzione lorda di riso (kg).
#noutput: produzione netta di riso, calcolata sottraendo il costo del raccolto dalla produzione lorda.
#price: prezzo del riso grezzo (in Rupiah per kg).


#AREE GEOGRAFICHE IN CUI OPERANO LE AZIENDE:
#region: area geografica di appartenenza dell’azienda agricola, tra:
#wargabinangun, langan, gunungwangi, malausma, sukaambit, ciwangi.


#Analisi della variabile target

#test di normalità
ggplot(Y, aes(x = price)) +
  geom_histogram(binwidth = 10, fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribuzione dei prezzi")
sort(unique(Y$price))





#distribuzione del prezzo per regione
ggplot(Y, aes(region,price,fill =region)) +
  geom_boxplot( fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribuzione dei prezzi")


#distribuzione del prezzo per regione
aziende_per_regione <- Y %>%
  group_by(region) %>%
  summarise(
    n_aziende = n_distinct(id),
    prezzo_medio = mean(price),
    prezzo_sd = sd(price)
  )
aziende_per_regione

#distribuzione del salario per regione:
ggplot(Y, aes(region,wage,fill =region)) +
  geom_boxplot( fill = "red", color = "black") +
  theme_minimal() +
  labs(title = "Distribuzione del salario per regione")



mod_ga<- gamlss(price ~ 1+ pseed + purea + wage+ (1|id), family = GA, data = Y)
summary(mod_ga)
res=residuals(mod_ga)


ggplot(data.frame(res), aes(x = res)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Istogramma dei residui", x = "Residui", y = "Frequenza")

par(mfrow=c(1,2))
histDist(res)

qqnorm(res)
qqline(res, col = "red")

shapiro.test(res)
 




