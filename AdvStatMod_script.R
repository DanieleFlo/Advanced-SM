
rm(list = ls())

#### Importazione dati ####
load("DataRegression2025_unical.RData")
riso = data.frame(RiceFarms)
riso$id = as.factor(riso$id)
dim(riso)
head(riso)
str(riso)
summary(riso)

#### Statistiche descrittive ####
library(dplyr)
cbind(min = apply(select_if(riso, is.numeric),MARGIN=2,min,na.rm=T),
      max = apply(select_if(riso, is.numeric),MARGIN=2,max,na.rm=T),
      mean = apply(select_if(riso, is.numeric),MARGIN=2,mean,na.rm=T),
      median = apply(select_if(riso, is.numeric),MARGIN=2,median,na.rm=T),
      sd = apply(select_if(riso, is.numeric),MARGIN=2,sd,na.rm=T))

arrange(as.data.frame(cbind(freq_ass = table(riso$region),
                            freq_perc = round(prop.table(table(riso$region)),4)*100)),
        desc(freq_ass))
arrange(as.data.frame(cbind(freq_ass = table(riso$status),
                            freq_perc = round(prop.table(table(riso$status)),4)*100)),
        desc(freq_ass))
arrange(as.data.frame(cbind(freq_ass = table(riso$varieties),
                            freq_perc = round(prop.table(table(riso$varieties)),4)*100)),
        desc(freq_ass))
arrange(as.data.frame(cbind(freq_ass = table(riso$bimas),
                            freq_perc = round(prop.table(table(riso$bimas)),4)*100)),
        desc(freq_ass))

riso %>% group_by(region) %>% summarise(min=min(price), max=max(price), mean=mean(price), median=median(price), sd=sd(price))
riso %>% group_by(status) %>% summarise(min=min(price), max=max(price), mean=mean(price), median=median(price), sd=sd(price))
riso %>% group_by(varieties) %>% summarise(min=min(price), max=max(price), mean=mean(price), median=median(price), sd=sd(price))
riso %>% group_by(bimas) %>% summarise(min=min(price), max=max(price), mean=mean(price), median=median(price), sd=sd(price))

riso %>% group_by(region) %>% summarise(min=min(wage), max=max(wage), mean=mean(wage), median=median(wage), sd=sd(wage))
riso %>% group_by(region) %>% summarise(min=min(pseed), max=max(pseed), mean=mean(pseed), median=median(pseed), sd=sd(pseed))

#table(riso$id,riso$region)
riso %>% group_by(region) %>% summarise(n=n_distinct(id))


regioni <- unique(riso$region)

# Cicliamo sulle regioni
for (reg in regioni) {
  cat("Analisi per la regione:", reg, "\n")
  
  # Sottoinsieme dei dati per la regione corrente
  dati_regione <- subset(riso, region == reg)
  
  # Calcolo delle statistiche
  stats <- dati_regione %>% 
    group_by(varieties) %>% 
    summarise(
      min    = min(price, na.rm = TRUE),
      max    = max(price, na.rm = TRUE),
      mean   = mean(price, na.rm = TRUE),
      median = median(price, na.rm = TRUE),
      sd     = sd(price, na.rm = TRUE)
    )
  
  # Visualizzazione dei risultati
  print(stats)
  cat("------------\n")
}


ggplot(riso, aes(x = varieties, y = price, fill = varieties)) +
  geom_boxplot() +
  facet_wrap(~ region, scales = "free") +  # Facet per regione, scales="free" permette ad ogni grafico di avere le proprie scale
  labs(title = "Boxplot dei Prezzi per Varietà per Regione",
       x = "Varietà", y = "Prezzo") +
  theme_minimal() +
  theme(legend.position = "none")   # Rimuove la legenda per semplificare il grafico

# Ripristina il layout grafico di default
par(mfrow = c(1, 1))


#### Correlazione ####
library(ggcorrplot)
matrix_corrplot = round(cor(select_if(riso, is.numeric), method="pearson"),4)
matrix_corrplot
ggcorrplot(matrix_corrplot, hc.order=T, type="lower", lab=T)
rm(matrix_corrplot)

#### Istogramma e densità di kernel di price ####
library(ggplot2)
library(gridExtra)
hist = ggplot(riso, aes(x=price)) +
         geom_histogram(fill="lightblue", color="white", bins=15) +
         theme_minimal() +
         labs(title="Istogramma di price",
              subtitle="Numero di bins = 15")
kernel = ggplot(riso, aes(x=price)) +
           geom_density(fill="lightblue", bw=bw.nrd0(riso$price)) +
           theme_minimal() +
           labs(title="Densità di kernel di price",
                subtitle="Stimata con parametro di smoothing ottimale")
grid.arrange(hist, kernel, nrow=1)
rm(hist, kernel)

#### Istogramma e densità di kernel di log(price) ####
hist = ggplot(riso, aes(x=log(price))) +
         geom_histogram(fill="lightblue", color="white", bins=15) +
         theme_minimal() +
         labs(title="Istogramma di log(price)",
              subtitle="Numero di bins = 15")
kernel = ggplot(riso, aes(x=log(price))) +
           geom_density(fill="lightblue", bw=bw.nrd0(log(riso$price))) +
           theme_minimal() +
           labs(title="Densità di kernel di log(price)",
                subtitle="Stimata con parametro di smoothing ottimale")
grid.arrange(hist, kernel, nrow=1)
rm(hist, kernel)

#### Boxplot di log(price) per le variabili categoriche ####
box1 = ggplot(riso, aes(x=region, y=log(price))) +
         geom_boxplot(fill="lightblue", alpha=0.5, notch=F) +
         theme_minimal()
box2 = ggplot(riso, aes(x=status, y=log(price))) +
         geom_boxplot(fill="lightblue", alpha=0.5, notch=F) +
         theme_minimal()
box3 = ggplot(riso, aes(x=varieties, y=log(price))) +
         geom_boxplot(fill="lightblue", alpha=0.5, notch=F) +
         theme_minimal()
box4 = ggplot(riso, aes(x=bimas, y=log(price))) +
         geom_boxplot(fill="lightblue", alpha=0.5, notch=F) +
         theme_minimal()
grid.arrange(box1, box2, box3, box4, nrow=2, ncol=2)
rm(box1, box2, box3, box4)

#### Scatterplot price e wage ####
scatter1 = ggplot(riso, aes(x=wage, y=log(price), group=id)) +
  geom_point(size=0.7, col="lightgrey", alpha=0.7) +
  geom_smooth(method=lm, se=F, col="darkgrey", size=0.5, alpha=0.7) +
  theme_minimal() +
  labs(title="Relazione lineare tra price e wage per le 171 fattorie")
scatter2 = ggplot(riso, aes(x=wage, y=log(price), group=id, color=region)) +
  geom_point(size=0.7, alpha=0.7) +
  geom_smooth(method=lm, se=F, size=0.5, alpha=0.7) +
  theme_minimal() +
  labs(title="Relazione lineare tra price e wage per le 171 fattorie",
       subtitle="Con differenti colori per le 6 regioni")
scatter3 = ggplot(riso, aes(x=wage, y=log(price), color=region)) +
  geom_point(size=0.5, alpha=0.7) +
  geom_smooth(method=lm, se=F, size=0.5, alpha=0.7) +
  theme_minimal() +
  labs(title="Relazione lineare tra price e wage per le 6 regioni")
grid.arrange(scatter1, scatter2, scatter3, nrow=2, ncol=2)
rm(scatter1, scatter2, scatter3)
# Dai grafici si nota che le rette delle fattorie hanno intercette e pendenze diverse,
# però le fattorie all'interno della stessa regione presentano intercette e pendenze simili.
# Pertanto sembrerebbe che la differenza è più tra le regione che tra le fattorie.

#### Scatterplot price e pseed ####
scatter1 = ggplot(riso, aes(x=pseed, y=log(price), group=id)) +
  geom_point(size=0.7, col="lightgrey", alpha=0.7) +
  geom_smooth(method=lm, se=F, col="darkgrey", size=0.5, alpha=0.7) +
  theme_minimal() +
  labs(title="Relazione lineare tra price e pseed per le 171 fattorie")
scatter2 = ggplot(riso, aes(x=pseed, y=log(price), group=id, color=region)) +
  geom_point(size=0.7, alpha=0.7) +
  geom_smooth(method=lm, se=F, size=0.5, alpha=0.7) +
  theme_minimal() +
  labs(title="Relazione lineare tra price e pseed per le 171 fattorie",
       subtitle="Con differenti colori per le 6 regioni")
scatter3 = ggplot(riso, aes(x=pseed, y=log(price), color=region)) +
  geom_point(size=0.5, alpha=0.7) +
  geom_smooth(method=lm, se=F, size=0.5, alpha=0.7) +
  theme_minimal() +
  labs(title="Relazione lineare tra price e pseed per le 6 regioni")
grid.arrange(scatter1, scatter2, scatter3, nrow=2, ncol=2)
rm(scatter1, scatter2, scatter3)

#### Modello ad effetti casuali ####
riso$logprice = log(riso$price)

# Con solo intercetta
library(lme4)
mod_int1 = lmer(logprice~1+(1|id), data=riso) #dà problemi
summary(mod_int1)
mod_int2 = lmer(logprice~1+(1|region), data=riso)
summary(mod_int2)
mod_int3 = lmer(logprice~1+(1|id)+(1|region), data=riso) #dà problemi
summary(mod_int3)
library(lmerTest)
ranova(mod_int2)
rm(mod_int1, mod_int2, mod_int3)

# Con variabile wage
mod_wage1 = lmer(logprice~1+wage+(1|id), data=riso)
summary(mod_wage1)
ranova(mod_wage1)
mod_wage2 = lmer(logprice~1+wage+(1|region), data=riso)
summary(mod_wage2)
ranova(mod_wage2)
mod_wage3 = lmer(logprice~1+wage+(1|id)+(1|region), data=riso)
summary(mod_wage3) #dà problemi
rm(mod_wage1, mod_wage2, mod_wage3)

# Con più variabili esplicative
mod_compl1 = lmer(logprice~1+wage+pseed+purea+varieties+bimas+(1|id), data=riso)
summary(mod_compl1)
ranova(mod_compl1)
mod_compl2 = lmer(logprice~1+wage+pseed+purea+varieties+bimas+(1|region), data=riso)
summary(mod_compl2)
ranova(mod_compl2)
mod_compl3 = lmer(logprice~1+wage+pseed+purea+bimas+(1|id)+(1|region), data=riso) #dà problemi
summary(mod_compl3)
ranova(mod_compl3)
mod_compl2_pc = lmer(logprice~1+wage+pseed+purea+varieties+bimas+(1+wage|region), data=riso)
summary(mod_compl2_pc)
ranova(mod_compl2_pc)
rm(mod_compl1, mod_compl3, mod_compl2_pc)

# Analisi residui (modello "mod_compl2")
residui = data.frame(res=residuals(mod_compl2))
shapiro.test(residui$res)
ggplot(residui, aes(x=res)) +
  geom_histogram(fill="lightblue", color="white", bins=30) +
  theme_minimal() +
  labs(title="Istogramma dei residui",
       x="residui")
qqnorm(residui$res)
qqline(residui$res, col="red")
rm(residui)
