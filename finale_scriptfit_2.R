mod_st3 <- gamlss(price ~ 1 + pseed + noutput  +  seed+ pphosph + urea+  varieties +  bimas, random = ~1 | region,family = ST3, data = data_2)
summary(mod_st3)
plot(mod_st3)
summary(fitted(mod_st3))
#il modello è troppo eteroschedastico 

summary(data_2$price)


#Modello Lognormale
mod_logno <- gamlss(price ~ 1 + pseed + noutput  +  seed+ pphosph + urea+  varieties + bimas , random = ~1 | region,family = LOGNO, data = data_2)
summary(mod_logno)
plot(mod_logno)
summary(fitted(mod_logno))

#Modello Gamma
mod_gam <- gamlss(price ~ 1 + pseed + noutput  +  seed+ pphosph + urea+ varieties + bimas, random = ~1 | region,family = GA, data = data_2)
plot(mod_ga)
summary(fitted(mod_ga))
summary(data_2$price)

#modello Box-Cox

mod_BCT <- gamlss(price ~ 1 + pseed + noutput  +  seed+ pphosph + urea+  varieties + bimas, random = ~1 | region,family = BCT, data = data_2)
plot(mod_BCT)
summary(fitted(mod_BCT))
summary(data_2$price)

#sembra che il modello lognormale sia il migliore sia perchè fitta meglio in termini di qqplot eteroschedasticità ecc
#sia perchè sembra il modello migliore considerando AIC e BIC.
AIC(mod_ga, mod_logno, mod_st3, mod_BCT)
BIC(mod_ga, mod_logno, mod_st3)


library(lme4)
#fit lognormale con quadratura gaussiana 
mod_gamma_glmer <- glmer(
  price ~ pseed + noutput + seed + pphosph + urea + varieties + bimas + (1 | region),
  family = Gamma(link = "log"),  # Gamma con link log è una scelta comune
  data = data_2,
  nAGQ = 7
)

plot(mod_gamma_glmer)
