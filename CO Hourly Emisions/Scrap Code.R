## TILDE = ~ ##

# Imports

library(readr)
library(magrittr)
library(dplyr)
library(xts)
library(lubridate)
library(forecast)
library(urca)
library(readxl)
library(KFAS)
library(xts)
library(splines)

# retrive the dataset
 
setwd("C:/Users/lucam/Desktop/Uni/Streaming Data Management and Time Series Analysis/Progetto")
df <- read_csv("Project_data_2021_2022 (TRAINSET).csv", col_types = cols(Date = col_character()))
df<- as_tibble(df)
oldpar <- par() # salvo parametri in caso di cambio succesivo

#### MISSING VALUES HANDLING ###

length(df$CO) # 8526 valori
missing <- df$CO %>% is.na %>% which() # missing values
missing %>% length() # 365 missing values



# Dati iniziano da mercoledì 10/03/2004 e finiscono lunedì 28/02/2005

# Gestisco i missing values facendo la media per il giorno della settimana, data la natura della serie

date <- as.Date(df$Date) # metto in versione data
df$Date <- date
date_months <- months(date) %>% match(month.name) # estraggo mese dai dati
df$Month <- date_months # creo colonna nel ds solo mesi
df$Weekday <- weekdays(date) # Trovo e assegno il giorno della settimana

df_missing <- df %>% slice(missing) # focus on missing values

medie <- df %>% na.omit() %>% group_by(Month, Weekday, Hour) %>% summarise_at(vars(CO), list(CO=mean)) # Creo la media per i mesi ed i giorni della settimana

df_missing$CO <- NULL # drop colonna CO con valori nulli
df_missing <- df_missing %>% left_join(medie, by = c('Month', 'Weekday', 'Hour')) # unisco con df medie e trovo valori in media

merged_df <- df %>% full_join(df_missing, by = c('Date', 'Hour', 'Month', 'Weekday')) # unisco i df con le medie e quello originale

df <- within(
  merged_df,{
    CO <- ifelse(is.na(CO.x), CO.y, CO.x) # creo nuova colonna mettendo i valori in media al posto dei missing values
    CO.x <- NULL # droppo colonne della join che non servono più
    CO.y <- NULL
  }
)
df$CO %>% is.na %>% which() # rimosso i valori nulli

y = df$CO # assegno per comodità

rm(df_missing) # rimuovo df non più necessari
rm(medie)
rm(merged_df)

### ARIMA MODELS ###

plot(y, type='l') # plot della serie per prima analisi

df$Hour<-sprintf("%02d:00",df$Hour) # cambio in formato ora corretto
df$Hour<-format(df$Hour, format= "%H:%M")
df$DT<-ymd_hm(paste(df$Date, df$Hour)) # metto anno e ora assieme

data_ts <- xts(df$CO, df$DT) #formato serie storica
plot(data_ts)

##  Non stazionarietà in Varianza

dvalues <- df %>% group_by(Date) %>% summarise_at(vars(CO), list(Mean=mean)) # medie giornaliere
plot(dvalues$Mean, type='l', main='Media Giornaliera')
dstd <- df %>% group_by(Date) %>% summarise_at(vars(CO), list(Std=sd)) # Deviazioni standard giornaliere
dvalues <- dvalues %>% left_join(dstd, by = c('Date'))


plot(dvalues$Mean, dvalues$Std, type = 'p', xlab='Mean', ylab='Std', main = 'Mean e Std Correlation') 
abline(lm(dvalues$Std ~ dvalues$Mean), col='red') # grafico evoluzione nel tempo

log_y <- log(data_ts)
plot(log_y)

lambda_y <- forecast::BoxCox(data_ts, lambda= "auto")
attributes(lambda_y) # valore di lambda applicato  -0.8999268
plot(lambda_y)
# per invertire la BoxCox:

# data_ts <- forecast::InvBoxCox(lambda_y, lambda= -0.8999268)

write.csv(df,"df.csv", row.names = FALSE) # save to csv

urt <- urca::ur.df(lambda_y)
summary(urt)

# non sazionarietà in media e stagionalità

# Stagionalità da indagare: mensile (diff 28/30/31), settimanale (diff 7 giorni = 168h)

# Costruisco due ARMA
# 1. trend + modello stagionalità settimanale con differenza (d7y) e giornaliera con sinusoidi + festivi
# 2. trend + modello stagionalità settimanale con le dummies e la giornaliera con diff(24) + festivi
# 3. provo autoarima

# Prova trend normale e quadratico

t <- 1:length(y)
t2 <- t^2

# dummy settimanali

Monday <- ifelse(df$Weekday == 'Monday', 1, 0)
Tuesday <- ifelse(df$Weekday == 'Tuesday', 1, 0)
Wednesday <- ifelse(df$Weekday == 'Wednesday', 1, 0)
Friday <- ifelse(df$Weekday == 'Friday', 1, 0)
Saturday <- ifelse(df$Weekday == 'Saturday', 1, 0)
Sunday <- ifelse(df$Weekday == 'Sunday', 1, 0)

ddummies <- cbind(Monday,Tuesday,Wednesday,Friday,Saturday,Sunday)

# dummy festività

liberazione <- ifelse(df$Date == "2004-04-25", 1, 0)
lavoro <- ifelse(df$Date == "2004-05-01", 1, 0)
repubblica <- ifelse(df$Date == "2004-06-02", 1, 0)
capodanno <- ifelse(df$Date == "2005-01-01", 1, 0)
befana <- ifelse(df$Date == "2005-01-06", 1, 0)
pasqua_1 <- ifelse(df$Date == "2004-04-12", 1, 0)
pasqua_2 <- ifelse(df$Date == "2004-04-13", 1, 0)
ferragosto <- ifelse(df$Date == "2004-08-15", 1, 0)
ognisanti <- ifelse(df$Date == "2004-11-01", 1, 0)
immacolata <- ifelse(df$Date == "2004-12-08", 1, 0)
natale <- ifelse(df$Date == "2004-12-25", 1, 0)
sstefano <- ifelse(df$Date == "2004-12-26", 1, 0)

fdummies <- cbind(liberazione,
                  lavoro,
                  repubblica,
                  capodanno,
                  befana,
                  pasqua_1,
                  pasqua_2,
                  ferragosto,
                  ognisanti,
                  immacolata,
                  natale,
                  sstefano)

# NB: p-values solo indicativi

dreg <- lm(lambda_y ~ t + t2 + cs + si) # daily regression con trend, dummies, coseni e seni
summary(dreg) # c'é trend singolo e quadratico, # No Tue/Wed/Fri/lavoro/capodanno/befana/pasqua2/immacolata/sstefano

ddummies1 <- cbind(Monday,Saturday,Sunday)
fdummies1 <- cbind(liberazione,
                  repubblica,
                  pasqua_1,
                  ferragosto,
                  ognisanti,
                  natale
                  )

dreg1 <- lm(lambda_y ~ t + t2 + ddummies1 + fdummies1)
summary(dreg1)

# sinusoidi giornaliere

omega <- outer(1:length(lambda_y), 1:6) * 2 * pi / 24 # creo frequenze ogni giorno
cc <- cos(omega) # prima sei basate sulla regressione, poi 3 basate sul modello arima. Risultati ugualli
ss <- sin(omega)

dreg2 <- lm(lambda_y ~ t + t2 + ddummies1 + cc + ss ) # 0.38 R^2 con solo la componente deterministica
summary(dreg2)

level <- dreg2$coefficients[1] +
  dreg2$coefficients[2]*t +
  dreg2$coefficients[3]*t2
level<-level - mean(level)+mean(lambda_y)

y_day <- cc %*% dreg2$coefficients[7:12] +
  ss %*% dreg2$coefficients[13:18]

wseas <- ddummies1 %*% dreg2$coefficients[4:6] # creo stagionalità nei 7 giorni
wseas0 <- wseas - mean(wseas[1:24])
wseas0 <- xts(level+wseas0, df$DT)
y_day <- xts(level+y_day, df$DT)

plot(lambda_y, type='l')
lines(wseas0, col = "green", lwd = 2)
lines(y_day, col = "red", lwd = 2)


plot(wseas0[1:168], type='l') # cattura della stagionalità giornaliera

# sinusoidi settimanali (20) (no dummies) 

omega1 <- outer(1:length(lambda_y), 1:16) * 2 * pi / 168 # creo frequenze ogni settimana
cc1 <- cos(omega1)
ss1 <- sin(omega1)

dreg3 <- lm(lambda_y ~ t + t2 + fdummies1 + cc1 + ss1) # 0.38 R^2 con solo la componente deterministica
summary(dreg3) # 0.41 R^2 con solo la componente deterministica

y_sett <- cc1 %*% dreg3$coefficients[10:29] +
  ss1 %*% dreg3$coefficients[30:49]

level <- dreg3$coefficients[1] +
  dreg3$coefficients[2]*t +
  dreg3$coefficients[3]*t2
level<-level - mean(level)+mean(lambda_y)

y_sett <- xts(level+y_sett, df$DT)

plot(lambda_y, type='l') # cattura della stagionalità settimanale
lines(y_sett, col = "red", lwd = 2)


# sinusoidi settimanali (35)

omega2 <- outer(1:length(lambda_y), 1:12) * 2 * pi / 24 # creo frequenze ogni giorno
cc2 <- cos(omega2)
ss2 <- sin(omega2)

dreg4 <- lm(lambda_y ~ t + t2 + fdummies1 + cc2 + ss2 ) # 0.38 R^2 con solo la componente deterministica
summary(dreg4) # 0.41 R^2 con solo la componente deterministica

y_sett <- cc2 %*% dreg4$coefficients[10:21] +
  ss2 %*% dreg4$coefficients[22:33]

level <- dreg4$coefficients[1] +
  dreg4$coefficients[2]*t +
  dreg4$coefficients[3]*t2

level<-level - mean(level)+mean(lambda_y)
y_sett <- xts(level+y_sett, df$DT)


plot(lambda_y, type='l') # cattura della stagionalità settimanale
lines(y_sett, col = "red", lwd = 2)

#######################################################################################
layout(matrix(c(1,2), 2, 1))
#######################################################################################

# Modello senza xreg migliore

mod1 <- Arima(data_ts, c(2, 0, 2),
              list(order=c(1, 1, 1), period =24), 
              include.constant = FALSE,
              lambda = 'auto',
              biasadj = TRUE) # aggiungendo ddummies1 non cambia molto


mod1 #AICc=-129659.2

Acf(mod1$residuals, 24*14)
Pacf(mod1$residuals, 24*14)

# Modello stagionalità settimanale ss1-cc1

mod2 <- Arima(data_ts, c(3, 0, 3), 
              list(order = c(3, 0, 2), period = 24), 
              lambda = 'auto',
              biasadj = TRUE,
              xreg = cbind(cc1,ss1)) 

mod2 #AICc=-130516.1    #cc2,ss2 ==> AICc=-130771.1

Acf(mod2$residuals, 24*60) 
Pacf(mod2$residuals, 24*60)

plot(mod2$residuals)

# Modello stagionalità settimanale dummies  

mod3 <- Arima(data_ts, c(1, 0, 1), 
                list(order = c(1, 0, 1), period = 24), 
                include.constant = TRUE,
                lambda = 'auto',
                biasadj = TRUE,
                xreg = cbind(ddummies)) # giorno

mod3 #AICc=-129847.9

Acf(mod3$residuals, 24*14) 
Pacf(mod3$residuals, 24*14)

# modello stagionalita sett dummies e giornaliera cc-ss 

mod4 <- Arima(data_ts, order=c(2, 0, 2), 
              include.constant = TRUE,
              lambda = 'auto',
              biasadj = TRUE,
              xreg = cbind(ddummies, cc,ss)) 

mod4 #AICc=-129022.1 

Acf(mod4$residuals, 24*14) 
Pacf(mod4$residuals, 24*14)

# modello solo la stagionalità giornaliera con cc-ss

mod5 <- Arima(data_ts, c(2, 0, 1), # differenziazione prima fa esplodere le previsioni
              list(order = c(1, 0, 1), period = 24),
              lambda = 'auto',
              biasadj = TRUE,
              xreg = cbind(cc,ss))# inverno

mod5 #AICc=-129927.7

Acf(mod5$residuals, 24*60) 
Pacf(mod5$residuals, 24*60)


# FACCIO PREVISIONI SULL'ULTIMO MESE COME PROVA

layout(matrix(c(1,1), 1, 1)) #====>>> questo

mod5 <- Arima(y[1:7854], c(3, 0, 3), 
              list(order = c(5, 0, 3), period = 24),
              include.mean=TRUE,
              lambda = 'auto',
              biasadj = TRUE,
              xreg = cbind(cc,ss)[1:7854,])

pre1 <- forecast(mod5, 672, xreg = cbind(cc, ss)[7854:8526, ]) # 672 è un mese da 28 giorni (febbraio)

plot(pre1, 346)
lines(7854:8526,y[7854:8526], col = "red", type='l') # Aggiungo la serie storica reale di confronto

plot(pre1)

####


# USO MAPE COME ERRORE

err1  <- window(y, start = 7854) - pre1$mean
rmse1 <- err1^2 %>% mean() %>% sqrt()
mape1 <- mean(abs(err1)/window(y, start = 7854)*100) # 11.60179

err2  <- window(y, start = 7854) - pre2$mean
rmse2 <- err2^2 %>% mean() %>% sqrt()
mape2 <- mean(abs(err2)/window(y, start = 7854)*100) # 57.19999

# FACCIO PREVISIONI PERIODO PER ESAME

mod_fin <- Arima(y[1:7854], c(2, 0, 1), 
              list(order = c(1, 0, 1), period = 24),
              include.mean=TRUE,
              lambda = 'auto',
              biasadj = TRUE,
              xreg = cbind(cc,ss)[1:7854])

pre <- forecast(mod_fin, 672, xreg = cbind(cc, ss)[7854:8526, ]) # 744 è un mese da 31 giorni (Marzo)

plot(pre, 10000)

plot(pre1)

err  <- window(y, start = 7854) - pre$mean
rmse <- err^2 %>% mean() %>% sqrt()
mape <- mean(abs(err)/window(y, start = 7854)*100) # 11.74091
#############################################################

mod1_t <- Arima(y[1:7854], c(2, 0, 2),
              list(order=c(1, 1, 1), period =24), 
              include.constant = FALSE,
              lambda = 'auto',
              biasadj = TRUE)

pre1_t <- forecast(mod1_t, 672) # 672 è un mese da 28 giorni (febbraio)

plot(pre1_t, 346)
lines(7854:8526,y[7854:8526], col = "red", type='l') 


err1_t  <- window(y, start = 7854) - pre1_t$mean
rmse1_t <- err1_t^2 %>% mean() %>% sqrt()
mape1_t <- mean(abs(err1_t)/window(y, start = 7854)*100) # 12.184

giorno <- ifelse(df$Hour == "10:00"| 
                   df$Hour == "11:00"|
                   df$Hour == "12:00"|
                   df$Hour == "13:00"|
                   df$Hour == "14:00"|
                   df$Hour == "15:00"|
                   df$Hour == "16:00"|
                   df$Hour == "17:00"|
                   df$Hour == "18:00"|
                   df$Hour == "19:00"|
                   df$Hour == "20:00"|
                   df$Hour == "09:00",
                 1, 0)

notte  <- ifelse(df$Hour == "21:00"| 
                   df$Hour == "22:00"|
                   df$Hour == "23:00"|
                   df$Hour == "00:00"|
                   df$Hour == "01:00"|
                   df$Hour == "02:00"|
                   df$Hour == "03:00"|
                   df$Hour == "04:00"|
                   df$Hour == "05:00"|
                   df$Hour == "06:00"|
                   df$Hour == "07:00"|
                   df$Hour == "08:00",
                 1, 0)

ciclo_giornaliero <- cbind(giorno, notte)


inverno <- ifelse(df$Month == 12|
                    df$Month == 1|
                    df$Month == 2|
                    df$Month == 3|
                    df$Month == 11,
                  1, 0)


mod <- Arima(data_ts, c(2, 0, 2), 
             list(order = c(3, 0, 3), period = 24),
             include.mean=TRUE,
             lambda = 'auto',
             biasadj = TRUE,
             xreg = cbind(cc1,ss1))

  layout(matrix(c(1,1), 1, 1))

mod_test <- Arima(y[1:7854], c(2, 0, 2), 
                  list(order = c(3, 0, 3), period = 24),
                  include.mean=TRUE,
                  lambda = 'auto',
                  biasadj = TRUE,
                  xreg = cbind(cc1,ss1)[1:7854,])

pre_test <- forecast(mod_test, 672, xreg = cbind(cc1, ss1)[7854:8526, ]) # 672 è un mese da 28 giorni (febbraio)

s<-plot(pre_test, 346, main='Previsioni vs Valori reali')
lines(7854:8526,y[7854:8526], col = "red", type='l')

err_test  <- window(y, start = 7854) - pre_test$mean
rmse_test <- err_test^2 %>% mean() %>% sqrt()
mape_test <- mean(abs(err_test)/window(y, start = 7854)*100) # 10.97875

omega_pre <- outer(1:(length(lambda_y)+744), 1:20) * 2 * pi / 168 # creo frequenze ogni settimana
cc_pre <- cos(omega_pre)
ss_pre <- sin(omega_pre)
pre <- forecast(mod, 744, xreg = cbind(cc_pre, ss_pre)[8527:9270, ]) # previsioni 744 giorni di marzo

plot(pre, 346,  main='Previsioni per Marzo')
ARIMA = pre$mean


Date <-seq(as.Date("2005-03-01"), as.Date("2005-03-31"), by = "day") %>% rep(times=24) %>% sort(Date, decreasing = FALSE)
Hour <- seq(0 , 23 , by = 1) %>% rep(times=31)
previsioni <- data.frame(Date, Hour, ARIMA)

write.csv(previsioni,"866654_20220613.csv", row.names = FALSE) # save to csv

omega_pre <- outer(1:(length(lambda_y)+744), 1:16) * 2 * pi / 168 # creo frequenze ogni settimana
cc_pre <- cos(omega_pre)
ss_pre <- sin(omega_pre)
pre <- forecast(mod, 744, xreg = cbind(cc_pre, ss_pre)[8527:9270, ]) # previsioni 744 giorni di Marzo
plot(pre, main='Previsioni per Marzo')

 ################################################################################################################################
################################# nel caso metto (3,0,2) nel periodo stagionale ################################################
################################################################################################################################

# Modelli UCM #

data1 <-data_ts
data1[7854:8500] <- NA

# 1) Quello con sinusoidi inserite
# 2) Quello con sinusoidi calcolate
# definisco le funzioni di perdita 

losses <- function(y, yhat) {
  aerr <- abs(y - yhat)
  c(MAE = mean(aerr),
    MAPE = mean(aerr/y)*100,
    RMSE = sqrt(mean(aerr^2)))
}


mod1 <- SSModel(data1 ~ 0 +
                  SSMtrend(degree = 1, Q=NA) + # primo numero è il grado del polinomio da usare
                  SSMseasonal(168, NA, "trig", harmonics = 1:16),
                data = df,
                H = NA
)

vary <- var(data1, na.rm = TRUE)

mod1$a1["level", ] <- data_ts[1] # posso metto la media come valore iniziale del livello, ha senso
diag(mod1$P1inf) <- 0 # metto a zero questo, ora il modello guarda quello che c'é sulla P1
diag(mod1$P1) <- vary


par_init <- c(
  log_var_eta = log(vary/100), # alto-basso retta
  log_var_zeta = log(vary/1000), # coefficiente angolare
  log_var_omega <- log(vary/1000), # stagionalità
  log_var_eps <- log(vary/10) # errori osservazione
)

updt1 <- function(pars, model) {
  model$Q[1, 1, 1] <- exp(pars[1]) # le matrici sono messe come array 3d
  #model$Q[2, 2, 1] <- exp(pars[2])
  diag(model$Q[2:33, 2:33, 1]) <- exp(pars[3]) # Tutti questi sono NA che devono essere sostituiti dalla stessa var che sta nella 3 posizione
  model$H[1, 1, 1] <- exp(pars[4])
  model
}

fit1 <- fitSSM(mod1, par_init, updt1)
cat("Codice di convergenza =", fit1$optim.out$convergence)

smo1 <- KFS(fit1$model,
            filtering = c("state","signal"), # per produrre le previsioni della serie storica quando trova i valori mancanti
            smoothing = c("state", "disturbance", "signal"))

plot(y, type='l')
lines(ts(smo1$alphahat[,'level']), col = "red")
#lines(ts(rowSums(smo1$alphahat[, seq(1, 2)])), col = "red")
plot(ts(smo1$m), col = "red")

seas365 <- rowSums(smo1$alphahat[, seq(2, 33, 2)]) # devo sommare tutte le componente armoniche (dalla 8 alla 39, solo i numeri pari) = step 2
plot(ts(seas365[1:720]), col='blue')

plot(ts(smo1$m))


######################################################


# modello con coefficienti presi dall'ARIMA e sinusoidi a parte

freq <- outer(1:nrow(lambda_y), 1:16)*2*pi/168 # Frequenze delle 16 sinusoidi con
# periodicità base di un anno
cs   <- cos(freq)                     # Coseni
colnames(cs) <- paste("cos", 1:16)
si   <- sin(freq)                     # Seni
colnames(si) <- paste("sin", 1:16)


mod2 <- SSModel(lambda_y~cs+si+SSMtrend(2, list(NA, NA)), H = NA)

#L'algoritmo per condizioni iniziali diffuse ha problemi di convergenza
# quindi diamo condizioni iniziali informative
 # mod
cfs <- coefficients(mod)[12:43]                     # I coefficienti delle dummy delle ferie
                                                   # e delle sinusoidi
mod2$a1[1:32] <- cfs                               # vengono usati come valori medi
                                                   # iniziali dei rispettivi coeff.
mod2$a1[33:34]   <- coefficients(mod)[11] # intercetta modello ARIMA                          # La prima osservazione viene usata
                                                   # per il livello iniziale
mod2$P1inf <- matrix(0, 34, 34)                    # Eliminiamo le distribuzioni diffuse
# Per le varianze iniziali usiamo le varianze degli stimatori di regressione
diag(mod2$P1[1:32, 1:32]) <- diag(mod$var.coef[12:43, 12:43])
diag(mod2$P1[33:34, 33:34]) <- var(lambda_y)  # Usiamo la varianza della serie per il livello

par_init <- c(
  # alto-basso retta
  log_var_zeta = log(vary/1000),
  log_var_omega <- log(vary/100), # stagionalità
  log_var_eps <- log(vary/10),
  log_var_slope = log(vary/1000)# errori osservazione
)

updt1 <- function(pars, model) {
  model$Q[1, 1, 1] <- exp(pars[1]) # le matrici sono messe come array 3d
  diag(model$Q[3:34, 3:34, 1]) <- exp(pars[2]) # Tutti questi sono NA che devono essere sostituiti dalla stessa var che sta nella 3 posizione
  model$H[1, 1, 1] <- exp(pars[3])
  model$Q[2, 2, 1] <- exp(pars[4])
  model
}

# Stima del modello (abbiamo solo tre varianze da stimare)
fit2 <- fitSSM(mod2, rep(7, 3)) # i tre NA
cat("Codice di convergenza =", fit2$optim.out$convergence)



smo2 <- KFS(fit2$model,
            filtering = c("state","signal"), # per produrre le previsioni della serie storica quando trova i valori mancanti
            smoothing = c("state", "disturbance", "signal"))


plot(log(y), type='l')
plot(ts(smo2$alphahat[, 'level']), col = "red")

seas365 <- rowSums(smo2$alphahat[, seq(1, 32)])
plot(ts(seas365), col='blue')

plot(ts(smo2$m))

# RIVEDO COEFFICIENTI CON 16 SINUSOIDI

##################################################################
##################################################################
##################################################################



previsioni <- read_csv("866654_20220613.csv", col_types = cols(Date = col_character()))

# SETTIMNALE (GIUSTO)

#previsioni di prova

data_test <- data_ts
data_test[7854:8526] <- NA

# previsioni mese compito

data_pre <- rep(NA, length(data_ts)+745)
data_pre[1:8526] <- data_ts
data_pre[8527] <- 900

losses <- function(y, yhat) {
  aerr <- abs(y - yhat)
  c(MAE = mean(aerr),
    MAPE = mean(aerr/y)*100,
    RMSE = sqrt(mean(aerr^2)))
}


mod_test <- SSModel(data_test ~ 0 +
                  SSMtrend(degree = 2, list(NA, NA)) + # LLT
                  SSMseasonal(168, NA, "trig", harmonics = 1:16),
                  data = df,
                  H = NA
)


vary <- var(data_test, na.rm = TRUE)
cfs <- coefficients(mod)[12:43]  # Coefficienti modello ARIMA                   

mod_test$a1["level", ] <- coefficients(mod)[11] # Intercetta modello ARIMA 
mod_test$a1[3:34] <- cfs
diag(mod_test$P1inf) <- 0
diag(mod_test$P1[1:2, 1:2]) <- vary
diag(mod_test$P1[3:34, 3:34]) <-  vary #diag(mod$var.coef[12:43, 12:43])

par_init <- c(
  log_var_zeta = log(vary/1000),
  log_var_omega <- log(vary/100),
  log_var_eps <- log(vary/10),
  log_var_slope = log(vary/1000)
)


updt1 <- function(pars, model) {
  model$Q[1, 1, 1] <- exp(pars[1]) 
  diag(model$Q[3:34, 3:34, 1]) <- exp(pars[2])
  model$H[1, 1, 1] <- exp(pars[3])
  model$Q[2, 2, 1] <- exp(pars[4])
  model
}


fit_test <- fitSSM(mod_test, par_init, updt1)

smo_test <- KFS(fit_test$model,
            filtering = c("state","signal"),
            smoothing = c("state", "disturbance", "signal"))


a <- plot(y, type='l')
lines(7854:8526, ts(smo_test$m)[7854:8526], col= 'purple')

b<-plot(y[7854:8526], type='l')
lines(ts(smo_test$m)[7854:8526], col= 'purple')

sel <- 7854:8526
loss <- losses(y[sel], smo_test$m[sel,])
loss # 11.85

### previsioni

mod_pre <- SSModel(data_pre ~ 0 +
                  SSMtrend(degree = 2, list(NA, NA)) + # LLT
                  SSMseasonal(168, NA, "trig", harmonics = 1:16),
                data = df,
                H = NA
)


vary <- var(data_pre, na.rm = TRUE)
cfs <- coefficients(mod)[12:43]  # Coefficienti modello ARIMA                   

mod_pre$a1["level", ] <- coefficients(mod)[11] # Intercetta modello ARIMA 
mod_pre$a1[3:34] <- cfs
diag(mod_pre$P1inf) <- 0
diag(mod_pre$P1[1:2, 1:2]) <- vary
diag(mod_pre$P1[3:34, 3:34]) <- vary


fit_pre<- fitSSM(mod_pre, par_init, updt1)

smo_pre <- KFS(fit_pre$model,
                filtering = c("state","signal"),
                smoothing = c("state", "disturbance", "signal"))

plot(data_pre, type='l')
lines(8528:9271,smo_pre$m[8528:9271], col= 'green', type='l')

UCM <- smo_pre$m[8528:9271]
previsioni <- cbind(previsioni, UCM)

##################################################################
##################################################################
##################################################################

# GIORNALIERO 

data1 <-data_ts
data1[7854:8526] <- NA

# 1) Quello con sinusoidi inserite
# 2) Quello con sinusoidi calcolate
# definisco le funzioni di perdita 

losses <- function(y, yhat) {
  aerr <- abs(y - yhat)
  c(MAE = mean(aerr),
    MAPE = mean(aerr/y)*100,
    RMSE = sqrt(mean(aerr^2)))
}


mod3 <- SSModel(data1 ~ 0 +
                  SSMtrend(degree = 2, list(NA,NA)) +
                  SSMseasonal(168, NA, "trig", harmonics = 1:16),
                  SSMseasonal(24, 0, "dummy"),
                  data = df,
                  H = NA
)

vary <- var(data1, na.rm = TRUE)

mod3$a1["level", ] <- mean(data1[7:31]) 
diag(mod3$P1inf) <- 0 # metto a zero questo, ora il modello guarda quello che c'é sulla P1
diag(mod3$P1) <- vary


par_init <- c(
  
  log_var_zeta = log(vary/1000), # coefficiente angolare
  log_var_omega <- log(vary/100), # stagionalità
  log_var_eps <- log(vary/10),
  log_var_slope = log(vary/1000)# errori osservazione
)

updt1 <- function(pars, model) {
  model$Q[1, 1, 1] <- exp(pars[1])
  model$Q[2, 2, 1] <- exp(pars[4])# le matrici sono messe come array 3d
  diag(model$Q[3:14, 3:14, 1]) <- exp(pars[2]) # Tutti questi sono NA che devono essere sostituiti dalla stessa var che sta nella 3 posizione
  model$H[1, 1, 1] <- exp(pars[3])
  model
}

fit3 <- fitSSM(mod3, par_init, updt1)
cat("Codice di convergenza =", fit_test$optim.out$convergence)

smo3 <- KFS(fit3$model,
            filtering = c("state","signal"), # per produrre le previsioni della serie storica quando trova i valori mancanti
            smoothing = c("state", "disturbance", "signal"))

plot(y, type='l')
lines(ts(smo3$alphahat[,'level']), col = "red")

seas365 <- rowSums(smo_test$alphahat[, seq(3, 33, 2)]) # devo sommare tutte le componente armoniche (dalla 8 alla 39, solo i numeri pari) = step 2
plot(ts(seas365[1:720]), col='blue')
plot(ts(seas365), col='blue')

plot(y, type='l')
lines(ts(smo3$m), col= 'purple')

sel <- 7854:8526
loss <- losses(data_ts[sel], smo3$m[sel,])
loss #12.44


# SETTIMNALE 

data1 <-data_ts
y[7854:8526] <- NA

# 1) Quello con sinusoidi inserite
# 2) Quello con sinusoidi calcolate
# definisco le funzioni di perdita 

losses <- function(y, yhat) {
  aerr <- abs(y - yhat)
  c(MAE = mean(aerr),
    MAPE = mean(aerr/y)*100,
    RMSE = sqrt(mean(aerr^2)))
}


mod_prova <- SSModel(y ~ ddummies1 + fdummies1+
                  SSMtrend(degree = 2, list(NA,NA)) +
                  SSMseasonal(24, NA, "trig", harmonics = 1:6),
                data = df,
                H = NA
)

vary <- var(data1, na.rm = TRUE)

mod_prova$a1["level", ] <- mean(data1[7:175]) 
diag(mod_prova$P1inf) <- 0 # metto a zero questo, ora il modello guarda quello che c'é sulla P1
diag(mod_prova$P1) <- vary


par_init <- c(
  log_var_zeta = log(vary/1000),
  log_var_omega <- log(vary/100),
  log_var_eps <- log(vary/10),
  log_var_slope = log(vary/1000)
)

updt1 <- function(pars, model) {
  model$Q[1, 1, 1] <- exp(pars[1]) # le matrici sono messe come array 3d
  diag(model$Q[3:14, 3:14, 1]) <- exp(pars[2]) # Tutti questi sono NA che devono essere sostituiti dalla stessa var che sta nella 3 posizione
  model$H[1, 1, 1] <- exp(pars[3])
  model$Q[2, 2, 1] <- exp(pars[4])
  model
}

fit_prova <- fitSSM(mod_prova, par_init, updt1)
cat("Codice di convergenza =", fit1$optim.out$convergence)

smo_prova <- KFS(fit_prova$model,
            filtering = c("state","signal"), # per produrre le previsioni della serie storica quando trova i valori mancanti
            smoothing = c("state", "disturbance", "signal"))

plot(y, type='l')
lines(ts(smo_prova$alphahat[,'level']), col = "red")

seas365 <- rowSums(smo_prova$alphahat[, seq(6, 17, 2)]) # devo sommare tutte le componente armoniche (dalla 8 alla 39, solo i numeri pari) = step 2
plot(ts(seas365[1:720]), col='blue')
plot(ts(seas365), col='blue')

plot(y, type='l')
lines(ts(smo_prova$m), col= 'purple')

plot(ts(smo_prova$m), col= 'green')

sel <- 7854:8526
loss <- losses(data_ts[sel], smo_prova$m[sel,])
loss # 12.66

###########################################################

y<- df$CO
datalags <- 10

x.train <- array(data = lag( y, datalags)[-(1:datalags)], # tutti tranne i primi 10
                 dim = c(nrow(df) - datalags, datalags, 1))
y.train <- array(data = y[-(1:datalags)], # toglie le prime 10 che sono NA
                 dim = c(nrow(df)-datalags, 1))

x.test <- array(data = lag( test$price, datalags)[-(1:datalags)],
                dim = c(nrow(test) - datalags, datalags, 1))
y.test <- array(data = test$price[-(1:datalags)],
                dim = c(nrow(test) - datalags, 1))
