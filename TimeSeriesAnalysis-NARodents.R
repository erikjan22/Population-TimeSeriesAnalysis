# R script to analyse Time Series data for two rodent species
# Data source: https://ecologicaldata.org/wiki/powdermill-biological-station-small-mammal-database
# Created in June 2019 


# CTRL + L clears the console
rm(list=ls()) # Empty the global environment

# You can set the path of your directory here
setwd("C:/Users/Erik Jan/Documents/Time Series  Assignment/Data set")

library(tidyverse)
library(dplyr)

### LOADING DATA

# R Code to read the raw csv file and name all the columns. Instruction from website.
powdermill_data <-read.csv('database.txt', header=F, skip=21,sep=",",quote='"',
                  col.names=c("animal_id","is_new","species_code", "period", "time",
                  "date", "quadrat", "sex", "sex_unknown", "weight", "sex_shrew",
                  "scrotal_male", "inguinal_male", "pregnant", "open_vulva",
                  "large_nipples"), 
                  check.names=TRUE, stringsAsFactors = FALSE)

# R code for a data frame of the species names to match the codes. Instruction from website.
powdermill_species = read.table(header=TRUE, sep=',', 
                              stringsAsFactors = FALSE, text='
            species_code, species_name
            BB,Blarina brevicauda
            CG,Clethrionomys gapperi
            DV,Didelphis virginiana
            GV,Glaucomys volans
            MF,Mustela frenata
            MM,Marmota monax 
            NI,Neozapus insignis 
            NM,Neotoma magister
            PL,Peromyscus leucopus
            PM,Peromyscus maniculatus
            SC,Sorex cinereus
            SD,Sorex dispar
            SF,Sorex fumeus
            SH,Sorex hoyi
            TS,Tamias striatus' )

# Reveal the structure of the dataframe
str(powdermill_data)


### MANIPULATING DATA ###

# First we need to split the data information into year, month and day. Right now these are combined into one integer value.
powdermill_data$year  <- as.numeric(substr(as.character(powdermill_data$date), 0, 4))
powdermill_data$month <- as.numeric(substr(as.character(powdermill_data$date), 5, 6))
powdermill_data$day   <- as.numeric(substr(as.character(powdermill_data$date), 7, 8))

# We can remove the data where the species_type was not observed correctly.
# In these cases species_code is either "" or "?". This means that we can exclude them by only selected those cases with length of species_code == 2
powdermill_data <- subset(powdermill_data, nchar(species_code) == 2)

# Select the data we're interested in and ignore the remaining columns
MyData <- powdermill_data[c("species_code", "month", "year")]
MyData$count = 1

# Our problem now is that we need to include a count 0 for those species that have not been observed at all in particular months
years <- c(1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 
          1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999)
months <- c(1:12)
species <- c("BB", "CG", "DV", "GV","MF", "MM","NI", "NM", "PL", "PM","SC", "SD", "SF", "SH", "TS")

# Create a new dataframe which we can fill up with data
NewData <- data.frame(species_code=character(), month=numeric(), year = numeric(), count = numeric(), stringsAsFactors = FALSE)

counter = 1

for (y in years) {
  for (m in months) {
    for (s in species) {
      NewData[counter,] <- c(s, m, y, 0)
      counter = counter + 1
    }
  }
}

NewData$month = as.numeric(NewData$month)
NewData$year = as.numeric(NewData$year)
NewData$count = as.numeric(NewData$count)

MyData <- rbind(MyData, NewData)

# Next, we need to add all the counts for species together, for specific months and years
FullData = 
  MyData %>%
  group_by(year, month, species_code)  %>%
  summarize(totalCount = sum(count))

# Trapping in 1979 started in September, so 1979 is not a full year
months = c(1:9)
for (i in months) {
  FullData = subset(FullData, year != 1979 | month != i)
}

# Trapping in 1999 ended in October, so 1999 is not a full year
months = c(11,12)
for (i in months) {
  FullData = subset(FullData, year != 1999 | month != i)
}

# We can keep the first ten months of 1999 as test data and keep the rest as training data
TestData <- subset(FullData, year == 1999)
TrainData <- subset(FullData, year != 1979 & year != 1999)

rm(NewData, MyData, counter, y, s, m, months, years, species)


### INSPECT SPECIES DATA ###

# Now we have a dataframe on which we can start analyses. Let's select the species with the most observations.

SpeciesTotalCount = 
  FullData %>%
  group_by(species_code)  %>%
  summarize(totalCount = sum(totalCount))

# The two species with the highest counts are: PM and TS


### SELECT FIRST SPECIES ###

# Select the data of the right species
# Species 1: PM (Peromyscus maniculatus) is the North American Deer Mouse
FullDataPM = subset(FullData, species_code == "PM")
TrainDataPM = subset(TrainData, species_code == "PM")
TestDataPM = subset(TestData, species_code == "PM")

par(mfrow=c(1,1))
plot(x = c(1:nrow(FullDataPM)), y = FullDataPM$totalCount, type = 'l', col="blue",
     xlab = 'time', ylab = 'Number of observations', main = 'Observations of Deer Mouse over time')

# Show the (partial) autocorrelation over lags for the training data
par(mfrow=c(2,1))
acf(TrainDataPM$totalCount,main='Autocorrelation for Deer Mouse data')
pacf(TrainDataPM$totalCount,main='Partial autocorrelation for Deer Mouse data')

# Transformation: do a differentiation by 1
TransFullDataPM = diff(FullDataPM$totalCount, differences=1)
TransTrainDataPM = diff(TrainDataPM$totalCount, differences=1)

par(mfrow=c(1,1)) 
plot(x = c(1:length(TransFullDataPM)), y = TransFullDataPM, type = 'l', col="blue",
     xlab = 'time (months)', ylab = 'Number of observations', main = 'Differentiation by 1 for Deer Mouse data')

par(mfrow=c(2,1))
acf(TransTrainDataPM, main='Autocorrelation for diff(1) Deer Mouse data')
pacf(TransTrainDataPM, main='Partial autocorrelation for diff(1) Deer Mouse data')


### FITTING ARIMA MODELS ###

# Order: (p, d, q). Stands for order in AR mode, ... and order in MA model
AR1 = arima(TrainDataPM$totalCount,order=c(1,1,0))
AR3 = arima(TrainDataPM$totalCount,order=c(3,1,0))
AR10 = arima(TrainDataPM$totalCount,order=c(10,1,0))

MA1 = arima(TrainDataPM$totalCount,order=c(0,1,1))
MA2 = arima(TrainDataPM$totalCount,order=c(0,1,2))
MA3 = arima(TrainDataPM$totalCount,order=c(0,1,3))
MA6 = arima(TrainDataPM$totalCount,order=c(0,1,6))
MA7 = arima(TrainDataPM$totalCount,order=c(0,1,7))

# All possible arma models
ARMA1_1 = arima(TrainDataPM$totalCount,order=c(1 ,1,1))
ARMA3_1 = arima(TrainDataPM$totalCount,order=c(3 ,1,1))
ARMA10_1 = arima(TrainDataPM$totalCount,order=c(10,1,1))
ARMA1_2 = arima(TrainDataPM$totalCount,order=c(1 ,1,2))
ARMA3_2 = arima(TrainDataPM$totalCount,order=c(3 ,1,2))
ARMA10_2 = arima(TrainDataPM$totalCount,order=c(10,1,2))
ARMA1_3 = arima(TrainDataPM$totalCount,order=c(1 ,1,3))
ARMA3_3 = arima(TrainDataPM$totalCount,order=c(3 ,1,3))
ARMA10_3 = arima(TrainDataPM$totalCount,order=c(10,1,3))
ARMA1_6 = arima(TrainDataPM$totalCount,order=c(1 ,1,6))
ARMA3_6 = arima(TrainDataPM$totalCount,order=c(3 ,1,6))
ARMA10_6 = arima(TrainDataPM$totalCount,order=c(10,1,6))
ARMA1_7 = arima(TrainDataPM$totalCount,order=c(1 ,1,7))
ARMA3_7 = arima(TrainDataPM$totalCount,order=c(3 ,1,7))
ARMA10_7 = arima(TrainDataPM$totalCount,order=c(10,1,7))

# Let's select one of the models and check the mean square prediction error (MSPE)
# Inspection of results reveals that MA3 model is the preferred choice
Model = MA3

fore=predict(Model, n.ahead=10)
MSPE = mean((fore$pred-TestDataPM$totalCount)^2)
MSPE  # Show the mean square prediction error to the user

# Residual diagnostics
par(mfrow=c(2,1))
hist(Model$res, main='Histogram of residuals. MA(3) model, Deer Mouse data, diff(1)')
qqnorm(Model$res, main='QQ-plot of residuals. MA(3) model, Deer Mouse data, diff(1)')
par(mfrow=c(1,1))
tsdiag(Model)

# Clean up the global environment
rm(Model, AR1, AR3, AR10, MA1, MA2, MA3, MA6, MA7, ARMA1_1, ARMA3_1, ARMA10_1, ARMA1_2, ARMA3_2, ARMA10_2, 
   ARMA1_3, ARMA3_3, ARMA10_3, ARMA1_6, ARMA3_6, ARMA10_6, ARMA1_7, ARMA3_7, ARMA10_7)


### SELECT SECOND SPECIES ###

# Select the data of the right species
# Species 2: TS (Tamias striatus) is the Eastern Chipmunk
FullDataTS = subset(FullData, species_code == "TS")
TrainDataTS = subset(TrainData, species_code == "TS")
TestDataTS = subset(TestData, species_code == "TS")

par(mfrow=c(1,1))
plot(x = c(1:nrow(FullDataTS)), y = FullDataTS$totalCount, type = 'l', col="red",
     xlab = 'time (months)', ylab = 'Number of observations', main = 'Observations of E. chipmunk over time')

# Show the (partial) autocorrelation over lags for the training data
par(mfrow=c(2,1))
acf(TrainDataTS$totalCount,main='Autocorrelation for E. chipmunk data')
pacf(TrainDataTS$totalCount,main='Partial autocorrelation for E. chipmunk data')
# no transformation is needed.


### FITTING SARIMA MODELS ###
# Since TS species has a more seasonal life cycle, instead we'll use SARIMA models.

# s value in SARIMA model is 12, due to the twelve months in the year.
SAR1 = arima(TrainDataTS$totalCount,order=c(0,0,0), seasonal=list(order=c(0,0,1), period=12))
SAR2 = arima(TrainDataTS$totalCount,order=c(0,0,0), seasonal=list(order=c(0,1,1), period=12))
SAR3 = arima(TrainDataTS$totalCount,order=c(0,0,0), seasonal=list(order=c(0,1,2), period=12))
SAR4 = arima(TrainDataTS$totalCount,order=c(0,0,0), seasonal=list(order=c(1,1,1), period=12))
SAR5 = arima(TrainDataTS$totalCount,order=c(0,0,1), seasonal=list(order=c(0,1,1), period=12))
SAR6 = arima(TrainDataTS$totalCount,order=c(0,0,2), seasonal=list(order=c(0,1,1), period=12))
SAR7 = arima(TrainDataTS$totalCount,order=c(0,0,3), seasonal=list(order=c(0,1,1), period=12))
SAR8 = arima(TrainDataTS$totalCount,order=c(0,0,4), seasonal=list(order=c(0,1,1), period=12))
SAR9 = arima(TrainDataTS$totalCount,order=c(0,0,5), seasonal=list(order=c(0,1,1), period=12))
SAR10= arima(TrainDataTS$totalCount,order=c(1,0,1), seasonal=list(order=c(0,1,1), period=12))
SAR11= arima(TrainDataTS$totalCount,order=c(2,0,1), seasonal=list(order=c(0,1,1), period=12))
SAR12= arima(TrainDataTS$totalCount,order=c(1,0,5), seasonal=list(order=c(0,1,1), period=12))
SARBEST = arima(TrainDataTS$totalCount,order=c(1,0,1), seasonal=list(order=c(0,1,1), period=12))
               
# Let's select one of the models and check the mean square prediction error (MSPE):
# Inspection of results reveals that SAR12 model is the preferred choice
Model = SAR12

fore=predict(Model, n.ahead=10)
MSPE = mean((fore$pred-TestDataPM$totalCount)^2)
MSPE  # Show the mean square prediction error to the user

# Residual diagnostics
par(mfrow=c(2,1))
hist(Model$res, main='Histogram of residuals. SARIMA(1,0,5)x(0,1,1)12 model, E. Chipmunk data.')
qqnorm(Model$res, main='QQ-plot of residuals. SARIMA(1,0,5)x(0,1,1)12 model, E. Chipmunk data.')
par(mfrow=c(1,1))
tsdiag(Model)

# Clean up the global environment
rm(Model, SAR1, SAR2, SAR3, SAR4, SAR5, SAR6, SAR7, SAR8, SAR9, SAR10, SAR11, SAR12, SARBEST)


### PREDICTIONS ###
# Now that models have been found for the two selected species, let's use them to make predictions

# Species 1: PM
Model = arima(FullDataPM$totalCount, order=c(0,1,3))
fore=predict(Model, n.ahead=24)

par(mfrow=c(1,1))
plot(x = c(1:(nrow(FullDataPM))), y = FullDataPM$totalCount, xlim=c(0, 265), ylim = c(-50, 350), type = 'l', col="blue",
     xlab = 'time (months)', ylab = 'Number of observations', main = 'Prediction of Deer Mouse population')
points(x = c(242:265), y = fore$pred, type = 'l', col="red")
lines(x = c(242:265), y = fore$pred+2*fore$se,lty='dashed')
lines(x = c(242:265), y = fore$pred-2*fore$se,lty='dashed')
legend(220, 345, legend=c("Original data", "Prediction", "Uncertainty"),
       col=c("blue", "red", "black"), lty=1:2, cex=0.8)

# Species 2: TS
Model = arima(TrainDataTS$totalCount,order=c(1,0,5), seasonal=list(order=c(0,1,1), period=12))
fore=predict(Model, n.ahead=24)

par(mfrow=c(1,1))
plot(x = c(1:(nrow(FullDataTS))), y = FullDataTS$totalCount, xlim=c(0, 265), ylim = c(-75, 250), type = 'l', col="blue",
     xlab = 'time (months)', ylab = 'Number of observations', main = 'Prediction of E. Chipmunk population')
lines(x = c(242:265), y = fore$pred, type = 'l', col="red")
lines(x = c(242:265), y = fore$pred+2*fore$se,lty='dashed')
lines(x = c(242:265), y = fore$pred-2*fore$se,lty='dashed')
legend(220, 245, legend=c("Original data", "Prediction", "Uncertainty"),
       col=c("blue", "red", "black"), lty=1:2, cex=0.8)


