pacman::p_load(tidymodels, tidyverse, nycflights13)
set.seed(42)

# cargar y limpiar datos ----
flight_data <- read.csv("Train.csv")
          
flight_data2 <- sample(1:nrow(flight_data),1000, replace=FALSE)

Sample_flight_data <- flight_data[flight_data2,]

Sample_flight_data <- Sample_flight_data

