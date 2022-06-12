pacman::p_load(tidymodels, tidyverse, nycflights13)
set.seed(42)

# cargar y limpiar datos ----
data <- read.csv("ALUMNOS-trainData.csv")
          
Base <- 
  data %>% 
  mutate(
    # discretiza arr_delay y lo hace factor
    noshow = ifelse(noshow >= 4, 1, 0),
    noshow = factor(noshow),

  ) %>% 

  # excluyo datos faltates
  na.omit() %>% 
  # transformo characteres en factores
  mutate_if(is.character, as.factor)  %>% 
  # tomo una muestra de tamaño 10.000 para poder ejecutar los modelos
  sample_n(10000)

## inspeccion de los datos 
Base %>% 
  count(noshow) %>% 
  mutate(prop = n/sum(n))

glimpse(Base)


# especificar receta ----
noshow ~ .

# receta simple
receta <- 
  recipe(noshow ~ ., data = Base) 


# visualizar
summary(receta)

Base %>% 
  distinct(date) %>% 
  mutate(numeric_date = as.numeric(date)) 

# modificando fechas, predictores nominales a dummys y eliminar columnas sin varianza
receta <- 
  recipe(noshow ~ ., data = Base) %>% 
  update_role(fligth_number, departure_time, new_role = "ID") %>% 
  step_date(date, features = c("dow", "month")) %>%               
  step_holiday(date, 
               holidays = timeDate::listHolidays("US"), 
               keep_original_cols = FALSE)%>% 
    step_dummy(all_nominal_predictors())%>% 
    step_zv(all_predictors())
# especificar modelo ----

nearest_neighbor()

nearest_neighbor() %>% 
  set_mode("classification")

knn_mod <- nearest_neighbor() %>% 
  set_mode("classification")
# genera workflow ----

base_wflow <- 
  workflow() %>% 
  add_model(knn_mod) %>% 
  add_recipe(receta)

base_wflow

# split data ----
data_split <- initial_split(Base, prop = 3/4)

train_data <- training(data_split)
test_data  <- testing(data_split)


train_data %>% 
  count(noshow) %>% 
  mutate(prop = n/sum(n))

data_split_strat <- initial_split(Base, prop = 3/4, strata = noshow)
train_data <- training(data_split_strat)
test_data  <- testing(data_split_strat)

train_data %>% 
  count(noshow) %>% 
  mutate(prop = n/sum(n))

