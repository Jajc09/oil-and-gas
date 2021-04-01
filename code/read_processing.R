# Librerias
pacman::p_load("readxl",
               "janitor",
               "dplyr",
               "data.table",
               "ggplot2",
               "scales")

# Archivo que lee y procesa los datos 
####### Bonus: Cargar las bases directamente desde la pagina de internet
proc_crudo_ago_2020 <- read_excel("data/proc_crudo_ago_2020.xlsx")
proc_crudo_2018 <- read_excel("data/proc_crudo_2018.xlsx")
proc_crudo_2019 <- read_excel("data/proc_crudo_dic_2019.xlsx")

# Limpieza para los datos de proc_crudo_ago_2020
proc_crudo_ago_2020 <- proc_crudo_ago_2020[2:nrow(proc_crudo_ago_2020)-2, ] %>% janitor::row_to_names(row_number = 5)
proc_crudo_ago_2020[, 6:ncol(proc_crudo_ago_2020)] <- base::sapply(proc_crudo_ago_2020[, 6:ncol(proc_crudo_ago_2020)], FUN = function(x) as.numeric(x))

# Limpieza para los datos de proc_crudo_2018
proc_crudo_2018 <- proc_crudo_2018[2:nrow(proc_crudo_2018)-1,] %>% janitor::row_to_names(row_number = 2)
proc_crudo_2018[, 6:ncol(proc_crudo_2018)] <- round(base::apply(proc_crudo_2018[, 6:ncol(proc_crudo_2018)], MARGIN = 2, FUN = function(x) as.numeric(x)), digits = 2)

#Limpieza para los datos de proc_crudo_2019
proc_crudo_2019 <- proc_crudo_2019[2:nrow(proc_crudo_2019)-2,] %>% janitor::row_to_names(row_number = 2)
proc_crudo_2019[, 6:ncol(proc_crudo_2019)] <- round(base::apply(proc_crudo_2019[, 6:ncol(proc_crudo_2019)], MARGIN = 2, FUN = function(x) as.numeric(x)), digits = 2)
