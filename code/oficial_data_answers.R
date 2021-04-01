# Pregunta 1: 
# Indique el top 5 de los campos con mayor produccion durante el año 2020

proc_crudo_ago_2020 %>% data.table::setDT()
oficial_answer_1 <- proc_crudo_ago_2020[, .(total_proc_enero = sum(enero),
                        total_proc_febrero = sum(febrero),
                        total_proc_marzo = sum(marzo),
                        total_proc_abril = sum(abril),
                        total_proc_mayo= sum(mayo),
                        total_proc_junio = sum(junio),
                        total_proc_julio = sum(julio),
                        total_proc_agosto = sum(agosto)), by = Campo]

oficial_answer_1 <- data.table(Campo = oficial_answer_1$Campo, Total_prod = base::apply(X = oficial_answer_1[,-1], MARGIN = 1, FUN = function(x) sum(x)))
oficial_answer_1[order(-Total_prod)][1:5]

# Respuesta 
#             Campo Total_prod
# 1:       RUBIALES   857001.1
# 2:       CASTILLA   537068.1
# 3:     CHICHIMENE   388023.8
# 4: CASTILLA NORTE   380005.8
# 5:          QUIFA   304783.7

# Pregunta 2: 
# Indique cuántas y cuáles compañías han reportado producción en más de 5 campos en Casanare en el año 2018

prod_casanare_2018 <- proc_crudo_2018[proc_crudo_2018$Departamento == "CASANARE",] %>% data.table::setDT()
cant_camp_por_oper_casanare <- prod_casanare_2018[, .(cant_campos = length(unique(Campo))), by = .(Departamento, Operadora)]
cant_camp_por_oper_casanare[cant_camp_por_oper_casanare$cant_campos > 5, c("Operadora","cant_campos")][order(-cant_campos)]

# Respuesta 
#                                           Operadora cant_campos
# 1:                         PERENCO COLOMBIA LIMITED          34
# 2:  Frontera Energy Colombia Corp Sucursal Colombia          34
# 3:                          GEOPARK COLOMBIA S.A.S.          20
# 4:                                   ECOPETROL S.A.           9
# 5:           PAREX RESOURCES COLOMBIA LTD. SUCURSAL           7
# 6:                              CEPSA COLOMBIA S.A.           6
# 7:                   COLOMBIA ENERGY DEVELOPMENT CO           6
# 8: NEW GRANADA ENERGY CORPORATION SUCURSAL COLOMBIA           6

# Pregunta 3:
# Indique los 5 contratos con las más alta producción en MMstb (Million Stock-tank barrels) en el año 2018

proc_crudo_2018 %>% data.table::setDT()
oficial_answer_3 <- proc_crudo_2018[, .(total_proc_enero = sum(Enero),
                                        total_proc_febrero = sum(Febrero),
                                        total_proc_marzo = sum(Marzo),
                                        total_proc_abril = sum(Abril),
                                        total_proc_mayo= sum(Mayo),
                                        total_proc_junio = sum(Junio),
                                        total_proc_julio = sum(Julio),
                                        total_proc_agosto = sum(Agosto),
                                        total_proc_septiembre = sum(Septiembre),
                                        total_proc_octubre = sum(Octubre),
                                        total_proc_noviembre = sum(Noviembre),
                                        total_proc_diciembre = sum(Diciembre)), by = Contrato]

oficial_answer_3 <- data.table(Contrato = oficial_answer_3$Contrato, Total_prod = base::apply(X = oficial_answer_3[,-1], MARGIN = 1, FUN = function(x) sum(x)))
oficial_answer_3$Total_prod_MMstb <- oficial_answer_3$Total_prod / 1000000
oficial_answer_3_top_5 <- oficial_answer_3[order(-Total_prod_MMstb)][1:5]
oficial_answer_3_top_5[,2:3] <-round(oficial_answer_3_top_5[,2:3], digits = 2) 
oficial_answer_3_top_5

# Respuesta 
#            Contrato Total_prod Total_prod_MMstb
# 1:         CUBARRAL  2177189.0             2.18
# 2:         RUBIALES  1433611.8             1.43
# 3:           LLA 34   725579.6             0.73
# 4:            QUIFA   554438.3             0.55
# 5: LA CIRA INFANTAS   542415.2             0.54

# Pregunta 4:
# Ordene de mayor a menor las 10 operadoras con mayor produccion en el mes de agosto 2019
proc_crudo_2019 %>% data.table::setDT()
oficial_answer_4 <- proc_crudo_2019[,c("Operadora", "Agosto")]
oficial_answer_4[,.(total_prod_agosto = sum(Agosto)), by = .(Operadora)][order(-total_prod_agosto)][1:10]

#                                          Operadora total_prod_agosto
# 1:                                  ECOPETROL S.A.         473986.41
# 2: Frontera Energy Colombia Corp Sucursal Colombia          81441.68
# 3:                         GEOPARK COLOMBIA S.A.S.          69154.42
# 4:                      OCCIDENTAL DE COLOMBIA LLC          53299.12
# 5:                          EQUION ENERGÍA LIMITED          34950.01
# 6:                 GRAN TIERRA ENERGY COLOMBIA LTD          29555.20
# 7:                  MANSAROVAR ENERGY COLOMBIA LTD          25256.75
# 8:                                      HOCOL S.A.          19614.02
# 9:          PAREX RESOURCES COLOMBIA LTD. SUCURSAL          15727.97
# 10:                             CEPSA COLOMBIA S.A.         12702.09

# Pregunta 5:
# Realice un análisis comparativo de la producción de los dos primeros trimestres de los años 2019 y 2020. Trimestres: Enero a Marzo, Abril a Junio 

# Diagrama de barras
prod_trim_2019 <- proc_crudo_2019[,c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio")]
prod_trim_2020 <- proc_crudo_ago_2020[,c("enero", "febrero", "marzo", "abril", "mayo", "junio")]

prod_trim_2019 %>% data.table::setDT()
prod_trim_1_2019 <- apply(X = prod_trim_2019[,c("Enero", "Febrero", "Marzo")], MARGIN = 1, FUN = function(x) sum(x))
prod_trim_2_2019 <- apply(X = prod_trim_2019[,c("Abril", "Mayo", "Junio")], MARGIN = 1, FUN = function(x) sum(x))
prod_trim_1_2019 <- sum(prod_trim_1_2019)
prod_trim_2_2019 <- sum(prod_trim_2_2019)
prod_total_trim_2019 <- data.table(año = rep(2019, 1), trim_1 = prod_trim_1_2019, trim_2 = prod_trim_2_2019)

prod_trim_2020 %>% data.table::setDT()
prod_trim_1_2020 <- apply(X = prod_trim_2020[,c("enero", "febrero", "marzo")], MARGIN = 1, FUN = function(x) sum(x))
prod_trim_2_2020 <- apply(X = prod_trim_2020[,c("abril", "mayo", "junio")], MARGIN = 1, FUN = function(x) sum(x))
prod_trim_1_2020 <- sum(prod_trim_1_2020)
prod_trim_2_2020 <- sum(prod_trim_2_2020)
prod_total_trim_2020 <- data.table(año = rep(2020, 1), trim_1 = prod_trim_1_2020, trim_2 = prod_trim_2_2020)

prod_total_trim_2019_2020 <- rbind(prod_total_trim_2020, prod_total_trim_2019)

prod_total_trim_2019_2020 <- melt(prod_total_trim_2019_2020, id.vars = c("año"))

colnames(prod_total_trim_2019_2020) <- c("año", "trimestre", "total_prod")

prod_total_trim_2019_2020$año <- as.factor(prod_total_trim_2019_2020$año)
prod_total_trim_2019_2020$Total_prod_MMstb <- prod_total_trim_2019_2020$total_prod / 1000000

ggplot(data=prod_total_trim_2019_2020, aes(x=año, y=Total_prod_MMstb, fill=trimestre)) +
  geom_bar(stat="identity", position="dodge")

# Respuesta:

#     año trimestre total_prod Total_prod_MMstb
# 1: 2020    trim_1    2619417         2.619417
# 2: 2019    trim_1    2676538         2.676538
# 3: 2020    trim_2    2258345         2.258345
# 4: 2019    trim_2    2677717         2.677717

# Se evidencia por medio del gráfico de barras la disminución de la producción de petróleo en el segundo trimestre
# del año 2020 comparado con el año 2019, esto debido posiblemente a la pandemia. También vemos que los dos primeros trimestres
# del año 2019 tienen un comportamiento bastante similar. En general hubo mayor producción de petróleo el año 2019.

