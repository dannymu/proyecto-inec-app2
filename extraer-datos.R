# Cargar librerias ----

library(readxl)
library(sf)
library(ggplot2)
library(tidyverse)
library(fuzzyjoin)
library(RColorBrewer)
library(paletteer)
library(purrr)
library(rio)
library(vctrs)

#-------------------------------
# Función para limpiar datos por documento

limpiar_datos <- function(Datos_base){
  
  # Normalizar variables
  Datos_base <- janitor::clean_names(Datos_base)
  
  # Eliminar notas finales de la tabla
  filas <- nrow(Datos_base)-5
  Datos_base = Datos_base[1:filas,]
  
  # Etraer y guardar fila de H M y total
  # fila <- Datos_base[1,]
  # Datos_base <-Datos_base[-1,]
  
  # Llenar Datos hacia abajo de la columna Región de salud
  Datos_base<- Datos_base |>
    fill(region_de_salud, .direction="down")
  
  # Eliminar celdas vacia de la columna cod que corresponde al total por region
  Datos_base <- Datos_base |> 
    dplyr::filter(! is.na(cod))
  
  
  # Seleccionar celdas diferentes a "Total", solo las de  region
  Datos_base <- Datos_base |> 
    dplyr::filter(region_de_salud!="Total")
  
  return(Datos_base)
}



#-----------------------------------------------
# Función para transformar datos por region ----
# de formato ancho a formato largo

region_pivotlonger <- function(Datos_base, region_){
  
  # region_ <- region_clean$Var1[1]
  edades <- c("Menor de un año",
              "1 a 4 años",
              "5 a 9 años",
              "10  a 14 años",
              "15  a 19 años",			
              "20 a 24 años",			
              "25 a 34 años",
              "35 a 49 años",
              "50 a 59 años",
              "60 a 64 años",
              "65 y  más"		
  )
  
  #columnas de datos por edad y causas
  columnas_causas <- c(1,2,3)
  
  edad_columnas <-list(
    c(5,6),
    c(8,9),
    c(11,12),
    c(14,15),
    c(17,18),
    c(20,21),
    c(23,24),
    c(26,27),
    c(29,30),
    c(32,33),
    c(35,36)
  )
  
  # Seleccionar celdas por region eliminando columna de totales
  data <- Datos_base[,-c(4,5,6)] |> 
    dplyr::filter(region_de_salud==region_)
  
  
  #Ciclo repetición para convertir datos por region en pivot_longer
  data_full <- data.frame()
  
  for (x in 1:length(edades)){
     # print(x)
    
    # columnas a evaluar para todas las  regiones
    columnas_selec <- c(columnas_causas,edad_columnas[[x]])
  
    # seleccionar columna spor region
    data_ <- data[,columnas_selec] 
    colnames(data_) <- c("Region", "Code","Causas","H","M")
 
    
    #Transformar data
    data_ <- data_ |>
      pivot_longer(
        c(H,M),
        names_to = "Genero",
        values_to = "Casos"
      ) |>
      mutate(Edad=edades[x])
    
    # print(data_)
    
    data_full <-rbind(data_full,data_)
  }  
  
    return(data_full)
    
}

# -----------
# Prueba ----

# # Cargar datos año 2010 ----
# Datos_base = read_excel("Irela.xlsx", 
#                         sheet= "2010", 
#                         skip = 5,
#                         na = "TRUE",
#                         .name_repair = "unique_quiet")
# 
# # Limpiar datos
# Datos_base <- limpiar_datos(Datos_base)
# 
# # Seleccionar lista de Regiones
# region_clean <- table(Datos_base$region_de_salud) |>  as.data.frame()
# 
# region_casos_anio <- map_df(region_clean$Var1, ~ region_pivotlonger(Datos_base, .))
# region_casos_anio$anio <- 2010


#------------------------------------------------
# funcion unir todos los datos de años por region 
region_casos_Allyear <- function(file_,year){
  
  # year<- data_list[4]
  print(paste0("Transformando año: ", year))
  
  # Cargar datos año  ----
  Datos_base = read_excel(file_, sheet= year, 
                          skip = 5,
                          na = "TRUE",
                          .name_repair = "unique_quiet")

  # Limpiar datos
  Datos_base <- limpiar_datos(Datos_base)
  
  # Seleccionar lista de Regiones
  region_clean <- table(Datos_base$region_de_salud) |>  as.data.frame() 
  # region_clean <- unique(Datos_base$region_de_salud)
  
  region_casos_anio <- map_df(region_clean$Var1, ~ region_pivotlonger(Datos_base, .))
  region_casos_anio$ano <- year
  
  return(region_casos_anio)
}


#------------------------------------------
#----------- codigo principal ------------
# se debe haber ejecutado las lineas anteriores de funciones


# Leer lista de hojas de Excel
data_list <- openxlsx::getSheetNames("Irela2.xlsx")
data_list <- data_list[c(8:19)] #solo ejecutar una vez

# Cargar archivo irela2
file_<-"Irela2.xlsx"


# Ejecutar funcion principal
region_casos_Completo <- map_df(data_list, ~ region_casos_Allyear(file_, .))

# opcional -transformar datos numericos
region_casos_Completo$Casos <- as.numeric(region_casos_Completo$Casos)
region_casos_Completo$ano <- as.numeric(region_casos_Completo$ano)

# Contabilizar registros
table(region_casos_Completo$ano) %>% as.data.frame()

#Guardar archivo en CSV
write.csv(region_casos_Completo,paste0("INEC-morbilidad-",Sys.Date(),".csv"))


# ELIMINAR MENSAJE 
# `summarise()` has grouped output by 'code'. You can override using the `.groups` argument.
# options(dplyr.summarise.inform = FALSE)
