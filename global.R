# Cargar librerias ----

library(readxl)
library(sf)
library(ggplot2)
library(tidyverse)
library(fuzzyjoin)
library(RColorBrewer)
library(paletteer)
library(httr2)
library(curl)
library(gganimate)
# display.brewer.all(colorblindFriendly = TRUE)  

# Cargar datos ----
#Datos_base = read_excel("Irela.xlsx", sheet= "Base")
# Datos_base <- region_casos_Completo
# Datos_base <- read_csv("INEC-morbilidad-2023-11-01.csv")
Datos_base <- read_excel("Candidiasis_BASE_COMPLETA.xlsx")
# table(Datos_base$edad)
# Datos_base <- Datos_base |> filter(code!="Total" & genero!="Total" & edad!="TOTAL")

# Datos_base |> filter(edad == "20 a 24 años") |> group_by(region) %>% 
#   summarise(casos = sum(casos))

# str(Datos_base)

# Normalizar variables
Datos_base <- janitor::clean_names(Datos_base)
Datos_base$casos <- as.numeric(Datos_base$casos)
Datos_base$ano <- as.numeric(Datos_base$ano)
Datos_base$prevalencia <- round(Datos_base$prevalencia,0)

# Cargar Poligonos mapa
gdf <- st_read('mapa-poligonos.json')
geometry_union <- st_union(gdf$geometry[5], gdf$geometry[6]) # unir geometrias de Darien
gdf$geometry[5] <- geometry_union
gdf <- gdf[-6,] # borar geometria que se unio

geometry_union <- st_union(gdf$geometry[10], gdf$geometry[11]) # unir geometrias de Panama
gdf$geometry[11] <- geometry_union
gdf <- gdf[-10,] # borar geometria que se unio
# df <- df[-6,]

# geom = list()
# for(i in 5:6) {
#   geom[[i]] = st_union(gdf$geometry[i])
# }
# geom = do.call(c, geom)
# geom <- append(geom[1], geom[2]) # unir
# # plot(geom)
# df$geometry[5] <-  geom

# gdf$geometry[5] ,<- st_join(gdf$geometry[5],gdf$geometry[6], left = FALSE)
# gdf <- gdf[-6,]
# gdf$PROV_NAME[6] <- gdf$PROV_NAME[5]

# Correcion de provincias ----
provincias <- c("Bocas del Toro",
                "Chiriquí",
                "Coclé",
                "Colón",
                "Kuna Yala",
                "Ngöbe Buglé",
                "Darién",
                "Herrera",
                "Los Santos",
                "Panamá Oeste",
                "Panamá",
                "Veraguas") 

# Limpieza de datos ----
# Renombrar region (provincias)

renombrar_provincias<- function(Datos_base){
  
  Datos_base$region[Datos_base$region =="BOCAS DEL TORO"]<-provincias[1]
  Datos_base$region[Datos_base$region =="CHIRIQUI"]<-provincias[2]
  Datos_base$region[Datos_base$region =="COCLE"]<-provincias[3]
  Datos_base$region[Datos_base$region =="COLON"]<-provincias[4]
  Datos_base$region[Datos_base$region =="COM. KUNA YALA"]<-provincias[5]
  Datos_base$region[Datos_base$region =="COMARCA KUNA YALA"]<-provincias[5]
  Datos_base$region[Datos_base$region =="KUNA YALA"]<-provincias[5]
  Datos_base$region[Datos_base$region =="C. NGOBE BUGLE"]<-provincias[6]
  Datos_base$region[Datos_base$region =="COM. NGOBE BUG"]<-provincias[6]
  Datos_base$region[Datos_base$region =="COMARCA NGOBE BUGLE"]<-provincias[6]
  Datos_base$region[Datos_base$region =="DARIEN"]<-provincias[7]
  Datos_base$region[Datos_base$region =="HERRERA"]<-provincias[8]
  Datos_base$region[Datos_base$region =="LOS SANTOS"]<-provincias[9]
  Datos_base$region[Datos_base$region =="PANAMA OESTE"]<-provincias[10]
  Datos_base$region[Datos_base$region =="PANAMA METRO"]<-provincias[11]
  Datos_base$region[Datos_base$region =="SAN MIGUELITO"]<-provincias[11]
  Datos_base$region[Datos_base$region =="PANAMA NORTE"]<-provincias[11]
  Datos_base$region[Datos_base$region =="PANAMA ESTE"]<-provincias[11]
  Datos_base$region[Datos_base$region =="PANAMÁ"]<-provincias[11]
  Datos_base$region[Datos_base$region =="VERAGUAS"]<-provincias[12]
  
  #modificar texto de causas
  Datos_base$causas[Datos_base$causas == "Candidiasis de la piel y las uÃ±as"] <- "Candidiasis de la piel y las uñas"
  # Datos_base$edad[Datos_base$edad == "TOTAL"] <- "Total"
  
  return(Datos_base)
}

# Listado de causas ----
listado_causas <- function(Datos_base){
  
  causas_ <- Datos_base %>% 
    group_by(code,causas) %>% 
    summarise(n = n())
  
  return(causas_)
  
}

# Listado de region ----
listado_region <- function(Datos_base){
  
  region_ <- Datos_base %>% 
    group_by(region) %>% 
    summarise(n = n())
  
  return(region_)
  
}

# Listado de edades ----

  
  # lista de edades
  # rm(lista_edad_)
  # rm(lista_edad_)

  lista_edad_ <- table(Datos_base$edad) |> 
              as.data.frame()
  # lista_edad_ <- lista_edad_[-c(3,5,13),]
  lista_edad_$Var1 <-  lista_edad_$Var1 |> as.character()
  lista_edad <-  lista_edad_$Var1 |> as.character()
  names(lista_edad) <- lista_edad

  
  # lista de anio
  lista_anio_ <- table(Datos_base$ano) |> 
                  as.data.frame()
  lista_anio_$Freq <- lista_anio_$Var1 %>% 
                      as.character()
  lista_anio_$Var1 <- as.character(lista_anio_$Var1)
  new_row <- list("Total","Total")
  lista_anio_ <- rbind(lista_anio_,new_row)
  # lista_anio <- lista_anio_$Var1 %>% as.numeric()
  lista_anio <- lista_anio_$Var1
  names(lista_anio) <- lista_anio_$Var1
  


# cargar datos en shiny ----
# Cargar datos de lista de causas
Datos_base <- renombrar_provincias(Datos_base)
causas_lista_ <- listado_causas(Datos_base)
causas_lista <- causas_lista_$code
names(causas_lista) <- causas_lista_$causas
  
# cargar datos de region
region_lista_ <- listado_region(Datos_base)
region_lista <- region_lista_$region
names(region_lista) <- region_lista_$region

# 
#  code_ <-"B37.0"
#  anio_ <- 2010
#  genero_ = "HM"
# edad_ <- "0 a 4 años"
# grupoPrevalencia_ <- 1

data_region <- function(Datos_base, code_, anio_,genero_, edad_, grupoPrevalencia_){
  
  print(genero_)
  print(edad_)
  print(paste0("prevalencia = ",grupoPrevalencia_))
  Datos_base$grupoPrevalencia <- grupoPrevalencia_ # añadir variable prevalencia a base de datos
  
  anio_ <- ifelse(anio_=="Total",0,as.numeric(anio_))
  print(paste0("anio",anio_))
  
  if(anio_==0){
    if(genero_=="HM"){
      print("data_region hm")
      data <- Datos_base |> 
        filter(code==code_ & edad==edad_ & genero!="Total") |> 
        group_by(region) |> 
        # summarise(casos = sum(casos))
        # summarise(casos = sum(ifelse(grupoPrevalencia==1,casos,
        #                              round(prevalencia,1)))) # validar si es caso o prevalencia en UI
        summarise(casos = ifelse(grupoPrevalencia==1,sum(casos), 
                                 round(mean(prevalencia),0)
        )) # validar si es caso o prevalencia en UI
    }else{
      print("data_region h o m")
      data <- Datos_base |> 
        filter(code==code_ & genero==genero_ & edad==edad_) |> 
        group_by(region) |> 
        summarise(casos = ifelse(grupoPrevalencia==1,sum(casos), 
                                 round(mean(prevalencia),0)
        )) # validar si es caso o prevalencia en UI
        # summarise(casos = sum(casos))
    }
  
  }else{
    if(genero_=="HM"){
      print("data_region hm anio")
      
      data <- Datos_base %>% 
        filter(code==code_ & ano== anio_ & edad==edad_ & genero!="Total") %>% 
        group_by(region) %>% 
        summarise(casos = ifelse(grupoPrevalencia==1,sum(casos), 
                                 round(mean(prevalencia),0)
                                 )) # validar si es caso o prevalencia en UI
        # summarise(casos = sum(casos))
    }else{
      print("data_region hom anio")
      data <- Datos_base %>% 
        filter(code==code_ & ano== anio_ & genero==genero_ & edad==edad_) %>% 
        group_by(region) %>% 
        summarise(casos = ifelse(grupoPrevalencia==1,sum(casos), 
                                 round(mean(prevalencia),0)
        )) # validar si es caso o prevalencia en UI
        # summarise(casos = sum(casos))
    }
  }
  
  print(data)
  
  return(data) 
}




# Rango de anio del mapa
rango_anio <- function(Datos_base,anio__){
  
  anio_min<- min(Datos_base$ano)
  anio_max<- max(Datos_base$ano)
  
  anio<- ifelse((anio__==0),paste0(anio_min,"-",anio_max),anio__)
  
  return(anio)
}




cargar_mapa <- function(Datos_base, gdf, code_, anio_, genero_, edad_, color_, grupoPrevalencia_, fuente_){
  
  print(paste0("-",edad_))
  print(color_)
  print(paste0("AA-",anio_))
  # color_ <-2
  # Datos para el mapa ---- filtro ---
  data <- data_region(Datos_base,code_,anio_, genero_, edad_, grupoPrevalencia_)
  titulo<- listado_causas(Datos_base) %>% 
    filter(code==code_) %>% 
    arrange(desc(n)) %>% 
    head(2)
  
  titulo <- titulo$causas
  anio <-rango_anio(Datos_base,anio_)
  
  #colores
  cols <- c("yellow","gold","lightblue","royalblue","orange","red")
  cols_ <- c("white","black","lightgreen","red")
  pos <- ifelse(color_==1, 1, 3)
  
  gdf_merged <- left_join(gdf, data, by = c("PROV_NAME" = "region"))
  gdf_merged$geometry_centroid <- st_centroid(st_geometry(gdf_merged))
  gdf_merged <- gdf_merged %>% 
                mutate(value_cut = cut(casos, breaks=c(0,250,500,750,1000,1250,1500)))
  mayor <- mean(data$casos) + (mean(data$casos)/2 )

  print(mayor)
  
  # min(gdf_merged$casos, na.rm = TRUE)
  # gdf_merged$casos %>% max(na.rm = TRUE)
  
  # 10500-400
  
  # Personalizar Titulo
  # si anio es == total  y code == total
  if(anio_=="Total"){
    titulo_ <- ifelse(code_=="Total", 
                     paste0("Morbilidad de todas las causas ", "atendidas de 2010-2020 ", "en Panamá"),
                     paste0("Morbilidad de ", titulo, " atendidas de 2010-2020 ", " en Panamá")
                     )

  }else{
    # titulo <- paste0("Morbilidad de todas las causas ", "atendidas en ",anio_, "en Panamá")
    titulo_ <- ifelse(code_=="Total", 
                     paste0("Morbilidad de todas las causas ", "atendidas en ",anio_, " en Panamá"),
                     paste0("Morbilidad de ", titulo, "atendidas en ",anio_, " en Panamá")
    )
  }
  
  casos_anio <- ggplot(data = gdf_merged) +  geom_sf(aes(fill = casos)) + 
                    geom_sf_text(aes(label = ifelse(!is.na(casos),paste0(PROV_NAME,"\n", casos), ''),
                                 geometry = st_centroid(st_geometry(gdf_merged))),
                                 color = ifelse(gdf_merged$casos>=mayor,"white","black"), 
                                 size = fuente_, 
                                 nudge_x = 0.105, 
                                 nudge_y = 0.105)+  
                    # scale_fill_gradient(low = "lightgreen", high = "red")+ 
                    # scale_fill_gradient(low = "white", high = "black")) +
                    scale_fill_gradient(low = cols_[pos], high = cols_[pos+1]) +
                    #scale_fill_manual(values = cols)+
                    labs(x="",
                         y="", 
                        fill=ifelse(grupoPrevalencia_==1,"Casos", 
                                     "Prevalencia"),
                      # title = paste0("Morbilidad de ",titulo," atendidas en ",anio," en Panamá"),
                      title = titulo_,
                      subtitle = paste0(ifelse(grupoPrevalencia_==1,"Casos", 
                                               "Prevalencia")," de ", ifelse(genero_=="HM","Hombres/Mujeres",genero_), " ", 
                                        ifelse(edad_=="TOTAL","de 0 a más de 65 años" ,paste0("de ",edad_))),
                    caption = "Fuente: INEC Panamá \n Realizado por: Danny Murillo, Yostin Añino, Irela Carrasco")+
                    theme_minimal()+
                    theme(legend.position = "bottom",
                          plot.title=element_text(size=20),
                          plot.subtitle = element_text(size=16, 
                                                       color = "#333333"))
  
   casos_anio
}

# funcion mapa animado
# ------------------------------------

mapa_animado<- function(Datos_base, gdf, code_, anio_, genero_, edad_, color_, grupoPrevalencia_, fuente_){
  
  code_= "Total" 
  anio_= "Total"
  genero_ = "HM" 
  edad_= "TOTAL"
  color_ = 2
  grupoPrevalencia_ = 1
  fuente_=4.5
  year <- unique(Datos_base$ano) |> as.vector()
  mapa_carga<- cargar_mapa(Datos_base, gdf, code_, anio_, genero_, edad_, color_, grupoPrevalencia_, fuente_)
  mapa_carga <-  mapa_carga +
    transition_states(year)
}



# mostrar el mapa PRUEBA
# cargar_mapa(Datos_base, gdf, code_, anio_)

# str(Datos_base)[1]
# anio_ <-2021
# region_ <- "Chiriquí"
# table(Datos_base$ano)

grafico_anio<- function(Datos_base,region_){
  
  plot <- Datos_base %>% 
    filter(region==region_& genero!="Total" & code!="Total" & edad!="TOTAL") %>% 
    group_by(ano, genero) %>% 
    summarise(casos = sum(casos)) %>% 
    ggplot(aes(x=factor(ano), y=casos, fill=genero)) +
    geom_col(position = "dodge")+
    labs(x =  "Año",
         y = "n° Casos",
         fill = "Genero",
         title = paste0("Número de casos atendidos en ",region_," de 2010-2020"),
         subtitle = "Centro de Estadisticas",
         caption = "Realizado por: Danny Murillo, Yostin Añino, Irela Carrasco")+
    geom_text(stat = 'identity',
              aes(label = paste0(casos)),
              position = position_dodge(0.9),
              size = 6,
              vjust=-0.6,
              color = "Black")+
      theme_minimal()+
      theme(plot.title=element_text(size=20),
          axis.text=element_text(size=15))
  
  plot
  
}


# grafico_anio(Datos_base,region_)




# str(lista_anio)

# anio_ <-2012
# 
# Datos_base %>% 
#   filter(ano==anio_) %>% 
#   group_by(causas, region) %>%  
#   summarise(n=sum(casos)) %>% 
#   ggplot(aes(region, causas, fill=n)) +
#   geom_tile()+
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 0, vjust = 0.6),
#         axis.title.x=element_blank(), axis.title.y=element_blank(),
#         panel.grid.major = element_line(colour = "NA"), 
#         panel.grid.minor = element_line(colour = "NA"),
#         plot.title = element_text(hjust = 0.5, size = rel(1)), 
#         plot.subtitle = element_text(hjust = 0.5, size = rel(1)),
#         panel.background = element_rect(fill = "NA", colour = "NA", size = 1),
#         legend.title = element_blank(), legend.position="top",
#         legend.direction="horizontal", legend.key.width=unit(2, "cm"),
#         legend.key.height=unit(0.25, "cm"), legend.spacing=unit(-0.5,"cm"), 
#         panel.spacing=element_blank()) +
#  labs(title= paste0("Casos por region y causas de morbilidad del  año ",anio_)) +
#   scale_fill_gradient(low ="lightgreen", high = "red") 


  # facet_wrap(~ genero)
