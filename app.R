library(shinydashboard)
library(shiny)
library(tidyverse)
library(ggplot2)
library(shinycssloaders)
library(shinyWidgets)
library(shiny)
library(leaflet)
library(periscope)
# library(Matrix)
# renv::dependencies()
# renv::init()   se debe restaurar el proyecto
# renv::snapshot() se debe hacer una captura del proyecto
# renv::status()



#caragar fuente
source("global.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(

  #includeCSS("www/custom.css"),
  dashboardHeader(title = "Estadisticas INEC"),
  ## Sidebar contenido ---- 
  dashboardSidebar(
    sidebarMenu(
      # menuItem("Revistas en DOAJ", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Morbilidad por región", tabName = "mapa", icon = icon("th")),
      # menuItem("Morbilidad por región - año", tabName = "mapaA", icon = icon("th")),
      menuItem("Morbilidad por año", tabName = "grafico", icon = icon("th"))
    )
    
  ),
  # Cuadro de contenido 
  ## Body content
  dashboardBody(
    tabItems(
      tabItem(tabName = "mapa",
              h2("Mapa de Panamá y causas de Morbilidad"),
              
              fluidRow(
                column(3,
                        selectInput(inputId = "sel_causas",
                                    label = "Seleccione una causa de morbilidad:", 
                                    choices = causas_lista,
                                    selected = names(causas_lista[16])),
                      ),
                column(3,
                        selectInput(inputId = "sel_anio",
                                    label = "Seleccione Año:", 
                                    choices = lista_anio,
                                    selected = names(lista_anio[12])),
                      ),
                column(2,
                       selectInput(inputId = "sel_edad",
                                   label = "Seleccione rango de Edad:", 
                                   choices = lista_edad,
                                   selected = names(lista_edad[11])),
                ),
                column(1,
                       radioButtons("grupoGenero", "Género", 
                                    choices = list("Hombres" = "Hombres",
                                                   "Mujeres" = "Mujeres" , 
                                                   "Ambos" = "HM"),
                                    selected = "HM"),
                       
                ),
                column(2,
                       radioButtons("grupoPrevalencia", "Indicador", 
                                    choices = list("Prevalencia" = 2,
                                                   "Casos" = 1 ),
                                    selected = 1),
                       
                ),
                column(1,
                       radioButtons("grupoColor", "Color Mapa", 
                                    choices = list("Color" = 2,
                                                   "B/N" = 1 ),
                                    selected = 2),
                       
                      )
              ),
              
              
              fluidRow(
                column(12,
                  plotOutput("mapa_panama",  height = "700px", width = "100%")|> 
                  withSpinner(), downloadButton(outputId = "downloadPlot",label =  "Download Plot")
                  # downloadLink("downloadPlot", "Descargar Gráfico"),
                ) 
              )
              ),
      
      tabItem(tabName = "grafico",
              h2("Gráficos de Morbilidad por año y región en Panamá"),
              selectInput(inputId = "sel_region",
                          label = "Seleccione una región de Panamá:", 
                          choices = region_lista),
              
              
              fluidRow(
                column(12,
                       plotOutput("plot_region",  height = "700px", width = "100%")|> 
                         withSpinner(),
                       
                ) 
              )
              
              
      ),
      tabItem(tabName = "mapaA",
              h2("Gráficos de Morbilidad por año y región en Panamá"),
              selectInput(inputId = "sel_region",
                          label = "Seleccione una región de Panamá:", 
                          choices = region_lista),
              
              
              fluidRow(
                column(12,
                       plotOutput("mapa_panamaA",  height = "700px", width = "100%")|> 
                         withSpinner()
                       
                ) 
              )
              
              
      )
    )
  )

)

# Define server logic required to draw map and plot
server <- function(input, output) {
  
  # imprimir mapa en UI
  output$mapa_panama <- renderPlot({
    cargar_mapa(Datos_base, gdf, 
                code_= input$sel_causas, 
                anio_= input$sel_anio,
                input$grupoGenero, 
                edad_= input$sel_edad,
                color_ = input$grupoColor, 
                grupoPrevalencia_ = input$grupoPrevalencia, 
                fuente=4.5)
  })
  
  # imprimir mapa en UI ANIMADO
  output$mapa_panamaA <- renderPlot({
    mapa_animado(Datos_base, gdf, 
                code_= "Total", 
                anio_= "Total",
                "HM", 
                edad_= "TOTAL",
                color_ = 2, 
                grupoPrevalencia_ = 1, 
                fuente=4.5)
  })
  
  # funcion paar descargar mapa
  miplot <- reactive({
    cargar_mapa(Datos_base, gdf, 
                code_= input$sel_causas, 
                anio_= input$sel_anio,
                input$grupoGenero, 
                edad_= input$sel_edad,
                color_ = input$grupoColor, 
                grupoPrevalencia_ = input$grupoPrevalencia, 
                fuente=3.0)
  })
  
  
  # caragar grafico de barras
  output$plot_region <- renderPlot({
    grafico_anio(Datos_base,region_= input$sel_region)
  })
  
  # descargar gráfico
  output$downloadPlot <- downloadHandler(
    filename = function() { "grafico.pdf" },
    content = function(file) {
      pdf(file, paper = "a4r", width=12,height=8) #default
      plot(miplot())
      dev.off()
    })
  


}

# Run the application 
shinyApp(ui = ui, server = server)
