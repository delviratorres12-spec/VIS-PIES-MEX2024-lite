



# app.R
library(shiny)
library(dplyr)

# Cargar datos
df <- read.csv("data/data.csv")
base <- df

# Categorías
tipos <- c("Gobernadores","Diputados locales","Munícipes")
vars <- c("PEIIndexp", "laws.1", "procedures.1", "boundaries.1", 
          "votereg", "partyreg", "media", "finance", 
          "voting", "count", "results", "EMBs")
vars2 <- c("PEIIndexp_i", "laws.1_i", "procedures.1_i", "boundaries.1_i", 
           "votereg_i", "partyreg_i", "media_i", "finance_i", 
           "voting_i", "count_i", "results_i", "EMBs_i")

# UI con tabsetPanel (R base)
ui <- fluidPage(
  
  # CSS institucional
  tags$head(
    tags$style(HTML("
      /* Color principal en encabezados y títulos */
      h1, h2, h3, h4 {
        color: #0029a3;
      }

      /* Fondo de las pestañas activas */
      .nav-tabs > li.active > a, 
      .nav-tabs > li.active > a:focus, 
      .nav-tabs > li.active > a:hover {
        background-color: #0029a3;
        color: white !important;
      }

      /* Fondo de las pestañas inactivas */
      .nav-tabs > li > a {
        background-color: grey50;
        color: #0029a3;
      }

      /* Botones */
      .btn {
        background-color: #0029a3;
        color: white;
        border: none;
      }

      .btn:hover {
        background-color: grey50;
        color: grey80;
      }

      /* Tablas */
      table {
        border: 1px solid grey50;
      }
      th {
        background-color: #0029a3;
        color: white;
      }
      td {
        border: 1px solid grey50;
      }
    "))
  ),
  
  titlePanel("PIES México 2024"),
  
  tabsetPanel(
    tabPanel("Inicio",
             h3("Acerca del visualizador"),
             h2("Proyecto de integridad electoral subnacional en México 2024"),
             p("Este visualizador interactivo presenta los resultados de la 
             encuesta a expertos del proyecto de 
             integridad electoral subnacional (PIES) México 2024."),
             p("Desde 2012 el Electoral Integrity Project (EIP), 
                                   originalmente encabezado por Pippa Norris, de las universidades de Harvard y Sydney, 
                                   realiza encuestas a expertas y expertos por país para conocer su percepción de la 
                                   integridad de las elecciones en cada nación. En México, en 2015 y 2016, 
                                   investigadores/as de FLACSO México asociados/as al equipo del EIP, han entrevistado 
                                   a expertas/os locales para conocer la integridad electoral de las contiendas en los 
                                   32 estados de la República mexicana. De 2015 a 2024 se ha calibrado la integridad de 
                                   142 procesos electorales locales entrevistando a 1,703 expertas/os con el objetivo 
                                   de es evaluar la integridad de las elecciones 
             subnacionales en México a través de distintos indicadores, 
             permitiendo la comparación entre entidades federativas y 
             tipos de elección."),
             p("Fuente de datos: PIES México 2024. Disponible en Harvard Dataverse."),
             p(em("Loza, Nicolas; Elvira Torres, Diego Enrique; Coca Rios, Itzel, 2025, 
                                        PIESM (2024) nivel estado-eleccion, https://doi.org/10.7910/DVN/60BT7S, Harvard Dataverse")),
             p(" - Elaborado por Diego Elvira -", em("https://github.com/delviratorres12-spec/VIS-PIES-MEX2024 
                                                     DOI:10.5281/zenodo.17241012"))
    ),
    
    tabPanel("Resumen",
             h3("Elecciones analizadas (2024)"),
             tableOutput("tabla"),
             br(),
             selectInput("tipo", "Selecciona tipo de elección", choices = tipos),
             tableOutput("tabla2")
    ),
    
    tabPanel("Indicadores",
             fluidRow(
               column(6,
                      h3("Resultados"),
                      selectInput("tipo2", "Tipo de elección", choices = tipos),
                      selectInput("var", "Indicador", choices = vars),
                      plotOutput("graf2", height = "600px")
               ),
               column(6,
                      h3("Resultados con imputación"),
                      selectInput("tipo3", "Tipo de elección", choices = tipos),
                      selectInput("var2", "Indicador (imputado)", choices = vars2),
                      plotOutput("graf3", height = "600px")
               )
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Tabla resumen
  output$tabla <- renderTable({
    base %>%
      group_by(`Tipo.de.elección`) %>% 
      summarise(`Entidades` = n(),
                `# de respuestas` = sum(numresponses),
                `Tasa` = paste0(round(sum(numresponses) / sum(contacted),4)*100,"%")) %>%
      ungroup() %>% 
      rename(`Elección`=`Tipo.de.elección`)
  })
  
  # Tabla detalle por tipo
  output$tabla2 <- renderTable({
    base %>%
      filter(`Tipo.de.elección` == input$tipo) %>%
      select(estado)%>% 
      rename(`Entidades`=estado)
  })
  
  # Gráfico sin imputación
  output$graf2 <- renderPlot({
    datos <- base %>%
      filter(`Tipo.de.elección` == input$tipo2) %>%
      select(estado, !!sym(input$var))
    
    valores <- datos[[input$var]]
    nombres <- datos$estado
    
    barplot(valores,
            names.arg = nombres,
            las = 2,
            col = "#0029a3",
            border = "grey50",
            ylim = c(0, 90),
            main = paste("Puntuación por entidad del", input$var),
            ylab = "Puntuación")
  })
  
  # Gráfico con imputación
  output$graf3 <- renderPlot({
    datos <- base %>%
      filter(`Tipo.de.elección` == input$tipo3) %>%
      select(estado, !!sym(input$var2))
    
    valores <- datos[[input$var2]]
    nombres <- datos$estado
    
    barplot(valores,
            names.arg = nombres,
            las = 2,
            col = "#0029a3",
            border = "grey50",
            ylim = c(0, 90),
            main = paste("Puntuación por entidad del", input$var2),
            ylab = "Puntuación")
  })
}

shinyApp(ui, server)

# library(shinylive)
#
# # Exporta la app a HTML y recursos estáticos
# shinylive::export(appdir = ".", destdir = "docs")

