



# app.R
library(shiny)
library(dplyr)

# Cargar datos
# rm(list=ls())
df <- read.csv("data/data.csv")
base <- df

base_larga <- read.csv("data/base_larga.csv")

# Categorías
tipos <- c("Gobernadores","Diputados locales","Munícipes")
vars <- c("PEIIndexp", "laws.1", "procedures.1", "boundaries.1", 
          "votereg", "partyreg", "media", "finance", 
          "voting", "count", "results", "EMBs")
vars2 <- c("PEIIndexp_i", "laws.1_i", "procedures.1_i", "boundaries.1_i", 
           "votereg_i", "partyreg_i", "media_i", "finance_i", 
           "voting_i", "count_i", "results_i", "EMBs_i")
entidades <- df$estado

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
             HTML("
             <p><b>Este visualizador interactivo presenta los resultados de la 
             encuesta a expertos del proyecto de 
             integridad electoral subnacional (PIES) México 2024.</b></p>",
                  
                  "<p>Desde 2012 el Electoral Integrity Project (EIP), 
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
             tipos de elección.</p>",
                  
                  "<p>En las pestañas del visualizador encontrará:</p>
             
             <ul>
               <li>Un <b>resumen</b> de las elecciones analizadas.</li>
               <li>Un visualizador de los resultados de los <b>indicadores</b> de integridad de las elecciones analizadas.</li>
               <li>Comparación de indicadores por <b>entidad</b>.</li>
             </ul>
             
             "),
             
             br(),
             HTML("<p><b>Fuente de datos:</b> PIES México 2024. Disponible en <i>Harvard Dataverse</i>:</p>
                  <ul>
             <li><i>Loza, Nicolas; Elvira Torres, Diego Enrique; Coca Rios, Itzel, 2025, 
                                        PIESM (2024) nivel estado-eleccion, https://doi.org/10.7910/DVN/60BT7S, Harvard Dataverse, V1</i></li>
    <li><i>Loza, Nicolas; Elvira Torres, Diego Enrique; Coca Rios, Itzel, 2025,
                                        PIESM (2024) nivel experto(a), https://doi.org/10.7910/DVN/ZETGPH, Harvard Dataverse, V1</i></li>
                                        </ul>
                  "),
             br(),
             br(),
             p(" - Visualizador elaborado por Diego Elvira - "),
HTML("[![DOI](https://zenodo.org/badge/1067623785.svg)](https://doi.org/10.5281/zenodo.17241011)")
    ),
    
    tabPanel("Resumen",
             h3("Elecciones analizadas (2024)"),
             div(style = "display:flex; justify-content:center;",
                 tableOutput("tabla")
             ),
             br(),
             selectInput("tipo", "Selecciona tipo de elección", choices = tipos),
             div(style = "display:flex; justify-content:center;",
                 tableOutput("tabla2")
             ),
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
    ),
    
    tabPanel("Entidades",
             h3("Indicadores por entidad/elección (con valores imputados)"),
             selectInput("entidad", "Selecciona por entidad", choices = entidades),
             div(style = "display:flex; justify-content:center;",
                 plotOutput("graf4")
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
      select(estado, !!sym(input$var)) %>% 
      arrange(desc(!!sym(input$var)))
    
    valores <- datos[[input$var]]
    nombres <- datos$estado
    
    par(mar = c(10, 4, 4, 2) + 0.1)
    
    bp <- barplot(valores,
                  names.arg = nombres,
                  las = 2,
                  col = "#0029a3",
                  border = "grey50",
                  ylim = c(0, 100),
                  main = paste("Puntuación por entidad del", input$var),
                  ylab = "Puntuación")
    
    abline(h = seq(0, 80, by = 20), col = "gray50", lty = "dotted")
    abline(h = mean(valores,na.rm=T), col = adjustcolor("red1", alpha=0.75), lty = "dotted", lwd=2)
    
    text(x = bp, 
         y = (valores / 20)*22,
         labels = round(valores, 1),
         cex = 1.25, font = 2, col = "gray50", srt = 90)
  })
  
  # Gráfico con imputación
  output$graf3 <- renderPlot({
    datos <- base %>%
      filter(`Tipo.de.elección` == input$tipo3) %>%
      select(estado, !!sym(input$var2)) %>% 
      arrange(desc(!!sym(input$var2)))
    
    valores <- datos[[input$var2]]
    nombres <- datos$estado
    
    par(mar = c(10, 4, 4, 2) + 0.1)
    
    bp <- barplot(valores,
                  names.arg = nombres,
                  las = 2,
                  col = "#0029a3",
                  border = "grey50",
                  ylim = c(0, 100),
                  main = paste("Puntuación por entidad del", input$var2),
                  ylab = "Puntuación")
    
    abline(h = seq(0, 80, by = 20), col = "gray50", lty = "dotted")
    abline(h = mean(valores,na.rm=T), col = adjustcolor("red1", alpha=0.75), lty = "dotted", lwd=2)
    
    text(x = bp, 
         y = (valores / 20)*22,
         labels = round(valores, 1),
         cex = 1.25, font = 2, col = "gray50", srt = 90)
  })
  

  
# Gráfico por entidades
output$graf4 <- renderPlot({
    req(input$entidad)
    
    datos <- base_larga %>%
      filter(estado == input$entidad) %>% 
      arrange(desc(value))
    
    valores <- datos$value
    nombres <- as.character(datos$var)
    
    par(mar = c(10, 4, 4, 2) + 0.1)
    
    bp <-
      barplot(valores,
                  names.arg = nombres,
                  las = 2,
                  col = ifelse(nombres == "PEIIndexp_i", "grey40", "#0029a3"),
                  border = "grey50",
                  ylim = c(0, 100),
                  main = paste("Puntuación por entidad del", input$entidad),
                  ylab = "Puntuación")
    
    abline(h = seq(0, 80, by = 20), col = "gray50", lty = "dotted")
    # abline(h = mean(valores,na.rm=T), col = adjustcolor("red1", alpha=0.75), lty = "dotted", lwd=2)
    # 
    text(x = bp,
         y = (valores / 20)*22,
         labels = round(valores, 1),
         cex = 1.25, font = 2, col = "gray50", srt = 0)
  })
  
  
  
}

shinyApp(ui, server)

# library(shinylive)
#
# # Exporta la app a HTML y recursos estáticos
# shinylive::export(appdir = ".", destdir = "docs")

