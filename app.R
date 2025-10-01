



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
  titlePanel("PIE México 2024"),
  
  tabsetPanel(
    tabPanel("Inicio",
             h2("Proyecto de Integridad Electoral Subnacional en México 2024"),
             p("Este visualizador presenta los resultados de la encuesta a expertos del PIE México 2024."),
             p("De 2015 a 2024 se ha calibrado la integridad de 142 procesos electorales locales entrevistando a 1,703 expertas/os."),
             p("En esta versión se muestran tablas y gráficos en R base, sin mapas, para simplificar la instalación.")
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
                      h3("Resultados sin imputación"),
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
      summarise(n = n(),
                n_exp = sum(numresponses),
                tasa = sum(numresponses) / sum(contacted)) %>%
      ungroup()
  })
  
  # Tabla detalle por tipo
  output$tabla2 <- renderTable({
    base %>%
      filter(`Tipo.de.elección` == input$tipo) %>%
      select(estado)
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
            col = "steelblue",
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


