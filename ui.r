library(shiny)

#Parâmetros da UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  
  titlePanel(h2("Análise de Anomalias Congênitas", align="center")),
  
  fluidRow(
    column(4,
           selectInput("tipo_analise", "Tipo de análise:",
                       choices = c("Univariado", "Bivariado", "Temporal"))
    ),
    column(4,
           selectInput("anomalia", "Selecione a AC:",
                       choices = ac_agrupadas,
                       selected = ac_agrupadas[1])
    ),
    column(4,
           selectInput("variavel", "Selecione variável explicativa:",
                       choices = c(var_num, var_cat),
                       selected = var_num[1])
    )
  ),
  
  conditionalPanel(
    condition = "input.tipo_analise == 'Temporal'",
    fluidRow(
      column(
        width = 12,
        uiOutput("titulo_mapa"),
        fluidRow(
          column(4,
                 selectInput("nivel_geo", "Nível geográfico:",
                             choices = c("Municipal", "Regional", "Mesoregional", "Macroregional"),
                             selected = "Municipal")
          ),
          column(8,
                 sliderInput("ano_mapa", "Selecione o ano:",
                             min = 2013, max = 2022, value = 2013,
                             step = 1, sep = "", animate = TRUE)
          )
        ),
        leafletOutput("mapa_anomalia", height = "600px")
      )
    )
  ),
  
  hr(),
  
  uiOutput("painel_graficos")
)