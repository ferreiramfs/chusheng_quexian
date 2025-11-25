#Pacotes
packages_list <- c("shiny", "bslib", "thematic", "tidyverse", "gitlink"
                   , "bslib", "leaflet", "geobr", "dplyr", "sf")

lapply(packages_list, library, character.only = TRUE)

data <- read_csv("data/dados_finais.csv")
prev_mun <- read_csv("data/prev_mun.csv")
prev_rm <- read_csv("data/prev_reg.csv")

source("data_transform.r")

data <- transform_data(data)

#Removendo 2 outliers de ULTMENST
data$ULTMENST[data$ULTMENST == 22518] <- NA
data$ULTMENST[data$ULTMENST == 30448] <- NA

ac_agrupadas <- c("Defeito do tubo Neural", "Microcefalia", "Cardiopatias congenitas", "Fendas Orais", "Órgãos genitais", "Defeitos de membros", "Defeitos de parede abdominal", "Sindrome de Dow")

#Variáveis Contínuas
var_num <- c('idhm', 'idhm_educacao', 'idhm_longevidade', 'idhm_renda', 'porcentagem_da_populacao_baixa_renda'
             , 'renda_domiciliar_per_capita', 'mortalidade', 'taxa_de_analfabetismo', 'cobertura_bcg'
             , 'IDADEMAE', 'PESO', 'IDADEPAI', 'ULTMENST')

#Variáveis Categóricas ou Discretas com poucos valores
var_cat <- c('ESCMAE', 'SEXO', 'RACACORMAE', 'ESTCIVMAE', 'CONSULTAS', 'APGAR1', 'APGAR5'
             , 'LOCNASC', 'CODOCUPMAE', 'QTDFILVIVO', 'QTDFILMORT', 'QTDGESTANT',
             'QTDPARTCES', 'QTDPARTNOR', 'GRAVIDEZ', 'PARTO', 'SEMAGESTAC', 'GESTACAO')

#Tema ggplot
ggplot2::theme_set(ggplot2::theme_minimal())

#Aplica o CSS do Shiny app nos plots do ggplot
thematic_shiny()

tam_fonte_eixos <- 10

pr_mun   <- read_municipality(code_muni = 41, year = 2020) %>% st_transform(4326)

pr_reg   <- read_health_region(year = 2013) %>% 
  filter(abbrev_state == "PR") %>% st_transform(4326)

source("ui.r")
source("server.r")

#Criar o Shiny APP
shinyApp(ui = ui, server = server)
