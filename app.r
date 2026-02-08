#Pacotes
packages_list <- c("shiny", "bslib", "thematic", "tidyverse", "gitlink"
                   , "bslib", "leaflet", "geobr", "dplyr", "sf")

lapply(packages_list, library, character.only = TRUE)

#Prevalências por agrupamentos regionais
prevalencia_municipal <- read_csv("data/prevalencias/municipal.csv")
prevalencia_regional <- read_csv("data/prevalencias/regional.csv")
prevalencia_mesoregional <- read_csv("data/prevalencias/mesoregional.csv")
prevalencia_macroregional <- read_csv("data/prevalencias/macroregional.csv")

prevalencia_regional <- prevalencia_regional %>%
  mutate(
    regional = sprintf("%02d", regional)
  )

#Arquivos Shape de cada agrupamento regional
shape_municipal <- st_read("data/shapes/municipal.shp")
shape_regional <- st_read("data/shapes/regional.shp")
shape_mesoregional <- st_read("data/shapes/meso.shp")
shape_macroregional <- st_read("data/shapes/macro.shp")

regioes <- read_csv("data/regioes.csv")

data <- read_csv("data/dados_finais.csv")
source("data_transform.r")
data <- transform_data(data)

ac_agrupadas <- c("Defeito do Tubo Neural", "Microcefalia", "Cardiopatias Congênitas", "Fendas Orais"
                  , "Órgãos Genitais", "Defeitos de Membros", "Defeitos de Parede Abdominal", "Síndrome de Down")

#Incluindo indicadora de presença de anomalia
data <- data %>%
  mutate(
    Todas = rowSums(across(all_of(ac_agrupadas), ~ as.logical(.)), na.rm = TRUE) > 0
  )

table(data$"Cardiopatias Congênitas")
ac_agrupadas <- c(ac_agrupadas, c('Todas'))

#Variáveis Contínuas
var_num <- c('idhm', 'idhm_educacao', 'idhm_longevidade', 'idhm_renda', 'porcentagem_da_populacao_baixa_renda'
             , 'renda_domiciliar_per_capita', 'mortalidade', 'taxa_de_analfabetismo', 'cobertura_bcg'
             , 'IDADEMAE', 'PESO', 'IDADEPAI')

#Variáveis Categóricas ou Discretas com poucos valores
var_cat <- c('ESCMAE', 'SEXO', 'RACACORMAE', 'ESTCIVMAE', 'CONSULTAS', 'APGAR1', 'APGAR5'
             , 'LOCNASC', 'CODOCUPMAE', 'QTDGESTANT', 'GRAVIDEZ', 'PARTO', 'SEMAGESTAC')

#Tema ggplot
ggplot2::theme_set(ggplot2::theme_minimal())
thematic_shiny()
tam_fonte_eixos <- 10

source("ui.r")
source("server.r")

#Criar o Shiny APP
shinyApp(ui = ui, server = server)
