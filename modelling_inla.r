library(arrow)
library(readxl)
library(dplyr)
library(stringi)

library(SpatialEpi)
library(spdep)
library(INLA)
library(sf)
library(ggplot2)

data <- read_parquet("data/model_data.parquet")
idh_municipios <- read.csv2("data/indicadores/idhm.csv")

data$ANO <- as.numeric(data$ANO_NASC)
data$MES <- as.numeric(data$MES_NASC)

data <- data %>%
  rename(
    defeito_tubo_neural = `Defeito do Tubo Neural`,
    microcefalia = Microcefalia,
    cardiopatias_congenitas = `Cardiopatias Congênitas`,
    fendas_orais = `Fendas Orais`,
    orgaos_genitais = `Órgãos Genitais`,
    defeitos_membros = `Defeitos de Membros`,
    defeitos_parede_abdominal = `Defeitos de Parede Abdominal`,
    sindrome_down = `Síndrome de Down`
)

#Removendo os 2 casos com ignorado para QTDFILMORT
data <- data %>% 
  filter(QTDFILMORT != "Ignorado")

regioes <- read.csv('data/regioes.csv')

regioes <- regioes %>%
  mutate(municipio = stri_trans_general(toupper(municipio), "Latin-ASCII")) %>%
  distinct(municipio, .keep_all = TRUE)

#----Modelo Espaço-Temporal INLA----
mapa_pr <- readRDS("light_data/municipal.rds")

mapa_pr <- mapa_pr %>%
  mutate(CC_2 = substr(CC_2, 1, 6))

pop_pr <- read_xlsx("data/pop_pr.xlsx")

pop_pr <- data.frame(
  NOMEMUN = stri_trans_general(toupper(pop_pr$NOMEMUN), "Latin-ASCII"),
  POP = pop_pr$POP_2022,
  ANO = rep(2013:2022, each = 399)
)

pop_pr <- pop_pr %>%
  left_join(regioes %>% select('municipio', 'cod'), by = c("NOMEMUN" = "municipio")) %>%
  mutate(cod = as.character(cod)) %>%
  mutate(cod = substr(cod, 1, 6))

casos_pr <- data %>%
  filter(cardiopatias_congenitas == TRUE) %>%
  group_by(ANO, CODMUNRES) %>%
  mutate(ANO = ANO + 2012) %>%
  summarise(Total = n())

anom_pr <- pop_pr %>%
  left_join(casos_pr, by = c("ANO", "cod" = "CODMUNRES"),
            multiple = "first") %>%
  mutate(Total = case_when(is.na(Total) ~ 0,
                           .default = Total)) %>%
  left_join((mapa_pr %>%
               mutate(cod = CC_2) %>%
               select(cod)), multiple = "first") %>%
  st_as_sf()

summary(anom_pr)

# Valores esperados de anomalias por ano:
E_anom_ano <- numeric()

for (i in 2013:2022) {
  E_anom_ano <- expected(
    (filter(anom_pr, ANO == i))$POP,
    (filter(anom_pr, ANO == i))$Total,
    1
  ) %>%
    c(E_anom_ano, .)
}

anom_pr$E <- E_anom_ano

# Matriz de adjacência entre os municípios:
nb_pr <- poly2nb(anom_pr)

nb2INLA("data/mapa_pr.adj", nb_pr)

# Grafo para INLA:
g <- inla.read.graph(filename = "data/mapa_pr.adj")

summary(anom_pr)

#Fórmula
anom_pr$u <- as.numeric(as.factor(anom_pr$cod))

formula_1 <- Total ~ f(u, model = "bym", graph = g)

formula_2 <- Total ~ f(u, model = "bym", graph = g) +
  f(ANO_NASC, model = "rw1")

modelo_0 <- inla(formula_1,
                 family = "zeroinflatedpoisson0",
                 data = anom_pr,
                 E = E,
                 control.predictor = list(compute = TRUE, link = 1),
                 control.compute = list(return.marginals.predictor = TRUE,
                                        dic = TRUE))

modelo_1 <- inla(formula_1,
                 family = "zeroinflatedpoisson1",
                 data = anom_pr,
                 E = E,
                 control.predictor = list(compute = TRUE, link = 1),
                 control.compute = list(return.marginals.predictor = TRUE,
                                        dic = TRUE))

modelo_0$dic$dic
modelo_1$dic$dic