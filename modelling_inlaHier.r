library(arrow)
library(dplyr)

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

# Grafo para INLA:
g <- inla.read.graph(filename = "data/mapa_pr.adj")

#Modelo Hierarquico - Efeitos
summary(data)

#Ajustando a variável resposta de booleano para numérica
data$cardiopatias_congenitas <- as.numeric(data$cardiopatias_congenitas)

data$mun_id <- as.numeric(as.factor(data$CODMUNRES))
data$mun_id2 <- as.numeric(as.factor(data$CODMUNRES))

#Base reduzida
set.seed(123)

# Separar casos e controles
casos <- data[data[[variavel_resposta]] == TRUE, ]
controles <- data[data[[variavel_resposta]] == FALSE, ]
n_controles <- nrow(casos) * proporcao_controles

# Amostrar controles
controles_amostra <- controles[sample(nrow(controles), n_controles), ]

# Combinar
data_balanceado <- rbind(casos, controles_amostra)

# Padronizar
data_balanceado <- data_balanceado %>%
  mutate(ANO = scale(ANO),
         IDADEMAE = scale(IDADEMAE),
         PESO = scale(PESO),
         idhm = scale(idhm))

formula_hier1 <- cardiopatias_congenitas ~ 
  IDADEMAE + ESCMAE + CONSULTAS +
  idhm +                         
  factor(ANO_NASC) +             
  f(mun_id, model = "besag", graph = g)

t1 <- Sys.time()

#Modelos
modelo_hier <- inla(
  formula_hier1,
  family = "binomial",
  data = data,
  control.predictor = list(compute = FALSE),  
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    return.marginals.predictor = FALSE
  ),
  control.fixed = list(mean = 0, prec = 0.001)
)

t2 <- Sys.time()
t2 - t1

# Resultados
summary(modelo_hier2)
saveRDS(modelo_hier, "data/modelo_hier.rds")
modelo_hier2 <- readRDS("data/modelo_hier.rds")
rm(modelo_hier2)
fixed_effects <- modelo_hier$summary.fixed
fixed_effects$variavel <- rownames(fixed_effects)

# Calcular OR e IC
fixed_effects$OR <- exp(fixed_effects$mean)
fixed_effects$IC_2.5 <- exp(fixed_effects$`0.025quant`)
fixed_effects$IC_97.5 <- exp(fixed_effects$`0.975quant`)

# Adicionar indicador de significância
fixed_effects$significativo <- ifelse(
  fixed_effects$`0.025quant` > 0 | fixed_effects$`0.975quant` < 0, 
  "*", ""
)

# Criar tabela final
tabela_resultados <- fixed_effects[, c("variavel", "mean", "OR", "IC_2.5", "IC_97.5", "significativo")]
tabela_resultados$IC_texto <- paste0("(", round(tabela_resultados$IC_2.5, 3), 
                                     ", ", round(tabela_resultados$IC_97.5, 3), ")")

print(tabela_resultados)

#Forest Plot
vars_interesse <- tabela_resultados[!grepl("Intercept|factor", tabela_resultados$variavel), ]

ggplot(vars_interesse, aes(x = OR, y = reorder(variavel, OR))) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = IC_2.5, xmax = IC_97.5), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", linewidth = 0.8) +
  scale_x_log10() +
  theme_minimal() +
  labs(x = "Odds Ratio (escala log)", 
       y = "", 
       title = "Efeito das Covariáveis sobre Cardiopatias Congênitas",
       subtitle = "Modelo Hierárquico Espacial - INLA")

# Mapa Efeito Espacial
# Efeitos aleatórios do besag
efeitos_mun <- modelo_hier$summary.random$mun_id

# Converter para Risco Relativo (exponenciar)
efeitos_mun$RR <- exp(efeitos_mun$mean)
efeitos_mun$RR_2.5 <- exp(efeitos_mun$`0.025quant`)
efeitos_mun$RR_97.5 <- exp(efeitos_mun$`0.975quant`)

pr_shape <- readRDS('light_data/municipal.rds')
pr_shape$mun_id <- as.numeric(as.factor(pr_shape$CC_2))

#Unir shapefile
correspondencia <- unique(data[, c("CODMUNRES", "mun_id")])
correspondencia <- correspondencia[order(correspondencia$mun_id), ]

pr_shape <- merge(pr_shape, correspondencia, 
                  by.x = "CC_2", by.y = "CODMUNRES",
                  all.x = TRUE)

pr_shape <- merge(pr_shape, efeitos_mun[, c("ID", "RR", "RR_2.5", "RR_97.5")],
                  by.x = "mun_id.x", by.y = "ID")

#Mapa
ggplot(data = pr_shape) +
  geom_sf(aes(fill = RR)) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 1, name = "Risco Relativo"
  ) +
  theme_minimal() +
  labs(title = "Risco Relativo de Cardiopatias Congênitas por Município",
       subtitle = "Modelo Hierárquico Espacial - INLA")

# Identificar municípios com efeito significativo
pr_shape$efeito_sig <- with(pr_shape, 
                            ifelse(RR_2.5 > 1, "Aumento de Risco",
                                   ifelse(RR_97.5 < 1, "Redução de Risco", 
                                          "Não Significativo")))

# Mapa com significância
ggplot(data = pr_shape) +
  geom_sf(aes(fill = efeito_sig)) +
  scale_fill_manual(
    values = c("Aumento de Risco" = "red",
               "Redução de Risco" = "blue",
               "Não Significativo" = "grey80"),
    name = "Efeito"
  ) +
  theme_minimal() +
  labs(title = "Significância do Efeito Espacial",
       subtitle = "Intervalo de Credibilidade 95%")

# Modelo Mais Complexo (Trava)
formula_hier2 <- cardiopatias_congenitas ~ 
  IDADEMAE + ESCMAE + CONSULTAS + # Covariáveis individuais
  idhm + # Covariáveis municipais
  f(mun_id, model = "besag", graph = g) + # Efeito espacial estruturado (ICAR)
  f(mun_id2, model = "iid") + # Efeito não-espacial (iid)
  f(ANO_NASC, model = "rw1") # Tendência temporal