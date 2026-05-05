#################################################################################
#----------------------------Preparacao dos dados-------------------------------#
#################################################################################
library(arrow)
library(dplyr)
library(lme4)
library(ggplot2)
library(patchwork)
library(viridis)

library(car)
library(arm)
library(lmtest)
library(statmod)
library(generalhoslem)
library(lmtest)
library(hnp)

data <- read_parquet("data/model_data.parquet")

data$ANO <- as.numeric(data$ANO_NASC)
data$MES <- as.numeric(data$MES_NASC)

#Removendo os 2 casos com ignorado para QTDFILMORT
data <- data %>% 
  filter(QTDFILMORT != "Ignorado")

summary(data)

#Amostrando dados para HNP
set.seed(123)
casos <- data[data$`Defeito do Tubo Neural` == 1, ]
controles <- data[data$`Defeito do Tubo Neural` == 0, ]

n_controles_amostra <- nrow(casos) * 100
controles_sub <- controles[sample(nrow(controles), n_controles_amostra), ]
data_hnp <- rbind(casos, controles_sub)
#################################################################################
#-----------------------(1)Modelo Inicial Atemporal-----------------------------#
#################################################################################
ajuste_log1 <- glm(`Defeito do Tubo Neural` ~ LOCNASC + IDADEMAE + ESTCIVMAE + ESCMAE + 
                     QTDGESTANT + QTDFILMORT + GRAVIDEZ + PARTO + CONSULTAS + SEXO + 
                     RACACORMAE + PESO + SEMAGESTAC + PANDEMIA + idhm_educacao + 
                     idhm_longevidade + idhm_renda + mortalidade + cobertura_bcg +
                     porcentagem_da_populacao_baixa_renda + taxa_de_analfabetismo + grau_urbanizacao,
                     family = binomial(link = 'logit'),
                     data = data)

summary(ajuste_log1)

#Multicolinearidade
vif(ajuste_log1)

#Resíduos quantílicos normalizados aleatorizados:
residuos <- qresiduals(ajuste_log1)
qqnorm(residuos)
qqline(residuos)

plot(fitted(ajuste_log1), residuos)

#Teste de Hosmer-Lemeshow (Qualidade do Ajuste)
obs_modelo <- ajuste_log1$y
pred_modelo <- fitted(ajuste_log1)

CH_test <- logitgof(obs_modelo, pred_modelo, g = 10)
print(CH_test)

#Curva de calibração
CH_test_obs <- CH_test$observed[,2]
CH_test_exp <- CH_test$expected[,2]

data_calibra <- data.frame(Esperado = CH_test_exp, Observado = CH_test_obs)

ggplot(data = data_calibra, aes(x = Esperado, y = Observado))+
  geom_point(size = 2)+
  theme_bw(base_size = 14)+
  geom_abline(intercept = 0, slope = 1, col = 'red', linewidth = 1)

#Reajustando modelo com HNP
ajuste_hnp <- update(ajuste_log1, data = data_hnp)
hnp(ajuste_hnp, sim = 99, conf = 0.95, main = "HNP - Amostra Estratificada")
#################################################################################
#-------------------------(2)ANO_NASC + MES_NASC--------------------------------#
#################################################################################
ajuste_temp3 <- glm(`Defeito do Tubo Neural` ~ LOCNASC + IDADEMAE + ESTCIVMAE + ESCMAE + 
                      QTDGESTANT + QTDFILMORT + GRAVIDEZ + PARTO + CONSULTAS + SEXO + 
                      RACACORMAE + PESO + SEMAGESTAC + PANDEMIA + idhm_educacao + 
                      idhm_longevidade + idhm_renda + mortalidade + cobertura_bcg +
                      porcentagem_da_populacao_baixa_renda + taxa_de_analfabetismo + 
                      grau_urbanizacao + ANO_NASC + MES_NASC,
                    family = binomial(link = 'logit'),
                    data = data)

summary(ajuste_temp3)

vif(ajuste_temp3)

#Resíduos quantílicos normalizados aleatorizados:
residuos <- qresiduals(ajuste_temp3)
qqnorm(residuos)
qqline(residuos)
plot(fitted(ajuste_temp3), residuos)

#Teste de Hosmer-Lemeshow (Qualidade do Ajuste)
obs_modelo <- ajuste_temp3$y
pred_modelo <- fitted(ajuste_temp3)

CH_test <- logitgof(obs_modelo, pred_modelo, g = 10)
print(CH_test)

#Curva de calibração
CH_test_obs <- CH_test$observed[,2]
CH_test_exp <- CH_test$expected[,2]

data_calibra <- data.frame(Esperado = CH_test_exp, Observado = CH_test_obs)

ggplot(data = data_calibra, aes(x = Esperado, y = Observado))+
  geom_point(size = 2)+
  theme_bw(base_size = 14)+
  geom_abline(intercept = 0, slope = 1, col = 'red', linewidth = 1)

#HNP com amostragem
ajuste_hnp <- update(ajuste_temp3, data = data_hnp)
hnp(ajuste_hnp, sim = 99, conf = 0.95, main = "HNP - Amostra Estratificada")

#################################################################################
#---------------------------(3)GLMM Regionais-----------------------------------#
#################################################################################
shape_regional <- readRDS("light_data/regional.rds")

shape_regional <- shape_regional %>%
  mutate(REGIONA = as.numeric(REGIONA))

data$regional <- sprintf("%02d", data$regional)

balancear_base <- function(data, variavel_resposta, proporcao_controles, seed = 123) {
  
  set.seed(seed)
  
  # Separar casos e controles
  casos <- data[data[[variavel_resposta]] == TRUE, ]
  controles <- data[data[[variavel_resposta]] == FALSE, ]
  
  # Calcular número de controles necessários
  n_controles <- nrow(casos) * proporcao_controles
  
  # Verificar se há controles suficientes
  if (n_controles > nrow(controles)) {
    warning(paste("Apenas", nrow(controles), "controles disponíveis. Usando todos."))
    n_controles <- nrow(controles)
  }
  
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
  
  # Mensagem de diagnóstico
  cat("Base balanceada e padronizada criada:\n")
  print(table(data_balanceado[[variavel_resposta]]))
  cat(paste("Total de linhas:", nrow(data_balanceado), "\n"))
  cat(paste("Proporção casos/controles:", round(nrow(casos)/n_controles, 3), "\n"))
  
  return(data_balanceado)
}

extrair_efeitos_mapa <- function(modelo) {
  
  # Extrair efeitos aleatórios
  ranef_lme4 <- ranef(modelo)$regional
  
  # Criar dataframe com efeitos
  efeitos_regional <- data.frame(
    regional = as.numeric(rownames(ranef_lme4)),  # Converter para numérico para ordenar
    efeito_aleatorio = as.numeric(ranef_lme4[, 1])
  )
  
  # Ordenar por regional
  efeitos_regional <- efeitos_regional[order(efeitos_regional$regional), ]
  
  # Extrair intercepto fixo
  intercepto_fixo <- fixef(modelo)["(Intercept)"]
  
  # Calcular log-odds e probabilidades
  efeitos_regional$log_odds <- intercepto_fixo + efeitos_regional$efeito_aleatorio
  efeitos_regional$probabilidade <- plogis(efeitos_regional$log_odds)
  
  return(efeitos_regional)
}

data_reduzido <- balancear_base(data, "Cardiopatias Congênitas", proporcao_controles = 10)

glmm_cardio <- glmer(`Cardiopatias Congênitas` ~ LOCNASC + IDADEMAE + ESTCIVMAE + ESCMAE + 
                       QTDGESTANT + QTDFILMORT + GRAVIDEZ + PARTO + CONSULTAS + SEXO + 
                       RACACORMAE + PESO + SEMAGESTAC + PANDEMIA + idhm + ANO + 
                       (1 | regional), 
                     family = binomial(link = 'logit'),
                     data = data_reduzido,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 100000)))

summary(glmm_cardio)

efeitos_regional <- extrair_efeitos_mapa(glmm_cardio)

ggplot(shape_regional) +
  geom_sf(aes(fill = efeitos_regional$probabilidade), color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = median(efeitos_regional$probabilidade),
                       labels = scales::percent) +
  labs(title = "Probabilidade de Cardiopatia Congênita por Regional")

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

# Lista com os novos nomes
anomalias <- c(
  "defeito_tubo_neural",
  "microcefalia",
  "cardiopatias_congenitas",
  "fendas_orais",
  "orgaos_genitais",
  "defeitos_membros",
  "defeitos_parede_abdominal",
  "sindrome_down"
)

# Função rodar anomalia (agora com nomes simples)
rodar_anomalia <- function(anomalia, data, proporcao) {
  data_bal <- balancear_base(data, anomalia, proporcao)
  
  formula_str <- paste0(anomalia, " ~ LOCNASC + IDADEMAE + ESTCIVMAE + ESCMAE + 
                        QTDGESTANT + QTDFILMORT + GRAVIDEZ + PARTO + 
                        CONSULTAS + SEXO + RACACORMAE + PESO + SEMAGESTAC + 
                        PANDEMIA + idhm + ANO + (1 | regional)")
  
  modelo <- glmer(as.formula(formula_str),
                  family = binomial(link = 'logit'),
                  data = data_bal, nAGQ = 0,
                  control = glmerControl(optimizer = "bobyqa"))
  
  extrair_efeitos_mapa(modelo)
}

# Rodar para todas
todos_efeitos <- lapply(anomalias, function(x) {
  cat("\nProcessando:", x, "\n")
  df <- rodar_anomalia(x, data, proporcao = 50)
  df$anomalia <- x
  df
}) |> bind_rows()

# Juntar efeitos ao shape (usando REGIONA como chave)
shape_plot <- shape_regional %>%
  left_join(todos_efeitos, by = c("REGIONA" = "regional"))
shape_plot$odds_ratio <- exp(shape_plot$efeito_aleatorio)

#Escalas locais
mapas <- list()
for(i in seq_along(anomalias)) {
  anom <- anomalias[i]
  
  # Calcular odds ratio
  shape_anom <- shape_plot %>% filter(anomalia == anom)
  shape_anom$odds_ratio <- exp(shape_anom$efeito_aleatorio)
  
  # Criar mapa individual com viridis
  mapas[[i]] <- ggplot(shape_anom) +
    geom_sf(aes(fill = odds_ratio), color = "white", size = 0.2) +
    scale_fill_viridis_c(option = "viridis", trans = "log2",
                         name = "Odds Ratio") +
    labs(title = anom) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(size = 10, face = "bold"))
}

# Juntar mapas em grid
mapa_final <- wrap_plots(mapas, ncol = 3)
print(mapa_final)

shape_plot$odds_ratio <- exp(shape_plot$efeito_aleatorio)

ggplot(shape_plot) +
  geom_sf(aes(fill = odds_ratio), color = "white", size = 0.2) +
  facet_wrap(~anomalia, ncol = 3) +
  scale_fill_viridis_c(option = "viridis", trans = "log2",
                       name = "Odds Ratio") +
  labs(title = "Risco Relativo por Regional (ajustado)",
       subtitle = "OR = 1 = risco médio | OR > 1 = maior risco | OR < 1 = menor risco") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 10),
        axis.text = element_blank(),
        axis.title = element_blank())
