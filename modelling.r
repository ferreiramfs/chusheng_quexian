#################################################################################
#----------------------------Preparacao dos dados-------------------------------#
#################################################################################
library(fst)
library(car)
library(arm)
library(lmtest)
library(statmod)
library(ggplot2)
library(generalhoslem)
library(lmtest)
library(hnp)

data <- read_fst("data/dados_comprimidos.fst")
summary(data)

data$ANO_NASC <- as.factor(format(data$DTNASC, "%Y"))
data$MES_NASC <- as.factor(format(data$DTNASC, "%m"))

data_inicio <- min(data$DTNASC)
data$TEMPO <- as.numeric(data$DTNASC - data_inicio + 1)

variaveis_explicativas <- c('LOCNASC', 'IDADEMAE', 'ESTCIVMAE', 'ESCMAE', 'QTDGESTANT', 'GRAVIDEZ',
                            'PARTO', 'CONSULTAS', 'SEXO', 'RACACORMAE', 'PESO', 'SEMAGESTAC')

indicadores <- c('idhm', 'idhm_educacao', 'idhm_longevidade', 'idhm_renda', 'porcentagem_da_populacao_baixa_renda', 
                 'cobertura_bcg', 'mortalidade', 'renda_domiciliar_per_capita', 'taxa_de_analfabetismo')

anomalias <- c('Defeito do Tubo Neural', 'Microcefalia', 'Cardiopatias Congênitas', 'Fendas Orais', 'Órgãos Genitais', 
               'Defeitos de Membros', 'Defeitos de Parede Abdominal', 'Síndrome de Down')

vars <- c(variaveis_explicativas, indicadores)

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
ajuste_log1 <- glm(`Defeito do Tubo Neural` ~ LOCNASC + IDADEMAE + ESTCIVMAE + 
                       ESCMAE + QTDGESTANT + GRAVIDEZ + PARTO + CONSULTAS + 
                       SEXO + RACACORMAE + PESO + SEMAGESTAC + 
                       idhm_educacao + idhm_longevidade + idhm_renda + mortalidade +
                       porcentagem_da_populacao_baixa_renda + cobertura_bcg + taxa_de_analfabetismo,
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
#---------------------(2)ANO_NASC + MES_NASC + TEMPO----------------------------#
#################################################################################
ajuste_temp2 <- glm(`Defeito do Tubo Neural` ~ LOCNASC + IDADEMAE + ESTCIVMAE + 
                       ESCMAE + QTDGESTANT + GRAVIDEZ + PARTO + CONSULTAS + 
                       SEXO + RACACORMAE + PESO + SEMAGESTAC + 
                       idhm_educacao + idhm_longevidade + idhm_renda + 
                       porcentagem_da_populacao_baixa_renda + cobertura_bcg + mortalidade + 
                       + taxa_de_analfabetismo + ANO_NASC + MES_NASC + TEMPO,
                     family = binomial(link = 'logit'),
                     data = data)

summary(ajuste_temp2)

#Multicolinearidade
vif(ajuste_temp2)

#Resíduos quantílicos normalizados aleatorizados:
residuos <- qresiduals(ajuste_temp2)
qqnorm(residuos)
qqline(residuos)
plot(fitted(ajuste_temp2), residuos)

#Teste de Hosmer-Lemeshow (Qualidade do Ajuste)
obs_modelo <- ajuste_temp2$y
pred_modelo <- fitted(ajuste_temp2)

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
ajuste_hnp <- update(ajuste_temp2, data = data_hnp)
hnp(ajuste_hnp, sim = 99, conf = 0.95, main = "HNP - Amostra Estratificada")
#################################################################################
#-------------------------(3)ANO_NASC + MES_NASC--------------------------------#
#################################################################################
ajuste_temp3 <- glm(`Defeito do Tubo Neural` ~ LOCNASC + IDADEMAE + ESTCIVMAE + 
                       ESCMAE + QTDGESTANT + GRAVIDEZ + PARTO + CONSULTAS + 
                       SEXO + RACACORMAE + PESO + SEMAGESTAC + 
                       idhm_educacao + idhm_longevidade + idhm_renda + 
                       porcentagem_da_populacao_baixa_renda + cobertura_bcg + mortalidade + 
                       + taxa_de_analfabetismo + ANO_NASC + MES_NASC,
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
#--------------------------------(4)TEMPO---------------------------------------#
#################################################################################
ajuste_temp4 <- glm(`Defeito do Tubo Neural` ~ LOCNASC + IDADEMAE + ESTCIVMAE + 
                         ESCMAE + QTDGESTANT + GRAVIDEZ + PARTO + CONSULTAS + 
                         SEXO + RACACORMAE + PESO + SEMAGESTAC + 
                         idhm_educacao + idhm_longevidade + idhm_renda + 
                         porcentagem_da_populacao_baixa_renda + cobertura_bcg + mortalidade + 
                         + taxa_de_analfabetismo + TEMPO,
                       family = binomial(link = 'logit'),
                       data = data)

summary(ajuste_temp4)

vif(ajuste_temp4)

#Resíduos quantílicos normalizados aleatorizados:
residuos <- qresiduals(ajuste_temp4)
qqnorm(residuos)
qqline(residuos)
plot(fitted(ajuste_temp4), residuos)

#Teste de Hosmer-Lemeshow (Qualidade do Ajuste)
obs_modelo <- ajuste_temp4$y
pred_modelo <- fitted(ajuste_temp4)

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
ajuste_hnp <- update(ajuste_temp4, data = data_hnp)
hnp(ajuste_hnp, sim = 99, conf = 0.95, main = "HNP - Amostra Estratificada")
#################################################################################
#-----------------------------(5)TEMPO + MES------------------------------------#
#################################################################################
ajuste_temp5 <- glm(`Defeito do Tubo Neural` ~ LOCNASC + IDADEMAE + ESTCIVMAE + 
                      ESCMAE + QTDGESTANT + GRAVIDEZ + PARTO + CONSULTAS + 
                      SEXO + RACACORMAE + PESO + SEMAGESTAC + 
                      idhm_educacao + idhm_longevidade + idhm_renda + 
                      porcentagem_da_populacao_baixa_renda + cobertura_bcg + mortalidade + 
                      + taxa_de_analfabetismo + MES_NASC + TEMPO,
                    family = binomial(link = 'logit'),
                    data = data)

summary(ajuste_temp5)

vif(ajuste_temp5)

#Resíduos quantílicos normalizados aleatorizados:
residuos <- qresiduals(ajuste_temp5)
qqnorm(residuos)
qqline(residuos)
plot(fitted(ajuste_temp5), residuos)

#Teste de Hosmer-Lemeshow (Qualidade do Ajuste)
obs_modelo <- ajuste_temp5$y
pred_modelo <- fitted(ajuste_temp5)

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
ajuste_hnp <- update(ajuste_temp5, data = data_hnp)
hnp(ajuste_hnp, sim = 99, conf = 0.95, main = "HNP - Amostra Estratificada")
