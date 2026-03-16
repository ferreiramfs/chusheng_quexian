library(fst)
library(car)
#library(statmod)
library(ggplot2)
library(generalhoslem)
#library(ResourceSelection)

data <- read_fst("data/dados_comprimidos.fst")
summary(data)

variaveis_explicativas <- c('LOCNASC', 'IDADEMAE', 'ESTCIVMAE', 'ESCMAE', 'QTDGESTANT', 'GRAVIDEZ',
                            'PARTO', 'CONSULTAS', 'SEXO', 'APGAR1', 'APGAR5', 'RACACORMAE', 'PESO', 'SEMAGESTAC')

indicadores <- c('idhm', 'idhm_educacao', 'idhm_longevidade', 'idhm_renda', 'porcentagem_da_populacao_baixa_renda', 
                 'cobertura_bcg', 'mortalidade', 'renda_domiciliar_per_capita', 'taxa_de_analfabetismo')

anomalias <- c('Defeito do Tubo Neural', 'Microcefalia', 'Cardiopatias Congênitas', 'Fendas Orais', 'Órgãos Genitais', 
               'Defeitos de Membros', 'Defeitos de Parede Abdominal', 'Síndrome de Down')

vars <- c(variaveis_explicativas, indicadores)

ajuste_logito <- glm(`Defeito do Tubo Neural` ~ LOCNASC + IDADEMAE + ESTCIVMAE + 
                       ESCMAE + QTDGESTANT + GRAVIDEZ + PARTO + CONSULTAS + 
                       SEXO + APGAR1 + APGAR5 + RACACORMAE + PESO + SEMAGESTAC + 
                       idhm + idhm_educacao + idhm_longevidade + idhm_renda + 
                       porcentagem_da_populacao_baixa_renda + cobertura_bcg + mortalidade + 
                       renda_domiciliar_per_capita + taxa_de_analfabetismo,
                     family = binomial(link = 'logit'),
                     data = data)

summary(ajuste_logito)

#Multicolinearidade
vif(ajuste_logito)

#Pontos Influentes/Outliers
influenceIndexPlot(ajuste_logito, vars = c('Studentized','Cook','Hat'), id.n = 3, cex = 1.4)

#Resíduos quantílicos normalizados aleatorizados:
residuos <- qresiduals(ajuste_logito)
qqnorm(residuos)
qqline(residuos)
plot(fitted(ajuste_logito), residuos)

#Teste de Hosmer-Lemeshow (Qualidade do Ajuste)
obs_modelo <- ajuste_logito$y
pred_modelo <- fitted(ajuste_logito)

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

#Removendo o IDHM devido a multicolinearidade
ajuste_logito2 <- update(ajuste_logito, ~ . - idhm - renda_domiciliar_per_capita)

summary(ajuste_logito2)

#Multicolinearidade
vif(ajuste_logito2)

#Resíduos quantílicos normalizados aleatorizados:
residuos <- qresiduals(ajuste_logito)
qqnorm(residuos)
qqline(residuos)
plot(fitted(ajuste_logito), residuos)

#Gráfico de resíduos com envelopes simulados
simulations <- simulateResiduals(ajuste_logito, n = 250)
plot(simulationOutput = simulations)
testResiduals(simulations)
plotQQunif(simulations)

#Teste de Hosmer-Lemeshow (Qualidade do Ajuste)
obs_modelo <- ajuste_logito$y
pred_modelo <- fitted(ajuste_logito)

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