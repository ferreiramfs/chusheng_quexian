library(fst)
library(car)
library(polycor)

data <- read_fst("data/dados_comprimidos.fst")
summary(data)

variaveis_explicativas <- c('CODMUNRES', 'LOCNASC', 'IDADEMAE', 'ESTCIVMAE', 'ESCMAE', 'CODOCUPMAE', 'QTDGESTANT', 'GRAVIDEZ',
                            'PARTO', 'CONSULTAS', 'SEXO', 'APGAR1', 'APGAR5', 'RACACORMAE', 'PESO', 'SEMAGESTAC', 'IDADEPAI')

indicadores <- c('idhm', 'idhm_educacao', 'idhm_longevidade', 'idhm_renda', 'porcentagem_da_populacao_baixa_renda', 
                 'cobertura_bcg', 'mortalidade', 'renda_domiciliar_per_capita', 'taxa_de_analfabetismo')

anomalias <- c('Defeito do Tubo Neural', 'Microcefalia', 'Cardiopatias Congênitas', 'Fendas Orais', 'Órgãos Genitais', 
               'Defeitos de Membros', 'Defeitos de Parede Abdominal', 'Síndrome de Down')

vars <- c('LOCNASC', 'IDADEMAE', 'ESTCIVMAE', 'ESCMAE', 'QTDGESTANT', 'GRAVIDEZ', 'PARTO'
          , 'CONSULTAS', 'SEXO', 'APGAR1', 'APGAR5', 'RACACORMAE', 'PESO', 'SEMAGESTAC')

data_reduzido <- data[, c('Defeitos de Membros', vars)]

ajuste_logito <- glm(`Defeito do Tubo Neural` ~ SEXO + IDADEMAE + PESO + APGAR5 
                     + LOCNASC + ESTCIVMAE + ESCMAE + QTDGESTANT + GRAVIDEZ + PARTO
                     + CONSULTAS + APGAR1 + RACACORMAE + SEMAGESTAC,
                     family = binomial(link = 'logit'),
                     data = data_reduzido)

summary(ajuste_logito)
vif(ajuste_logito)
matriz_hetero <- hetcor(data_reduzido)$correlation