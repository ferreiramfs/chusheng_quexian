library(gtsummary)
library(kableExtra)
library(flextable)
library(fst)

dados <- read_fst("data/dados_comprimidos.fst")

variaveis_explicativas <- c('LOCNASC', 'IDADEMAE', 'ESTCIVMAE', 'ESCMAE', 'QTDGESTANT', 'GRAVIDEZ',
                            'PARTO', 'CONSULTAS', 'SEXO', 'RACACORMAE', 'PESO', 'SEMAGESTAC')

indicadores <- c('idhm', 'idhm_educacao', 'idhm_longevidade', 'idhm_renda', 'porcentagem_da_populacao_baixa_renda', 
                 'cobertura_bcg', 'mortalidade', 'renda_domiciliar_per_capita', 'taxa_de_analfabetismo')

vars <- c(variaveis_explicativas, indicadores)

anomalias <- c('Cardiopatias Congênitas', 'Fendas Orais', 'Órgãos Genitais', 'Defeitos de Parede Abdominal')

#Teste Qui-Quadrado
tabela <- dados %>%
  select(anomalias[1], variaveis_explicativas) %>%
  tbl_summary(
    by = anomalias[1],
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing = "ifany",
    missing_text = "Valores Ausentes"
  ) %>%
  add_p(test = all_categorical() ~ "chisq.test") %>%
  add_overall() %>%
  modify_header(label ~ "**Característica**")

tabela_fo <- dados %>%
  select(anomalias[2], variaveis_explicativas) %>%
  tbl_summary(
    by = anomalias[2],
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing = "ifany",
    missing_text = "Valores Ausentes"
  ) %>%
  add_p(test = all_categorical() ~ "chisq.test") %>%
  add_overall() %>%
  modify_header(label ~ "**Característica**")

tabela_og <- dados %>%
  select(anomalias[3], variaveis_explicativas) %>%
  tbl_summary(
    by = anomalias[3],
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing = "ifany",
    missing_text = "Valores Ausentes"
  ) %>%
  add_p(test = all_categorical() ~ "chisq.test") %>%
  add_overall() %>%
  modify_header(label ~ "**Característica**")

tabela_dpa <- dados %>%
  select(anomalias[4], variaveis_explicativas) %>%
  tbl_summary(
    by = anomalias[4],
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing = "ifany",
    missing_text = "Valores Ausentes"
  ) %>%
  add_p(test = all_categorical() ~ "chisq.test") %>%
  add_overall() %>%
  modify_header(label ~ "**Característica**")

print(tabela_dpa)
