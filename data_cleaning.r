#----------------------------------------Importando bases e arquivos -------------------
packages_list <- c('readxl', 'foreign', 'Epi', 'this.path', 'dplyr', 'stringr', 'tidyr')
lapply(packages_list, library, character.only=TRUE)

setwd(this.path::here())
#utilizando o argumento "as.is=TRUE"para transformar os dados em caracteres
BANCOTOTAL<- read.dbf(file = 'data/BANCO_RES.dbf', as.is = TRUE)

cols_data <- c('DTNASC', 'DTCADASTRO', 'DTULTMENST')
BANCOTOTAL[cols_data] <- lapply(BANCOTOTAL[cols_data], as.Date, format= "%d%m%Y")

BANCOTOTAL$ULTMENST <- as.numeric(BANCOTOTAL$DTNASC - BANCOTOTAL$DTULTMENST)

LISTA_AC<-read_excel(path = 'data/anomalias/LISTA_AC.xlsx',
sheet="COD",
skip=0)

regioes <- read.csv('data/regioes.csv')

#Bases indicadores
idhm <- read.csv('data/indicadores/idhm.csv', sep = ';')
cob_bcg <- read.csv('data/indicadores/cobertura_bcg.csv', sep = ';')
baixa_renda <- read.csv('data/indicadores/porc_baixarenda.csv', sep = ';')
renda_pcap <- read.csv('data/indicadores/renda_dompercap.csv', sep = ';')
analfabetismo <- read.csv('data/indicadores/taxa_analfabetismo.csv', sep = ';')
mortalidade <- read.csv('data/indicadores/taxa_mortalidade.csv', sep = ';')

#----------------------------------------Montando base resumida -------------------

# selecao de variaveis
BANCORESUMIDO <-  BANCOTOTAL |> 
  select(CODMUNNASC, CODESTAB, CODMUNRES, LOCNASC, IDADEMAE, ESTCIVMAE, ESCMAE
         , CODOCUPMAE, QTDFILVIVO, QTDFILMORT,QTDGESTANT, QTDPARTCES, QTDPARTNOR
         , GRAVIDEZ, PARTO, CONSULTAS, DTNASC, SEXO, APGAR1, APGAR5, RACACORMAE
         , PESO, IDANOMAL, CODANOMAL, DTCADASTRO, SEMAGESTAC, GESTACAO, IDADEPAI, ULTMENST)

##Criando variáveis que identificam a presença de cada tipo de Anomalia
#Limpa os códigos na base de tipos (remove o "-descrição")
LISTA_AC[] <- lapply(LISTA_AC, function(x) sub("-.*", "", x))

#Para cada tipo, junta os códigos válidos em um vetor
tipos_lista <- apply(LISTA_AC[-1], 1, function(x) na.omit(x))

names(tipos_lista) <- LISTA_AC$AC

#Substitui NA por string vazia (para evitar erros no str_detect)
BANCORESUMIDO$CODANOMAL[is.na(BANCORESUMIDO$CODANOMAL)] <- ""

#Cria colunas booleanas TRUE/FALSE para cada tipo
for (tipo in names(tipos_lista)) {
  codigos_desse_tipo <- tipos_lista[[tipo]]
  padrao <- paste(codigos_desse_tipo, collapse = "|")  # expressão regex "Q000|M233|W223"
  
  BANCORESUMIDO[[tipo]] <- str_detect(BANCORESUMIDO$CODANOMAL, padrao)
}

regioes <- regioes %>% 
  distinct(regioes$cod, .keep_all = TRUE)

regioes$cod_reduzido <- as.character(regioes$cod_reduzido)

BANCORESUMIDO <- left_join(BANCORESUMIDO, regioes[, c("cod_reduzido", "cod", "macro", "meso", "regional")], by = c("CODMUNRES" = "cod_reduzido"))

cols_data <- c('DTNASC', 'DTCADASTRO')
BANCORESUMIDO[cols_data] <- lapply(BANCORESUMIDO[cols_data], as.Date, format= "%d%m%Y")

BANCORESUMIDO$ANO_NASC <- format(as.Date(BANCORESUMIDO$DTNASC), "%Y")

#Variáveis municipais
indicadores <- left_join(baixa_renda, cob_bcg[, c("municipio", "cobertura_bcg")], by = "municipio")
indicadores <- left_join(indicadores, mortalidade[, c("municipio", "mortalidade")], by = "municipio")
indicadores <- left_join(indicadores, renda_pcap[, c("municipio", "renda_domiciliar_per_capita")], by = "municipio")
indicadores <- left_join(indicadores, analfabetismo[, c("municipio", "taxa_de_analfabetismo")], by = "municipio")

indicadores$cod6 <- sub(" .*", "", indicadores$municipio)

idhm$cod6 <- substr(idhm$codigo, 1, 6)

#Base final de indicadores
indicadores <- left_join(indicadores, idhm[, c("cod6", "idhm", "idhm_educacao", "idhm_longevidade", "idhm_renda", "codigo")], by = "cod6")
indicadores$codigo <- as.character(indicadores$codigo)

#Juntando Base Resumida com Indicadores
base_final <- left_join(BANCORESUMIDO, indicadores[, c("cod6", "idhm", "idhm_educacao", "idhm_longevidade", "idhm_renda"
                                                       , "porcentagem_da_populacao_baixa_renda", "cobertura_bcg", "mortalidade"
                                                       , "renda_domiciliar_per_capita", "taxa_de_analfabetismo")], by = c("CODMUNRES" = "cod6"))

#Removendo os 21 casos com código municipal inválido (código 410000)
base_final <- base_final %>%
  filter(cod %in% regioes$cod)

base_final <- base_final %>%
  rename(
    'Síndrome de Down' = 'Sindrome de Dow',
    'Defeito do Tubo Neural' = 'Defeito do tubo Neural',
    'Cardiopatias Congênitas' = 'Cardiopatias congenitas',
    'Órgãos Genitais' = 'Órgãos genitais',
    'Defeitos de Membros' = 'Defeitos de membros',
    'Defeitos de Parede Abdominal' = 'Defeitos de parede abdominal'
  )

ac_agrupadas <- c("Defeito do Tubo Neural", "Microcefalia", "Cardiopatias Congênitas"
                  , "Fendas Orais", "Órgãos Genitais", "Defeitos de Membros", "Defeitos de Parede Abdominal", "Síndrome de Down")

#----------------------------------------Calculando prevalências por níveis regionais -------------------
#Prevalências
prevalencias_mun <- base_final %>%
  pivot_longer(cols = all_of(ac_agrupadas),
               names_to = "anomalia",
               values_to = "val") %>%
  mutate(val = as.integer(val)) %>%
  group_by(cod, ANO_NASC, anomalia) %>%
  summarise(
    nascidos = n(),
    casos = sum(val, na.rm = TRUE),
    prevalencia = ifelse(nascidos > 0, (casos / nascidos) * 10000, NA_real_),
    .groups = "drop"
  )

prevalencias_reg <- base_final %>%
  pivot_longer(cols = all_of(ac_agrupadas),
               names_to = "anomalia",
               values_to = "val") %>%
  mutate(val = as.integer(val)) %>%
  group_by(regional, ANO_NASC, anomalia) %>%
  summarise(
    nascidos = n(),
    casos = sum(val, na.rm = TRUE),
    prevalencia = ifelse(nascidos > 0, (casos / nascidos) * 10000, NA_real_),
    .groups = "drop"
  )

prevalencias_meso <- base_final %>%
  pivot_longer(cols = all_of(ac_agrupadas),
               names_to = "anomalia",
               values_to = "val") %>%
  mutate(val = as.integer(val)) %>%
  group_by(meso, ANO_NASC, anomalia) %>%
  summarise(
    nascidos = n(),
    casos = sum(val, na.rm = TRUE),
    prevalencia = ifelse(nascidos > 0, (casos / nascidos) * 10000, NA_real_),
    .groups = "drop"
  )

prevalencias_macro <- base_final %>%
  pivot_longer(cols = all_of(ac_agrupadas),
               names_to = "anomalia",
               values_to = "val") %>%
  mutate(val = as.integer(val)) %>%
  group_by(macro, ANO_NASC, anomalia) %>%
  summarise(
    nascidos = n(),
    casos = sum(val, na.rm = TRUE),
    prevalencia = ifelse(nascidos > 0, (casos / nascidos) * 10000, NA_real_),
    .groups = "drop"
  )

prevalencias_mun <- prevalencias_mun[!is.na(prevalencias_mun$cod),]
prevalencias_reg <- prevalencias_reg[!is.na(prevalencias_reg$regional),]
prevalencias_meso <- prevalencias_meso[!is.na(prevalencias_meso$meso),]
prevalencias_macro <- prevalencias_macro[!is.na(prevalencias_macro$macro),]

#----------------------------------------Exportando resultados -------------------
write.csv(prevalencias_mun, "data/prevalencias/municipal.csv", row.names = FALSE)
write.csv(prevalencias_reg, "data/prevalencias/regional.csv", row.names = FALSE)
write.csv(prevalencias_meso, "data/prevalencias/mesoregional.csv", row.names = FALSE)
write.csv(prevalencias_macro, "data/prevalencias/macroregional.csv", row.names = FALSE)
write.csv(base_final, "data/dados_finais.csv", row.names = FALSE)