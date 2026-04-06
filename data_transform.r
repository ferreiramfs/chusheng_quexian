codificar_qtde_gestant <- function(x) {
  out <- dplyr::case_when(
    x == 99 ~ "Ignorado",
    x == 0  ~ "0",
    x == 1  ~ "1",
    x == 2  ~ "2",
    x == 3  ~ "3",
    x >= 4 & x < 99 ~ "4+",
    TRUE ~ NA_character_
  )
  factor(out, levels = c("0", "1", "2", "3", "4+", "Ignorado"))
}

codificar_qtde_filmort <- function(x) {
  out <- dplyr::case_when(
    x == 99 ~ "Ignorado",
    x == 0  ~ "0",
    x == 1  ~ "1",
    x >= 2 & x < 99 ~ "2+",
    TRUE ~ NA_character_
  )
  factor(out, levels = c("0", "1", "2+", "Ignorado"))
}

codificar_locnasc <- function(x) {
  out <- dplyr::case_when(
    x == 9 ~ "Ignorado",
    x == 1  ~ "Hospital",
    x == 2 | x == 3 | x == 4 | x == 5  ~ "Outros",
    TRUE ~ NA_character_
  )
  factor(out, levels = c("Hospital", "Outros", "Ignorado"))
}

codificar_estciv <- function(x) {
  out <- dplyr::case_when(
    x == 9 ~ "Ignorada",
    x == 1  ~ "Solteira",
    x == 2  ~ "Casada",
    x == 5  ~ "União Estável",
    x == 3 | x == 4  ~ "Outros",
    TRUE ~ NA_character_
  )
  factor(out, levels = c("Solteira", "Casada", "União Estável", 'Outros', 'Ignorada'))
}

codificar_escmae <- function(x) {
  factor(x,
         levels = c(1, 2, 3, 4, 5, 9),
         labels = c("Nenhuma", "1 a 3anos", "4 a 7anos", "8 a 11anos", "12 e mais", "Ignorado")
  )
}

codificar_racacor <- function(x) {
  out <- dplyr::case_when(
    x == 1 ~ "Branca",
    x == 2  ~ "Preta",
    x == 4  ~ "Parda",
    x == 3 | x == 5  ~ "Outros",
    TRUE ~ NA_character_
  )
  factor(out, levels = c("Branca", "Preta", "Parda", 'Outros'))
}

codificar_idanomal <- function(x) {
  factor(x,
    levels = c(1, 2, 9),
    labels = c("Sim", "Não", "Ignorada")
  )
}

codificar_sexo <- function(x) {
  factor(x,
         levels = c(1, 2, 9),
         labels = c("M", "F", "Ignorado")
  )
}

codificar_parto <- function(x){
  factor(x,
         levels = c(1, 2, 9),
         labels = c("Vaginal", "Cesário", "Ignorado")
  )
}

codificar_gravidez <- function(x){
  out <- dplyr::case_when(
    x == 9 ~ "Ignorado",
    x == 1  ~ "Única",
    x == 2 | x == 3  ~ "Dupla ou mais",
    TRUE ~ NA_character_
  )
  factor(out, levels = c("Única", "Dupla ou mais", "Ignorado"))
}

codificar_semanas_gestacao <- function(x){
  cut(x,
      breaks = c(0, 37, 42, 100),
      labels = c("Pré-termo (<37 semanas)", 
                 "Termo (37-41 semanas)",
                 "Pós-termo (>41 semanas)"),
      right = FALSE,
      include.lowest = TRUE)
}

codificar_consultas <- function(x) {
  factor(
    x,
    levels = c(1, 2, 3, 4, 9),
    labels = c("Nenhuma", "De 1 a 3", "De 4 a 6", "7 e mais", "Ignorado")
  )
}

codificar_apgar <- function(x) {
  factor(
    x,
    levels = c(c(seq(0, 10), c(99))),
    labels = c(as.character(seq(0, 10)), c("ignorado"))
  )
}


#Depara das colunas
transform_data <- function(data){
  
  data <- data %>%
    mutate(PANDEMIA = ANO_NASC %in% c(2020, 2021))
  
  data$ANO_NASC <- as.factor(format(data$DTNASC, "%Y"))
  data$MES_NASC <- as.factor(format(data$DTNASC, "%m"))
  
  data_inicio <- min(data$DTNASC)
  data$TEMPO <- as.numeric(data$DTNASC - data_inicio + 1)
  
  data$SEXO[data$SEXO == "M"] <- 1
  data$SEXO[data$SEXO == "F"] <- 2
  data$SEXO[data$SEXO == "I"] <- 9
  
  data$IDADEMAE[data$IDADEMAE == 99] <- NA
  
  #Transformando colunas para numéricas
  cols_num <- c("IDADEMAE", "QTDFILMORT", "QTDGESTANT", "GRAVIDEZ", "PARTO", "CONSULTAS", "APGAR1", "APGAR5", "PESO", "SEMAGESTAC", "GESTACAO")
  data[cols_num] <- lapply(data[cols_num], as.numeric)
  
  #Transformando colunas para datas
  cols_data <- c('DTNASC', 'DTCADASTRO')
  data[cols_data] <- lapply(data[cols_data], as.Date, format= "%d%m%Y")
  
  data$QTDGESTANT <- codificar_qtde_gestant(data$QTDGESTANT)
  data$QTDFILMORT <- codificar_qtde_filmort(data$QTDFILMORT)
  data$ESCMAE <- codificar_escmae(data$ESCMAE)
  data$LOCNASC <- codificar_locnasc(data$LOCNASC)
  data$ESTCIVMAE <- codificar_estciv(data$ESTCIVMAE)
  data$RACACORMAE <- codificar_racacor(data$RACACORMAE)
  data$IDANOMAL <- codificar_idanomal(data$IDANOMAL)
  data$SEXO <- codificar_sexo(data$SEXO)
  data$CONSULTAS <- codificar_consultas(data$CONSULTAS)
  data$PARTO <- codificar_parto(data$PARTO)
  data$GRAVIDEZ <- codificar_gravidez(data$GRAVIDEZ)
  data$SEMAGESTAC <- codificar_semanas_gestacao(data$SEMAGESTAC)
  data$APGAR1 <- codificar_apgar(data$APGAR1)
  data$APGAR5 <- codificar_apgar(data$APGAR5)
  
  return(data)
}