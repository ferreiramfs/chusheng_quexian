codificar_qtdes <- function(x) {
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

codificar_semanas_gestacao <- function(x){
  cut(
    x,
    breaks = c(0, 28, 32, 34, 37, 42, 100),
    labels = c("Extremamente Pré-termo", "Muito Pré-Termo", "Pré-termo Moderado", "Pré-termo Tardio", "Termo", "Pós-termo"),
    include.lowest = TRUE,
    right = TRUE
  )
}

codificar_consultas <- function(x) {
  factor(
    x,
    levels = c(1, 2, 3, 4, 5),
    labels = c("Nenhuma", "De 1 a 3", "De 4 a 6", "7 e mais", NA)
  )
}

codificar_escmae <- function(x) {
  factor(x,
         levels = c(1, 2, 3, 4, 5, 9),
         labels = c("Nenhuma", "1a3anos", "4a7anos", "8a11anos", "12anosemais", "ignorado")
  )
}

codificar_locnasc <- function(x) {
  factor(x,
         levels = c(1, 2, 3, 4, 5, 9),
         labels = c("Hospital", "Outros estabelecimentos de saúde"
                    , "Domicílio", "Outros", "Aldeia indígena", "Ignorado")
  )
}

codificar_estciv <- function(x) {
  factor(x,
         levels = c(1, 2, 3, 4, 5, 9),
         labels = c("solteira", "Casada", "Viúva", "Separada jud.", "União estável", "Ignorada")
  )
}

codificar_racacor <- function(x) {
  factor(x,
    levels = c(1, 2, 3, 4, 5),
    labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena")
  )
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
         labels = c("M", "F", "I")
  )
}

#Depara das colunas
transform_data <- function(data){
  
  data$SEXO[data$SEXO == "M"] <- 1
  data$SEXO[data$SEXO == "F"] <- 2
  data$SEXO[data$SEXO == "I"] <- 9
  
  data$IDADEMAE[data$IDADEMAE == 99] <- NA
  
  #Transformando colunas para numéricas
  cols_num <- c("IDADEMAE", "QTDFILVIVO", "QTDFILMORT", "QTDGESTANT", "QTDPARTCES", "QTDPARTNOR", "GRAVIDEZ", "PARTO", "CONSULTAS", "APGAR1", "APGAR5", "PESO", "SEMAGESTAC", "GESTACAO")
  data[cols_num] <- lapply(data[cols_num], as.numeric)
  
  #Transformando colunas para datas
  cols_data <- c('DTNASC', 'DTCADASTRO')
  data[cols_data] <- lapply(data[cols_data], as.Date, format= "%d%m%Y")
  
  #Colunas de quantidades
  cols_qtdes <- c("QTDFILVIVO", "QTDFILMORT", "QTDGESTANT", "QTDPARTCES", "QTDPARTNOR")
  
  #Codificando campos
  data[cols_qtdes] <- lapply(data[cols_qtdes], codificar_qtdes)
  data$SEMAGESTAC <- codificar_semanas_gestacao(data$SEMAGESTAC)
  data$ESCMAE <- codificar_escmae(data$ESCMAE)
  data$LOCNASC <- codificar_locnasc(data$LOCNASC)
  data$ESTCIVMAE <- codificar_estciv(data$ESTCIVMAE)
  data$RACACORMAE <- codificar_racacor(data$RACACORMAE)
  data$IDANOMAL <- codificar_idanomal(data$IDANOMAL)
  data$SEXO <- codificar_sexo(data$SEXO)
  data$CONSULTAS <- codificar_consultas(data$CONSULTAS)
  
  return(data)
}