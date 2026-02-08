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

codificar_escmae <- function(x) {
  factor(x,
         levels = c(1, 2, 3, 4, 5, 9),
         labels = c("Nenhuma", "1 a 3anos", "4 a 7anos", "8 a 11anos", "12 e mais", "Ignorado")
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
  factor(x,
         levels = c(1, 2, 3, 9),
         labels = c("Única", "Dupla", "Tripla ou mais", "Ignorado")
  )
}

codificar_semanas_gestacao <- function(x){
  cut(x,
      breaks = c(0, 28, 32, 37, 42, 100),
      labels = c("Extremamente pré-termo (<28 semanas)",
                 "Muito pré-termo (28-31 semanas)",
                 "Pré-termo moderado/tardio (32-36 semanas)", 
                 "Termo (37-41 semanas)",
                 "Pós-termo (≥42 semanas)"),
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

codificar_codocumae <- function(x) {
  
  cod <- as.numeric(substr(x, 1, 3))
  
  categorias <- case_when(
    is.na(x) ~ NA_character_,
    between(cod, 0, 99) ~ "0 - Militares",
    between(cod, 101, 199) ~ "1 - Dirigentes/Gerentes",
    between(cod, 201, 299) ~ "2 - Profissionais Ciências/Artes",
    between(cod, 301, 399) ~ "3 - Técnicos Nível Médio",
    between(cod, 401, 499) ~ "4 - Trabalhadores Serviços",
    between(cod, 501, 599) ~ "5 - Trabalhadores Comércio",
    between(cod, 601, 699) ~ "6 - Trabalhadores Agropecuária",
    between(cod, 701, 899) ~ "7/8 - Trabalhadores Indústria",
    between(cod, 901, 999) ~ "9 - Trabalhadores Reparos/Manutenção",
    TRUE ~ "10 - Outros"
  )
  
  niveis_ordenados <- c(
    "0 - Militares",
    "1 - Dirigentes/Gerentes", 
    "2 - Profissionais Ciências/Artes",
    "3 - Técnicos Nível Médio",
    "4 - Trabalhadores Serviços",
    "5 - Trabalhadores Comércio",
    "6 - Trabalhadores Agropecuária",
    "7/8 - Trabalhadores Indústria",
    "9 - Trabalhadores Reparos/Manutenção",
    "10 - Outros"
  )
  
  factor(categorias, levels = niveis_ordenados)
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
  
  data$CODOCUPMAE <- codificar_codocumae(data$CODOCUPMAE)
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