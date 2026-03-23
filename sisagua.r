dados_vig <- read.csv('data/vigilancia_demais_parametros.csv', header=TRUE, sep=";", fileEncoding = "latin1")

dados_vig <- dados_vig[dados_vig$UF == "PR", ]
summary(dados_vig)

db <- as.data.frame(table(dados_vig$Município))
