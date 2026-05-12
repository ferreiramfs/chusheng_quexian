idhm <- read.csv2("data/indicadores/idhm.csv")
grau_urbanizacao <- read.csv("data/indicadores/grau_urbanizacao.csv")
taxa_analfabetismo <- read.csv2("data/indicadores/taxa_analfabetismo.csv")
renda_dompercap <- read.csv2("data/indicadores/renda_dompercap.csv")

#Ajustando códigos
taxa_analfabetismo$cod <- sub(" .*", "", taxa_analfabetismo$municipio)
renda_dompercap$cod <- sub(" .*", "", renda_dompercap$municipio)
idhm$cod <- substr(as.character(idhm$codigo), 1, 6)
grau_urbanizacao$cod <- as.character(grau_urbanizacao$cod)

#Merge das bases
indicadores <- left_join(idhm, grau_urbanizacao[, c("cod", "grau_urbanizacao")], by = "cod")
indicadores <- left_join(indicadores, renda_dompercap[, c("cod", "renda_domiciliar_per_capita")], by = "cod")
indicadores <- left_join(indicadores, taxa_analfabetismo[, c("cod", "taxa_de_analfabetismo")], by = "cod")

write.csv2(indicadores, "data/indicadores/indicadores.csv", row.names = FALSE)
