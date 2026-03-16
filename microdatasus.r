library(microdatasus)

sinasc <- fetch_datasus(year_start = 2013, year_end = 2014, uf = "PR", information_system = "SINASC")

sim <- fetch_datasus(year_start = 2013, year_end = 2014, uf = "PR", information_system = "SIM-DO")

sim <- fetch_datasus(year_start = 2013, year_end = 2014, uf = "PR", information_system = "SIM-DO")

sih <- fetch_datasus(year_start = 2013, month_start = 1, year_end = 2014, month_end = 12 , uf = "PR", information_system = "SIH-RD")

sih$NASC <- gsub("([0-9]{4})([0-9]{2})([0-9]{2})", "\\1-\\2-\\3", sih$NASC)
sih$NASC <- as.Date(sih$NASC)

# Filtra mantendo apenas as datas maiores ou iguais
sih2013 <- sih[sih$NASC >= as.Date("2013-01-01"), ]

sum(is.na(sih$CEP))
sum(table(sinasc$DTNASCMAE))

dados <- process_sim(dados)