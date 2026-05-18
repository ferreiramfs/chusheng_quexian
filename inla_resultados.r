library(ggplot2)
library(sf)
library(dplyr)

###############################################################################
###-------------------------Carregamento e Métricas-------------------------###
###############################################################################
#Selecionar anomalia
anomalia <- 'fendas_orais'
caminho <- 'data/resultados/'

modelo_iid <- readRDS(paste0(caminho, anomalia, '_iid.RDS'))
modelo_icar <- readRDS(paste0(caminho, anomalia, '_icar.RDS'))
modelo_bym <- readRDS(paste0(caminho, anomalia, '_bym.RDS'))
modelo_icar_rw <- readRDS(paste0(caminho, anomalia, '_icar_rw.RDS'))
modelo_bym_rw <- readRDS(paste0(caminho, anomalia, '_bym_rw.RDS'))

correspondencia <- readRDS("data/resultado_modelos/correspondencia.rds")

comparacao_modelos <- data.frame(
  Modelo = c("IID", "ICAR", "BYM", "ICAR + RW1", "BYM + RW1"),
  DIC = c(
    modelo_iid$dic$dic,
    modelo_icar$dic$dic,
    modelo_bym$dic$dic,
    modelo_icar_rw$dic$dic,
    modelo_bym_rw$dic$dic
  ),
  WAIC = c(
    modelo_iid$waic$waic,
    modelo_icar$waic$waic,
    modelo_bym$waic$waic,
    modelo_icar_rw$waic$waic,
    modelo_bym_rw$waic$waic
  )
)

# Ordenar por DIC (menor é melhor)
comparacao_modelos <- comparacao_modelos[order(comparacao_modelos$DIC), ]
print(comparacao_modelos)

###############################################################################
###---------------------Análise de Modelos Comparativa----------------------###
###############################################################################
# Função para extrair OR
extrair_OR <- function(modelo, nome_modelo) {
  fixed <- modelo$summary.fixed
  fixed$variavel <- rownames(fixed)
  fixed <- fixed[!grepl("Intercept|factor", fixed$variavel), ]
  data.frame(
    variavel = fixed$variavel,
    OR = exp(fixed$mean),
    IC_inf = exp(fixed$`0.025quant`),
    IC_sup = exp(fixed$`0.975quant`),
    Modelo = nome_modelo
  )
}

# Extrair para cada modelo
or_iid <- extrair_OR(modelo_iid, "IID")
or_icar <- extrair_OR(modelo_icar, "ICAR")
or_bym <- extrair_OR(modelo_bym, "BYM")
or_icar_rw <- extrair_OR(modelo_icar_rw, "ICAR + RW1")
or_bym_rw <- extrair_OR(modelo_bym_rw, "BYM + RW1")

# Juntar todos
ors_todos <- rbind(or_iid, or_icar, or_bym, or_icar_rw, or_bym_rw)

# Ordenar modelos
ors_todos$Modelo <- factor(ors_todos$Modelo, 
                           levels = c("IID", "ICAR", "BYM", "ICAR + RW1", "BYM + RW1"))

ggplot(ors_todos, aes(x = OR, y = variavel, color = Modelo)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  geom_errorbarh(aes(xmin = IC_inf, xmax = IC_sup), 
                 position = position_dodge(width = 0.6), height = 0.15) +
  scale_x_log10() +
  scale_color_manual(values = c("IID" = "#E41A1C", 
                                "ICAR" = "#377EB8", 
                                "BYM" = "#4DAF4A",
                                "ICAR + RW1" = "#984EA3",
                                "BYM + RW1" = "#FF7F00")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Odds Ratio (escala log)", 
       y = "",
       title = "Comparação dos Efeitos Fixos entre Modelos",
       subtitle = "IC 95%")
###############################################################################
###-----------------------Análise de Efeitos Espaciais----------------------###
###############################################################################
# Função para extrair efeitos espaciais
extrair_efeitos <- function(modelo, nome_modelo) {
  efeitos <- modelo$summary.random$mun_id
  # Remover linhas z1, z2... (coeficientes level 2)
  efeitos <- efeitos[!grepl("z", efeitos$ID), ]
  efeitos$mun_id <- as.numeric(efeitos$ID)
  efeitos$RR <- exp(efeitos$mean)
  efeitos$Modelo <- nome_modelo
  return(efeitos[, c("mun_id", "RR", "Modelo")])
}

# Extrair para cada modelo
rr_iid <- extrair_efeitos(modelo_iid, "IID")
rr_icar <- extrair_efeitos(modelo_icar, "ICAR")
rr_bym <- extrair_efeitos(modelo_bym, "BYM")
rr_icar_rw <- extrair_efeitos(modelo_icar_rw, "ICAR + RW1")
rr_bym_rw <- extrair_efeitos(modelo_bym_rw, "BYM + RW1")

# Juntar todos
rr_todos <- rbind(rr_iid, rr_icar, rr_bym, rr_icar_rw, rr_bym_rw)

# Carregar shapefile
pr_municipios <- readRDS('light_data/municipal.rds')
pr_municipios$cod_mun_6 <- as.character(gsub(".$", "", pr_municipios$CC_2))

# Merge com correspondência (já criada anteriormente)
pr_municipios <- merge(pr_municipios, correspondencia, 
                       by.x = "cod_mun_6", by.y = "CODMUNRES",
                       all.x = TRUE)

# Fazer merge com os RRs
pr_mapas <- merge(pr_municipios, rr_todos, by = "mun_id")

# Ordenar os níveis dos modelos
pr_mapas$Modelo <- factor(pr_mapas$Modelo, 
                          levels = c("IID", "ICAR", "BYM", "ICAR + RW1", "BYM + RW1"))

ggplot(data = pr_mapas) +
  geom_sf(aes(fill = RR), color = NA, size = 0.05) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 1, 
    limits = c(min(pr_mapas$RR), max(pr_mapas$RR)),
    name = "Risco Relativo"
  ) +
  facet_wrap(~ Modelo, ncol = 3) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  ) +
  labs(title = "Comparação do Risco Relativo Espacial entre Modelos",
       subtitle = "Modelos Hierárquicos Espaciais - INLA")

###############################################################################
###-----------------------Análise Completa ICAR + RW1-----------------------###
###############################################################################
# Sumário completo
summary(modelo_icar_rw)

# Verificar convergência
modelo_icar_rw$mode$mode.status

# Hiperparâmetros (precisões)
modelo_icar_rw$summary.hyperpar

#Covariáveis
# Efeitos fixos
efeitos_fixos <- modelo_icar_rw$summary.fixed

# Adicionar nomes
efeitos_fixos$variavel <- rownames(efeitos_fixos)

# Remover intercepto e anos
efeitos_individuais <- efeitos_fixos[!grepl("Intercept|factor", efeitos_fixos$variavel), ]

# Calcular OR
efeitos_individuais$OR <- exp(efeitos_individuais$mean)
efeitos_individuais$OR_inf <- exp(efeitos_individuais$`0.025quant`)
efeitos_individuais$OR_sup <- exp(efeitos_individuais$`0.975quant`)
efeitos_individuais$poder_associacao <- ifelse(
  efeitos_individuais$`0.025quant` > 0 | efeitos_individuais$`0.975quant` < 0,
  "Significativo", "Não significativo"
)

# Tabela
tabela_individuais <- efeitos_individuais[, c("variavel", "OR", "OR_inf", "OR_sup", "poder_associacao")]
print(tabela_individuais)

#Efeito Temporal
efeito_temporal <- modelo_icar_rw$summary.random$ANO_NASC

# Exponenciar para OR relativo ao ano base
efeito_temporal$OR <- exp(efeito_temporal$mean)
efeito_temporal$OR_inf <- exp(efeito_temporal$`0.025quant`)
efeito_temporal$OR_sup <- exp(efeito_temporal$`0.975quant`)

# Gráfico da tendência
ggplot(efeito_temporal, aes(x = ID, y = OR)) +
  geom_ribbon(aes(ymin = OR_inf, ymax = OR_sup), fill = "steelblue", alpha = 0.2) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  theme_minimal() +
  labs(x = "Ano de Nascimento", y = "Odds Ratio",
       title = paste0("Tendência Temporal do Risco de ", anomalia),
       subtitle = "Efeito temporal suavizado (RW1) - IC 95%")

#EFEITO ESPACIAL
# Extrair resíduos espaciais (excluindo z1, z2...)
efeitos_espaciais <- modelo_icar_rw$summary.random$mun_id[1:399, ]
efeitos_espaciais$mun_id <- as.numeric(efeitos_espaciais$ID)

# Calcular RR residual
efeitos_espaciais$RR <- exp(efeitos_espaciais$mean)
efeitos_espaciais$RR_inf <- exp(efeitos_espaciais$`0.025quant`)
efeitos_espaciais$RR_sup <- exp(efeitos_espaciais$`0.975quant`)

# Classificar significância
efeitos_espaciais$categoria <- with(efeitos_espaciais,
                                    ifelse(RR_inf > 1, "Risco Aumentado",
                                           ifelse(RR_sup < 1, "Risco Reduzido", "Não Significativo"))
)

# Quantos municípios em cada categoria
table(efeitos_espaciais$categoria)

#MAPA
# Merge com shapefile (usando correspondência já criada)
pr_mapas <- merge(pr_municipios, 
                  efeitos_espaciais[, c("mun_id", "RR", "RR_inf", "RR_sup", "categoria")],
                  by = "mun_id")

# Mapa 1: Gradiente contínuo
ggplot(pr_mapas) +
  geom_sf(aes(fill = RR), color = "gray70", size = 0.1) +
  scale_fill_gradient2(
    low = "#2166AC", mid = "#F7F7F7", high = "#B2182B",
    midpoint = 1, name = "RR Residual"
  ) +
  theme_void() +
  labs(title = paste0("Risco Relativo Residual de", anomalia),
       subtitle = "Modelo ICAR + RW1 - Efeito espacial após ajuste por covariáveis")

# Mapa 2: Significância
ggplot(pr_mapas) +
  geom_sf(aes(fill = categoria), color = "gray70", size = 0.1) +
  scale_fill_manual(
    values = c("Risco Aumentado" = "#B2182B",
               "Risco Reduzido" = "#2166AC",
               "Não Significativo" = "gray90"),
    name = "Efeito Espacial"
  ) +
  theme_void() +
  labs(title = "Significância do Efeito Espacial Residual",
       subtitle = "IC 95% - Modelo ICAR + RW1")
