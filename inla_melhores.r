#Comparação Melhores Modelos Todas Anomalias
cardiopatias_congenitas_icar_rw <- readRDS('data/resultados/cardiopatias_congenitas_icar_rw.RDS')
defeitos_membros_icar_rw <- readRDS('data/resultados/defeitos_membros_icar_rw.RDS')
defeitos_parede_abdominal_icar_rw <- readRDS('data/resultados/defeitos_parede_abdominal_icar_rw.RDS')
defeito_tudo_neural_icar_rw <- readRDS('data/resultados/defeito_tubo_neural_icar_rw.RDS')
fendas_orais_icar_rw <- readRDS('data/resultados/fendas_orais_icar_rw.RDS')
microcefalia_icar_rw <- readRDS('data/resultados/microcefalia_icar_rw.RDS')
orgaos_genitais_icar_rw <- readRDS('data/resultados/orgaos_genitais_icar_rw.RDS')
sindrome_down_icar_rw <- readRDS('data/resultados/sindrome_down_icar_rw.RDS')

##----------------------------------------Comparação dos efeitos OR - Florest Plot----------------------------------------##
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
or_card <- extrair_OR(cardiopatias_congenitas_icar_rw, "Cardiopatias Congênitas")
or_memb <- extrair_OR(defeitos_membros_icar_rw, "Defeitos de Membros")
or_abdo <- extrair_OR(defeitos_parede_abdominal_icar_rw, "Defeitos de Parede Abdominal")
or_tubo <- extrair_OR(defeito_tudo_neural_icar_rw, "Defeitos do Tubo Neural")
or_fendas <- extrair_OR(fendas_orais_icar_rw, "Fendas Orais")
or_micro <- extrair_OR(microcefalia_icar_rw, "Microcefalia")
or_orgaos <- extrair_OR(orgaos_genitais_icar_rw, "Órgãos Genitais")
or_down <- extrair_OR(sindrome_down_icar_rw, "Síndrome de Down")

# Juntar todos
ors_todos <- rbind(or_card, or_memb, or_abdo, or_tubo, or_fendas, or_micro, or_orgaos, or_down)

# Ordenar modelos
#ors_todos$Modelo <- factor(ors_todos$Modelo, 
#                           levels = c("IID", "ICAR", "BYM", "ICAR + RW1", "BYM + RW1"))

ggplot(ors_todos, aes(x = OR, y = variavel, color = Modelo)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  geom_errorbarh(aes(xmin = IC_inf, xmax = IC_sup), 
                 position = position_dodge(width = 0.6), height = 0.15) +
  scale_x_log10() +
  scale_color_manual(values = c("Cardiopatias Congênitas" = "#E41A1C", 
                                "Defeitos de Membros" = "#377EB8", 
                                "Defeitos de Parede Abdominal" = "#4DAF4A",
                                "Defeitos do Tubo Neural" = "#984EA3",
                                "Fendas Orais" = "#FF7F00",
                                "Microcefalia" = "#000000",
                                "Órgãos Genitais" = "#F0E442",
                                "Síndrome de Down" = "#0072B2")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Odds Ratio (escala log)", 
       y = "",
       title = "Comparação dos Efeitos Fixos entre Modelos",
       subtitle = "IC 95%")


##----------------------------------------Análise Espacial----------------------------------------##
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
rr_card <- extrair_efeitos(cardiopatias_congenitas_icar_rw, "Cardiopatias Congênitas")
rr_memb <- extrair_efeitos(defeitos_membros_icar_rw, "Defeitos de Membros")
rr_abdo <- extrair_efeitos(defeitos_parede_abdominal_icar_rw, "Defeitos de Parede Abdominal")
rr_tubo <- extrair_efeitos(defeito_tudo_neural_icar_rw, "Defeitos do Tubo Neural")
rr_fendas <- extrair_efeitos(fendas_orais_icar_rw, "Fendas Orais")
rr_micro <- extrair_efeitos(microcefalia_icar_rw, "Microcefalia")
rr_orgaos <- extrair_efeitos(orgaos_genitais_icar_rw, "Órgãos Genitais")
rr_down <- extrair_efeitos(sindrome_down_icar_rw, "Síndrome de Down")

# Juntar todos
rr_todos <- rbind(rr_card, rr_memb, rr_abdo, rr_tubo, rr_fendas, rr_micro, rr_orgaos, rr_down)

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
