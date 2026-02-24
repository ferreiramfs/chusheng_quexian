library(dplyr)
library(tidyr)
library(fst)

# Lê a base completa (só uma vez!)
data_completa <- read_fst("data/dados.fst")

anomalias <- c(
  "Defeito do Tubo Neural", "Microcefalia", "Cardiopatias Congênitas",
  "Fendas Orais", "Órgãos Genitais", "Defeitos de Membros",
  "Defeitos de Parede Abdominal", "Síndrome de Down", "Todas"
)

# Variáveis Contínuas
var_num <- c(
  'idhm', 'idhm_educacao', 'idhm_longevidade', 'idhm_renda',
  'porcentagem_da_populacao_baixa_renda', 'renda_domiciliar_per_capita',
  'mortalidade', 'taxa_de_analfabetismo', 'cobertura_bcg',
  'IDADEMAE', 'PESO', 'IDADEPAI'
)

# Variáveis Categóricas
var_cat <- c(
  'ESCMAE', 'SEXO', 'RACACORMAE', 'ESTCIVMAE', 'CONSULTAS',
  'APGAR1', 'APGAR5', 'LOCNASC', 'CODOCUPMAE', 'QTDGESTANT',
  'GRAVIDEZ', 'PARTO', 'SEMAGESTAC'
)

# ─────────────────────────────────────────────────────────────────────────────
# 1. UNIVARIADO NUMÉRICO — histograma com ponto médio e LARGURA real do bin
# ─────────────────────────────────────────────────────────────────────────────
univariado_num_list <- list()

for (v in var_num) {
  vals <- data_completa[[v]]
  vals <- vals[!is.na(vals)]
  
  h <- hist(vals, plot = FALSE, breaks = 30)
  
  df_hist <- data.frame(
    x        = head(h$breaks, -1) + diff(h$breaks) / 2,  # ponto médio
    y        = h$counts,
    width    = diff(h$breaks),                            # largura real do bin ← NOVO
    variavel = v,
    stringsAsFactors = FALSE
  )
  
  univariado_num_list[[v]] <- df_hist
}

univariado_num <- bind_rows(univariado_num_list)

# ─────────────────────────────────────────────────────────────────────────────
# 2. UNIVARIADO CATEGÓRICO — frequências e proporções
# ─────────────────────────────────────────────────────────────────────────────
univariado_cat <- data_completa %>%
  select(all_of(var_cat)) %>%
  pivot_longer(everything(), names_to = "variavel", values_to = "categoria") %>%
  filter(!is.na(categoria), categoria != "") %>%
  count(variavel, categoria) %>%
  group_by(variavel) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# ─────────────────────────────────────────────────────────────────────────────
# 3. BIVARIADO NUMÉRICO — estatísticas do boxplot por grupo
# ─────────────────────────────────────────────────────────────────────────────
bivariado_num_stats_list <- list()

for (anom in anomalias) {
  for (v in var_num) {
    base <- data_completa %>%
      select(x = all_of(v), grupo = all_of(anom)) %>%
      filter(!is.na(x), !is.na(grupo))
    
    grupos <- unique(base$grupo)
    
    for (g in grupos) {
      vals <- base$x[base$grupo == g]
      if (length(vals) < 10) next
      
      bivariado_num_stats_list[[paste(anom, v, g, sep = "_")]] <- data.frame(
        variavel       = v,
        anomalia       = anom,
        anomalia_valor = g,
        n              = length(vals),
        mediana        = median(vals),
        q1             = quantile(vals, 0.25),
        q3             = quantile(vals, 0.75),
        media          = mean(vals),
        min            = min(vals),
        max            = max(vals),
        # Bigodes Tukey: até 1.5×IQR além do Q1/Q3, limitado aos dados reais
        bigode_inf     = min(vals[vals >= quantile(vals, 0.25) - 1.5 * diff(quantile(vals, c(0.25, 0.75)))]),
        bigode_sup     = max(vals[vals <= quantile(vals, 0.75) + 1.5 * diff(quantile(vals, c(0.25, 0.75)))])
      )
    }
  }
}

bivariado_num_stats <- bind_rows(bivariado_num_stats_list)

# ─────────────────────────────────────────────────────────────────────────────
# 4. BIVARIADO CATEGÓRICO — proporções por anomalia
#
#    Resultado final por linha:
#      variavel | anomalia | anomalia_valor (Sim/Não) | categoria | n | prop | total
# ─────────────────────────────────────────────────────────────────────────────
bivariado_cat_list <- list()

for (anom in anomalias) {
  for (v in var_cat) {
    
    base <- data_completa %>%
      select(categoria = all_of(v), grupo = all_of(anom)) %>%
      filter(!is.na(categoria), !is.na(grupo), categoria != "")
    
    # Denominador real: total de NV por categoria (TRUE + FALSE)
    totais <- base %>%
      count(categoria, name = "total")
    
    # Numerador: apenas casos TRUE (com anomalia)
    casos_true <- base %>%
      filter(grupo == TRUE) %>%
      count(categoria, name = "n")
    
    # left_join garante que categorias sem nenhum TRUE apareçam com n = 0
    df_temp <- totais %>%
      left_join(casos_true, by = "categoria") %>%
      mutate(
        n              = coalesce(n, 0L),
        prop           = n / total,
        prev_10k       = (n / total) * 10000,
        variavel       = v,
        anomalia       = anom,
        anomalia_valor = TRUE
      ) %>%
      select(variavel, anomalia, anomalia_valor, categoria, n, prop, prev_10k, total)
    
    bivariado_cat_list[[paste(anom, v, sep = "_")]] <- df_temp
  }
}

bivariado_cat <- bind_rows(bivariado_cat_list)

# ─────────────────────────────────────────────────────────────────────────────
# 5. Salva os dados pré-processados
# ─────────────────────────────────────────────────────────────────────────────
saveRDS(univariado_num,      "data/univariado_num.rds")
saveRDS(univariado_cat,      "data/univariado_cat.rds")
saveRDS(bivariado_num_stats, "data/bivariado_num_stats.rds")
saveRDS(bivariado_cat,       "data/bivariado_cat.rds")