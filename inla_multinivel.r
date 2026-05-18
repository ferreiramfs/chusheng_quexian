library(arrow)
library(dplyr)

library(SpatialEpi)
library(spdep)
#install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)
library(sf)
library(ggplot2)

data <- read_parquet("data/model_data.parquet")

data$ANO_NASC <- as.numeric(data$ANO_NASC)

data <- data %>%
  rename(
    defeito_tubo_neural = `Defeito do Tubo Neural`,
    microcefalia = Microcefalia,
    cardiopatias_congenitas = `Cardiopatias Congênitas`,
    fendas_orais = `Fendas Orais`,
    orgaos_genitais = `Órgãos Genitais`,
    defeitos_membros = `Defeitos de Membros`,
    defeitos_parede_abdominal = `Defeitos de Parede Abdominal`,
    sindrome_down = `Síndrome de Down`
  )

#Removendo os 2 casos com ignorado para QTDFILMORT
data <- data %>% 
  filter(QTDFILMORT != "Ignorado")

# Grafo para INLA:
g <- inla.read.graph(filename = "data/mapa_pr.adj")

#Ajustando a variável resposta de booleano para numérica
data$cardiopatias_congenitas <- as.numeric(data$cardiopatias_congenitas)

data$mun_id <- as.numeric(as.factor(data$CODMUNRES))

data$idhm_c <- scale(data$idhm)
data$urbanizacao_c <- scale(data$grau_urbanizacao)
data$renda_dom_c <- scale(data$renda_domiciliar_per_capita)
data$analfabetismo_c <- scale(data$taxa_de_analfabetismo)

#Mantendo somente variáveis uteis na base
data2 <- subset(data, select = c("defeito_tubo_neural", "microcefalia", "cardiopatias_congenitas", 
                                 "fendas_orais", "orgaos_genitais", "defeitos_membros", "defeitos_parede_abdominal",
                                 "sindrome_down","IDADEMAE", "ESCMAE", "CONSULTAS", 
                                 "ANO_NASC", "CODMUNRES", "mun_id", "idhm_c", 
                                 "urbanizacao_c", "renda_dom_c", "analfabetismo_c"))

#Amostrando
set.seed(123)
casos <- data2[data2$cardiopatias_congenitas == 1, ]
controles <- data2[data2$cardiopatias_congenitas == 0, ]
n_controles_amostra <- min(nrow(casos) * 20, nrow(controles))
controles_amostra <- controles[sample(1:nrow(controles), n_controles_amostra), ]
data_amostra <- rbind(casos, controles_amostra)

# Variáveis individuais por anomalia
vars_individuais <- list(
  defeito_tubo_neural      = c("IDADEMAE", "ESCMAE", "CONSULTAS"),
  microcefalia             = c("IDADEMAE", "ESCMAE", "CONSULTAS"),
  cardiopatias_congenitas  = c("IDADEMAE", "ESCMAE", "CONSULTAS"),
  fendas_orais             = c("IDADEMAE", "ESCMAE", "CONSULTAS"),
  orgaos_genitais          = c("IDADEMAE", "ESCMAE", "CONSULTAS"),
  defeitos_membros         = c("IDADEMAE", "ESCMAE", "CONSULTAS"),
  defeitos_parede_abdominal= c("IDADEMAE", "ESCMAE", "CONSULTAS"),
  sindrome_down            = c("IDADEMAE", "ESCMAE", "CONSULTAS")
)

# Variáveis municipais — fixas para todas as anomalias
vars_municipais <- c("idhm_c", "urbanizacao_c", "renda_dom_c", "analfabetismo_c")

build_formula <- function(anomalia, vars_ind, modelo_tipo, g) {
  
  resp <- anomalia
  
  # Preditores fixos: individuais + municipais
  fixos_ind  <- paste(vars_ind, collapse = " + ")
  fixos_mun  <- paste(vars_municipais, collapse = " + ")
  
  # Parte temporal: factor() nos modelos sem RW, f(rw1) nos modelos com RW
  temporal_fator <- "factor(ANO_NASC)"
  temporal_rw    <- "f(ANO_NASC, model = \"rw1\")"
  
  # Hiperparâmetros
  hyper_iid   <- "hyper = list(prec = list(prior = \"loggamma\", param = c(1, 0.01)))"
  hyper_besag <- "hyper = list(prec = list(prior = \"loggamma\", param = c(1, 0.01)))"
  hyper_bym   <- "hyper = list(
                    prec.unstruct = list(prior = \"loggamma\", param = c(1, 0.01)),
                    prec.spatial  = list(prior = \"loggamma\", param = c(1, 0.01)))"
  
  efeito_espacial <- switch(modelo_tipo,
                            "iid"     = sprintf("f(mun_id, model = \"iid\", %s)", hyper_iid),
                            "icar"    = sprintf("f(mun_id, model = \"besag\", graph = g, %s)", hyper_besag),
                            "bym"     = sprintf("f(mun_id, model = \"bym\",  graph = g, %s)", hyper_bym),
                            "icar_rw" = sprintf("f(mun_id, model = \"besag\", graph = g, %s)", hyper_besag),
                            "bym_rw"  = sprintf("f(mun_id, model = \"bym\",  graph = g, %s)", hyper_bym)
  )
  
  temporal <- if (modelo_tipo %in% c("icar_rw", "bym_rw")) temporal_rw else temporal_fator
  
  formula_str <- sprintf(
    "%s ~ %s + %s + %s + %s",
    resp, fixos_ind, fixos_mun, temporal, efeito_espacial
  )
  
  as.formula(formula_str)
}

rodar_todos_modelos <- function(data, g, vars_individuais, output_dir = "resultados/modelos") {
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  anomalias   <- names(vars_individuais)
  tipos_modelo <- c("iid", "icar", "bym", "icar_rw", "bym_rw")
  
  # Log de progresso
  total     <- length(anomalias) * length(tipos_modelo)
  contador  <- 0
  erros     <- list()
  
  for (anomalia in anomalias) {
    
    vars_ind <- vars_individuais[[anomalia]]
    
    # Preparar variável resposta para esta anomalia
    data$y_resp <- as.numeric(data[[anomalia]])
    
    for (tipo in tipos_modelo) {
      
      contador <- contador + 1
      nome_arquivo <- file.path(output_dir, sprintf("%s_%s.RDS", anomalia, tipo))
      
      # Pular se já foi rodado (útil para retomar após interrupção)
      if (file.exists(nome_arquivo)) {
        message(sprintf("[%d/%d] Já existe, pulando: %s | %s", contador, total, anomalia, tipo))
        next
      }
      
      message(sprintf("[%d/%d] Rodando: %s | %s  (%s)", 
                      contador, total, anomalia, tipo, Sys.time()))
      
      tryCatch({
        
        formula <- build_formula(anomalia = "y_resp",
                                 vars_ind  = vars_ind,
                                 modelo_tipo = tipo,
                                 g = g)
        
        modelo <- inla(
          formula,
          family = "binomial",
          data   = data,
          control.predictor = list(compute = FALSE),
          control.compute   = list(
            dic  = TRUE,
            waic = TRUE,
            return.marginals.predictor = FALSE
          ),
          control.fixed = list(mean = 0, prec = 0.001)
        )
        
        # Salvar e liberar memória imediatamente
        saveRDS(modelo, file = nome_arquivo)
        message(sprintf("    ✔ Salvo em: %s", nome_arquivo))
        
        rm(modelo)
        gc()
        
      }, error = function(e) {
        msg <- sprintf("    ✘ ERRO em %s | %s: %s", anomalia, tipo, e$message)
        message(msg)
        erros[[length(erros) + 1]] <<- list(anomalia = anomalia, tipo = tipo, erro = e$message)
      })
    }
  }
  
  # Relatório final
  message("\n── Concluído ──────────────────────────────")
  message(sprintf("Total rodado : %d / %d", contador - length(erros), total))
  
  if (length(erros) > 0) {
    message("Modelos com erro:")
    for (e in erros) {
      message(sprintf("  - %s | %s : %s", e$anomalia, e$tipo, e$erro))
    }
  }
  
  invisible(erros)  # retorna lista de erros para inspeção
}

# ─────────────────────────────────────────────
# Chamada
# ─────────────────────────────────────────────
erros <- rodar_todos_modelos(
  data            = data_amostra,        # sua base já tratada
  g               = g,            # grafo INLA
  vars_individuais = vars_individuais,
  output_dir      = "data/resultados"
)