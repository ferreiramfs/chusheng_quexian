#Análise Temporal
mod_temporal_server <- function(input, output, session) {

  #Título Mapa Temporal
  output$titulo_mapa <- renderUI({
    req(input$anomalia, input$ano_mapa, input$nivel_geo)
    
    nivel_texto <- switch(
      input$nivel_geo,
      "Municipal" = "Município",
      "Regional" = "Regional de Saúde",
      "Mesoregional" = "Mesoregião",
      "Macroregional" = "Macroregional de Saúde"
    )
    
    titulo <- paste0(
      "Prevalência de ", input$anomalia, 
      " por ", nivel_texto, 
      " em ", input$ano_mapa
    )
    
    h3(
      titulo,
      style = "text-align:center; margin-top:10px; font-weight:600;"
    )
  })
  
  #Mapa
  output$mapa_anomalia <- renderLeaflet({
    req(input$anomalia, input$ano_mapa, input$nivel_geo)
    
    #Metadados de cada nível
    niveis <- list(
      "Municipal" = list(
        df = prevalencia_municipal,
        shape = shape_municipal,
        join_key_x = "CC_2",
        join_key_y = "cod",
        col_prev = "prevalencia",
        label_col = "name_muni"
      ),
      "Regional" = list(
        df = prevalencia_regional,
        shape = shape_regional,
        join_key_x = "REGIONA",
        join_key_y = "regional",
        col_prev = "prevalencia",
        label_col = "REGIONA"
      ),
      "Mesoregional" = list(
        df = prevalencia_mesoregional,
        shape = shape_mesoregional,
        join_key_x = "meso",
        join_key_y = "meso",
        col_prev = "prevalencia",
        label_col = "meso"
      ),
      "Macroregional" = list(
        df = prevalencia_macroregional,
        shape = shape_macroregional,
        join_key_x = "MACRO",
        join_key_y = "macro",
        col_prev = "prevalencia",
        label_col = "MACRO"
      )
    )
    
    # 🔹 Recupera as configurações do nível selecionado
    nivel_info <- niveis[[input$nivel_geo]]
    
    # 🔹 Filtra os dados correspondentes
    dados <- nivel_info$df %>%
      filter(anomalia == input$anomalia, ANO_NASC == input$ano_mapa)
    
    # 🔹 Faz o join com o shape
    dados_mapa <- nivel_info$shape %>%
      mutate(across(nivel_info$join_key_x, as.character)) %>%
      left_join(
        dados %>% mutate(across(nivel_info$join_key_y, as.character)),
        by = setNames(nivel_info$join_key_y, nivel_info$join_key_x)
      )
    
    # 🔹 Extrai os campos corretos
    var_prev <- dados_mapa[[nivel_info$col_prev]]
    label_nome <- dados_mapa[[nivel_info$label_col]]
    
    #Define domínio fixo por tipo geográfico e anomalia
    base_dados <- get(paste0("prevalencia_", tolower(input$nivel_geo)))
    dom_fixo <- range(base_dados$prevalencia[base_dados$anomalia == input$anomalia], 
                      na.rm = TRUE)
    
    # Paleta com domínio fixo da anomalia selecionada
    pal <- colorNumeric(
      palette = "Greens",
      domain = dom_fixo,
      na.color = "gray90"
    )
    
    # Renderiza o mapa
    leaflet(dados_mapa) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(var_prev),
        color = "#BBB",         # cor da borda
        weight = 1,              # espessura da linha
        opacity = 1,             # opacidade da linha
        fillOpacity = 0.8,       # transparência do preenchimento
        smoothFactor = 0.2,      # suaviza os contornos
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~paste0(label_nome, ": ", round(var_prev, 1), " /10k")
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = dom_fixo,
        title = paste("Prevalência de", input$anomalia, "-", input$ano_mapa),
        labFormat = labelFormat(suffix = " /10k")
      )
  })
}
