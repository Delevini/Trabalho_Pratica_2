rmv.cha <- function(x) iconv(x, to = "ASCII//TRANSLIT")

library(lubridate)
library(ggplot2)
library(dplyr)
library(reshape2)
library(ggpubr)
library(GGally)
library(readxl)
library(tidyverse)
library(geobr)
library(patchwork)
library(tmap)
library(plotly)
library(DT)
library(sf)
library(writexl)
library(irr)
library(shinydashboard)
library(shiny)

#### Tratamento ####

dadosdon2023 <- read_xlsx("Donkelar_dados_completos_consolidado_2023.xlsx")  
dadosdon2022 <- read_xlsx("Donkelar_dados_completos_consolidado_2022.xlsx")  

dadosdon2022 = dadosdon2022 %>% filter(SIGLA_UF == "ES")
dadosdon2023 = dadosdon2023 %>% filter(AREA_KM2 == "ES")
dadosdon2023$CD_MUN = as.character(dadosdon2023$CD_MUN)

anual_cams = read_xlsx("cams_anual_ES.xlsx")
diario_cams = read_xlsx("cams_diario_ES.xlsx")


cams23 = left_join(diario_cams |> filter(ano == "2023") |> select(-c(1:2,Cod2,ano)) |> rename("CD_MUN" = Cod) ,
                   dadosdon2022 |> select(1:4),
                   by = "CD_MUN") |> unique()

cams22 = left_join(diario_cams |> filter(ano == "2022") |> select(-c(1:2,Cod2,ano)) |> rename("CD_MUN" = Cod) ,
                   dadosdon2022 |> select(1:4),
                   by = "CD_MUN") |> unique()


donk22  = dadosdon2022 |> select("pm2.5" = Media_PM25, CD_MUN, "month" = Mes) |> mutate(fonte = "Donkelar")
cams22 = cams22 |> group_by(month,CD_MUN) |> summarise(pm2.5 = mean(pm2.5)) |> mutate(fonte = "cams")

compara22 = rbind(donk22,cams22)

donk23  = dadosdon2023 |> select("pm2.5" = Media_PM25, CD_MUN, "month" = Mes) |> mutate(fonte = "Donkelar")
cams23 = cams23 |> group_by(month,CD_MUN) |> summarise(pm2.5 = mean(pm2.5)) |> mutate(fonte = "cams")

compara23 = rbind(donk23,cams23)

mapa <- read_municipality(code_muni = "ES", year = 2024)


ES23 = left_join(mapa,anual_cams |> filter(ano == "2023"), by = c("name_muni" = "NM_MUN")) |> mutate(Ano = "2023")
ES22 = left_join(mapa,anual_cams |> filter(ano == "2022"), by = c("name_muni" = "NM_MUN")) |> mutate(Ano = "2022")
EScams = rbind(ES22,ES23)

anualdonk22 = donk22 |> group_by(CD_MUN) |> mutate(pm_2.5 = mean(pm2.5)) |> select(2,5) |> unique() |> rename("pm2.5" = pm_2.5)
anualdonk23 = donk23 |> group_by(CD_MUN) |> mutate(pm_2.5 = mean(pm2.5)) |> select(2,5) |> unique() |> rename("pm2.5" = pm_2.5)

anualdonk22 = left_join(anualdonk22, anual_cams |> filter(ano == "2022"), by = "CD_MUN") |> select(1:3)
anualdonk23 = left_join(anualdonk23, anual_cams |> filter(ano == "2022"), by = "CD_MUN") |> select(1:3)

ES23donk = left_join(mapa,anualdonk23, by = c("name_muni" = "NM_MUN")) |> mutate(Ano = "2023")
ES22donk = left_join(mapa,anualdonk22, by = c("name_muni" = "NM_MUN")) |> mutate(Ano = "2022")

ESdonk = rbind(ES22donk,ES23donk) |> rename("pm2.5" = pm2.5.x)

diff = abs(EScams$pm2.5 - ESdonk$pm2.5)
ESdiff = ESdonk |> select(-pm2.5) |> mutate(pm2.5 = diff)


breaks <- c(0, 5, 10, 15, 20,25,30,35, Inf )
legendas <- c(
  "0-5: Ideal",
  "5-10: Bom",
  "10-15: Aceitável",
  "15-20: Moderado",
  "20-25: Ruim",
  "25-30: Muito Ruim",
  "30-35: Perigoso",
  "35+: Crítico"
)

EScams <- EScams %>%
  mutate(
    pm25_categoria = cut(
      pm2.5,
      breaks = breaks,
      labels = legendas,
      right = FALSE, 
      include.lowest = TRUE
    )
  )

ESdonk <- ESdonk %>%
  mutate(
    pm25_categoria = cut(
      pm2.5,
      breaks = breaks,
      labels = legendas,
      right = FALSE, # Inclui o valor da esquerda no intervalo [0, 5), [5, 10), etc.
      include.lowest = TRUE
    )
  )  

ESdiff <- ESdiff %>%
  mutate(
    pm25_categoria = cut(
      pm2.5,
      breaks = breaks,
      labels = legendas,
      right = FALSE, # Inclui o valor da esquerda no intervalo [0, 5), [5, 10), etc.
      include.lowest = TRUE
    )
  )  

cores_personalizadas <- c(
  "0-5: Ideal" = "#4CAF50",       
  "5-10: Bom" = "#8BC34A",       
  "10-15: Aceitável" = "#CDDC39", 
  "15-20: Moderado" = "#FFEB3B",  
  "20-25: Ruim" = "#FFC107",       
  "25-30: Muito Ruim" = "#FF9800", 
  "30-35: Perigoso" = "#F44336",   
  "35+: Crítico" = "#B71C1C"      
)

media_cams = EScams |> group_by(name_muni) |> summarise(pm2.5 = mean(pm2.5)) 
media_donk = ESdonk |> group_by(name_muni) |> summarise(pm2.5 = mean(pm2.5)) 
media_diff = ESdiff |> group_by(name_muni) |> summarise(pm2.5 = mean(pm2.5)) 
Descri = ESdonk |> select(Ano,"Municipio" = name_muni) |> mutate(pm2.5_donk = ESdonk$pm2.5,
                                                                 pm2.5_cams = EScams$pm2.5,
                                                                 pm2.5_diff = ESdiff$pm2.5)

tabela_resumo <- Descri %>%
  group_by(Ano) %>%
  summarise(
    media_donk = mean(pm2.5_donk, na.rm = TRUE),
    desvio_padrao_donk = sd(pm2.5_donk, na.rm = TRUE),
    max_donk = max(pm2.5_donk, na.rm = TRUE),
    municipio_max_donk = Municipio[which.max(pm2.5_donk)],
    min_donk = min(pm2.5_donk, na.rm = TRUE),
    municipio_min_donk = Municipio[which.min(pm2.5_donk)],
    media_cams = mean(pm2.5_cams, na.rm = TRUE),
    desvio_padrao_cams = sd(pm2.5_cams, na.rm = TRUE),
    max_cams = max(pm2.5_cams, na.rm = TRUE),
    municipio_max_cams = Municipio[which.max(pm2.5_cams)],
    min_cams = min(pm2.5_cams, na.rm = TRUE),
    municipio_min_cams = Municipio[which.min(pm2.5_cams)],
    media_diff = mean(pm2.5_diff, na.rm = TRUE),
    desvio_padrao_diff = sd(pm2.5_diff, na.rm = TRUE),
    max_diff = max(pm2.5_diff, na.rm = TRUE),
    municipio_max_diff = Municipio[which.max(pm2.5_diff)],
    min_diff = min(pm2.5_diff, na.rm = TRUE),
    municipio_min_diff = Municipio[which.min(pm2.5_diff)],
    .groups = 'drop' 
  )

tabela_resumo$geom <- NULL

tabela_resumo = t(tabela_resumo)
colnames(tabela_resumo) = tabela_resumo[1,]
tabela_resumo = tabela_resumo[-1,]

####

dispersao22 = compara22 |> pivot_wider(names_from = fonte,
                                       values_from = pm2.5)

dispersao23 = compara23 |> pivot_wider(names_from = fonte,
                                       values_from = pm2.5)



### dash ####
ui <- dashboardPage(
  skin = "black", 
  dashboardHeader(title = "Análise PM2.5 (ES)"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visão Temporal", tabName = "temporal", icon = icon("calendar")),
      menuItem("Relação", tabName = "Dispersao", icon = icon("braille")),
      menuItem("Validação Estatística", tabName = "Bland-Altman", icon = icon("chart-line")),
      menuItem("Análise Espacial", tabName = "dados", icon = icon("map-marked-alt")),
      menuItem("Resumo Descritivo", tabName = "resumo", icon = icon("clipboard-list")),
      menuItem("Testes Estatísticos", tabName = "testes_est", icon = icon("calculator"))
    ),
    hr()
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "temporal",
              fluidRow(
                box(
                  title = "Filtros", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 3, 
                  selectInput(
                    inputId = "select_ano",
                    label = "Selecione o Ano:",
                    choices = c("2022", "2023"),
                    selected = "2023"
                  ),
                  
                  hr(),
                  checkboxGroupInput(
                    inputId = "check_fontes", 
                    label = "Selecione as Fontes:", 
                    choices = c("Donkelar", "cams"), 
                    selected = c("Donkelar", "cams")
                  ),
                  sliderInput(
                    inputId = "slider_meses",
                    label = "Intervalo de Meses:",
                    min = 1, max = 12, 
                    value = c(1, 12),
                    step = 1, sep = ""
                  )
                ),
                box(
                  title = "Análise de Dispersão Mensal", 
                  status = "primary", 
                  width = 9, 
                  plotOutput("plot_boxplot", height = "80vh")
                )
              )
      ),
      tabItem(tabName = "Dispersao",
              fluidRow(
                box(
                  title = "Filtros da Dispersão",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 3, 
                  selectInput(
                    inputId = "select_ano_disp",
                    label = "Selecione o Ano:",
                    choices = c("2022", "2023"),
                    selected = "2023"
                  )
                ),
                box(
                  title = "Gráfico de Dispersão",
                  status = "primary",
                  width = 9, 
                  plotOutput("plot_dispersao", height = "80vh")
                )
              )
      ),
      tabItem(tabName = "Bland-Altman",
              fluidRow(
                box(
                  title = "Filtros Bland-Altman",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 3,
                  
                  selectInput(
                    inputId = "select_ano_bland", 
                    label = "Selecione o Ano:",
                    choices = c("2022", "2023"),
                    selected = "2023"
                  )
                ),
                box(
                  title = "Gráfico de Bland-Altman",
                  status = "primary",
                  width = 9,
                  plotOutput("plot_bland", height = "80vh")
                )
              )
      ),
      ####
      tabItem(tabName = "dados",
              fluidRow(
                box(
                  title = "Configuração do Mapa",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 3,
                  selectInput(
                    inputId = "mapa_modelo",
                    label = "Selecione o Modelo:",
                    choices = c("Donkelar", "CAMS", "Diferença"), 
                    selected = "Donkelar"
                  ),
                  hr(),
                  selectInput(
                    inputId = "mapa_ano",
                    label = "Selecione o Período:",
                    choices = c("2022", "2023", "Média (22-23)"), 
                    selected = "2023"
                  ),
                  p(class = "text-muted", style = "font-size: 0.9em;",
                    "Nota: A opção 'Média' calcula a média aritmética entre os dois anos para cada município.")
                ),
                box(
                  title = "Distribuição Espacial de PM2.5",
                  status = "primary",
                  width = 9,
                  plotOutput("plot_mapa_es", height = "85vh")
                )
              )
        ),
      tabItem(tabName = "resumo",
              fluidRow(
                box(
                  title = "Configuração do Resumo",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 3,
                  
                  selectInput(
                    inputId = "resumo_modelo",
                    label = "Selecione o Modelo:",
                    choices = c("Donkelar", "CAMS"),
                    selected = "Donkelar"
                  ),
                  
                  hr(),
                  
                  selectInput(
                    inputId = "resumo_ano",
                    label = "Selecione o Ano:",
                    choices = c("2022", "2023"),
                    selected = "2023"
                  )
                ),
                box(
                  title = "Estatísticas Descritivas",
                  status = "primary",
                  width = 9,
                  DTOutput("tabela_estatisticas",height = "85vh")
                )
              )
      ),
      tabItem(tabName = "testes_est",
              fluidRow(
                box(
                  title = "Parâmetros do Teste",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 3,
                  selectInput(
                    inputId = "teste_ano",
                    label = "Selecione o Ano:",
                    choices = c("2022", "2023"),
                    selected = "2023"
                  ),
                  hr(),
                  radioButtons(
                    inputId = "teste_tipo",
                    label = "Escolha o Teste:",
                    choices = c(
                      "Correlação de Pearson" = "pearson",
                      "Correlação de Spearman" = "spearman",
                      "Concordância (ICC)" = "icc"
                    )
                  ),
                  p(class = "text-muted", style = "font-size: 0.9em;",
                    "Nota: O ICC utiliza o modelo 'twoway' e unit 'single' (concordância absoluta).")
                ),
                box(
                  title = "Resultados Detalhados",
                  status = "primary",
                  width = 9,
                  verbatimTextOutput("saida_teste"),
                  uiOutput("interpretacao_teste")
                )
              )
      )
      )
    )
  )  

server <- function(input, output) {
  dados_selecionados <- reactive({
    tabela_base <- if (input$select_ano == "2022") {
      compara22 
    } else {
      compara23  
    }
    req(input$check_fontes)
    tabela_base %>%
      filter(
        fonte %in% input$check_fontes,
        month >= input$slider_meses[1] & month <= input$slider_meses[2]
      )
  })
  output$plot_boxplot <- renderPlot({
    df <- dados_selecionados()
    ggplot(data = df, 
           mapping = aes(y = pm2.5,
                         x = factor(month),
                         fill = fonte)) +
      geom_boxplot() +
      stat_summary(fun = mean, 
                   geom = "point", 
                   size = 2, 
                   color = "green",
                   position = position_dodge(width = 0.75)) +
      stat_summary(fun = median, 
                   geom = "point", 
                   size = 2, 
                   color = "red",
                   position = position_dodge(width = 0.75)) +
      
      theme_minimal() +
      labs(x = "Mês",
           y = "PM2.5",
           title = paste("Ano de", input$select_ano), 
           caption = "ponto verde: Média | Ponto vermelho: Mediana",
           fill = "Fonte")
  })
  output$plot_dispersao <- renderPlot({
    df_base <- if (input$select_ano_disp == "2022") {
      compara22
    } else {
      compara23
    }
    dispersao_data <- df_base %>% 
      pivot_wider(names_from = fonte, values_from = pm2.5)
    ggplot(dispersao_data, aes(x = Donkelar, y = cams)) +
      geom_point(size = 3, alpha = 0.7, color = "blue") +
      geom_smooth(method = "lm", se = FALSE, color = "darkred") +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
      labs(
        title = "Comparação de Métodos de Medição de PM2.5",
        subtitle = paste("Ano de", input$select_ano_disp), 
        x = "PM2.5 Donkelar",
        y = "PM2.5 Cams"
      ) +
      theme_bw()
  })
  output$plot_bland <- renderPlot({
    df_base <- if (input$select_ano_bland == "2022") {
      compara22
    } else {
      compara23
    }
    bland_data <- df_base %>%
      pivot_wider(names_from = fonte, values_from = pm2.5) %>%
      mutate(
        media = (Donkelar + cams) / 2,
        diferenca = Donkelar - cams
      )
    media_diff <- mean(bland_data$diferenca, na.rm = TRUE)
    sd_diff <- sd(bland_data$diferenca, na.rm = TRUE)
    limite_superior <- media_diff + (1.96 * sd_diff)
    limite_inferior <- media_diff - (1.96 * sd_diff)
    ggplot(bland_data, aes(x = media, y = diferenca)) +
      geom_point(alpha = 0.6, size = 3) +
      geom_hline(yintercept = media_diff, color = "blue", linetype = "dashed", linewidth = 1) +
      geom_hline(yintercept = limite_superior, color = "red", linetype = "dotted", linewidth = 1) +
      geom_hline(yintercept = limite_inferior, color = "red", linetype = "dotted", linewidth = 1) +
      
      labs(
        title = paste("Gráfico de Bland-Altman - Ano", input$select_ano_bland),
        subtitle = "Comparação Donkelar vs. Cams",
        x = "Média das Medições ((Donkelar + cams) / 2)",
        y = "Diferença das Medições (Donkelar - cams)"
      ) +
      theme_bw()
  }) 
  
  
  dados_mapa_final <- reactive({
    tabela_base <- switch(input$mapa_modelo,
                          "Donkelar" = ESdonk,  
                          "CAMS" = EScams,     
                          "Diferença" = ESdiff) 
    if (input$mapa_ano == "Média (22-23)") {
      tabela_base %>%
        group_by(name_muni, geom) %>% 
        summarise(pm2.5 = mean(pm2.5, na.rm = TRUE), .groups = "drop") %>%
        mutate(
          pm25_categoria = cut(
            pm2.5,
            breaks = c(0, 5, 10, 15, 20, 25, 30, 35, Inf), 
            labels = c("0-5: Ideal", "5-10: Bom", "10-15: Aceitável", 
                       "15-20: Moderado", "20-25: Ruim", "25-30: Muito Ruim", 
                       "30-35: Perigoso", "35+: Crítico"),
            right = FALSE, 
            include.lowest = TRUE
          )
        )
    } else {
      tabela_base %>% 
        filter(Ano == input$mapa_ano)
    }
  })
  output$plot_mapa_es <- renderPlot({
    dados <- dados_mapa_final()
    ggplot(data = dados) +
      geom_sf(mapping = aes(fill = pm25_categoria), color = "black", size = 0.1) +
      geom_sf_text(
        aes(label = name_muni), 
        size = 4,          
        color = "black",     
        check_overlap = TRUE 
      ) +
      scale_fill_manual(
        name = "Níveis de PM 2.5",
        values = cores_personalizadas,
        drop = FALSE 
      ) +
      theme_void() + 
      labs(
        title = paste("Modelo:", input$mapa_modelo),
        subtitle = paste("Período:", input$mapa_ano)
      ) +
      theme(
        legend.title = element_text(size = 16, colour = "Red", face = "bold"),
        legend.text = element_text(size = 12, colour = "Red"), 
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(size = 16, hjust = 0.5)
      ) +
      coord_sf(xlim = c(-42, -39.5), ylim = c(-21.5, -17.5))
  })
  output$tabela_estatisticas <- renderDT({
    df_base <- switch(input$resumo_modelo,
                      "Donkelar" = ESdonk,
                      "CAMS" = EScams,)

    df_filtrado <- df_base %>% 
      filter(Ano == input$resumo_ano)

    media <- mean(df_filtrado$pm2.5, na.rm = TRUE)
    desvio <- sd(df_filtrado$pm2.5, na.rm = TRUE)
    
    max_val <- max(df_filtrado$pm2.5, na.rm = TRUE)

    muni_max <- df_filtrado$name_muni[which.max(df_filtrado$pm2.5)]
    
    min_val <- min(df_filtrado$pm2.5, na.rm = TRUE)

    muni_min <- df_filtrado$name_muni[which.min(df_filtrado$pm2.5)]
    
    tabela_final <- data.frame(
      Indicador = c(
        "Média",
        "Desvio Padrão",
        "Valor Máximo",
        "Município (Máx)",
        "Valor Mínimo",
        "Município (Mín)"
      ),
      Valor = c(
        round(media, 4),      
        round(desvio, 4),
        round(max_val, 4),
        as.character(muni_max), 
        round(min_val, 4),
        as.character(muni_min)
      )
    )
    datatable(tabela_final, 
              options = list(dom = 't', paging = FALSE), 
              rownames = FALSE,
              colnames = c("Métrica Estatística", "Resultado"))
  })
  output$saida_teste <- renderPrint({

    df_base <- if(input$teste_ano == "2022") compara22 else compara23

    df_wide <- df_base %>%
      pivot_wider(names_from = fonte, values_from = pm2.5) %>%
      drop_na() 

    if (input$teste_tipo == "pearson") {
      
      print(paste("--- Teste de Correlação de Pearson (Linear) - Ano", input$teste_ano, "---"))
      cor.test(df_wide$Donkelar, df_wide$cams, method = "pearson")
      
    } else if (input$teste_tipo == "spearman") {
      
      print(paste("--- Teste de Correlação de Spearman (Postos) - Ano", input$teste_ano, "---"))
      cor.test(df_wide$Donkelar, df_wide$cams, method = "spearman")
      
    } else if (input$teste_tipo == "icc") {
      print(paste("--- Intraclass Correlation Coefficient (ICC) - Ano", input$teste_ano, "---"))
      dados_icc <- df_wide %>% select(Donkelar, cams)
      icc(dados_icc, model = "twoway", type = "agreement", unit = "single")
    }
  })
  output$interpretacao_teste <- renderUI({
    if (input$teste_tipo == "icc") {
      helpText("Interpretação ICC: < 0.5 (Ruim), 0.5-0.75 (Moderado), 0.75-0.9 (Bom), > 0.9 (Excelente)")
    } else {
      helpText("Interpretação Correlação: 0 (Sem relação) a 1 (Relação Perfeita). P-value < 0.05 indica significância.")
    }
  })
####
}

shinyApp(ui, server)
