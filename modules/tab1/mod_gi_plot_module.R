# O código exibe gráficos mensais e semanais sobre dados de Tempo entre Coleta e Resultado
#   
# #### Lógica e Fluxo Geral
# 
# 1. Principais Objetos Envolvidos:  
# - Tabelas Reactivas:  
# - `gi_tbl_month`, `gi_tbl_week`: Dados agregados por mês/semana.  
# - `gi_tbl_sector`, `gi_tbl_sector_week`: Dados detalhados por setores hospitalares.  
# - Parâmetros Reativos:  
# - `filter_gi`: Define o escopo do gráfico (ex.: "Agregados" ou por setor).  
# - `max_dif_isolamento_*`: Define o limite máximo do eixo Y.  
# - `gi_x_label_*`: Rótulos dos eixos X.  

# UI:
ui_gi_dif_amostra_result_plot_month <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("gi_dif_amostra_result_plot_month"))
}

ui_gi_dif_amostra_result_plot_week <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("gi_dif_amostra_result_plot_week"))
}


# SERVER:
server_gi_dif_amostra_result_plot_month <- function(id, gi_tbl_month, gi_tbl_sector, max_dif_isolamento_month, filter_gi, gi_x_label_month) {
  moduleServer(id, function(input, output, session) {
    
  output$gi_dif_amostra_result_plot_month <- renderPlotly({
    
    if(filter_gi$gi_button_sector == "Agregados") {
      
      display_hoverinfo = ifelse(all(is.na(gi_tbl_month()$Horas)), 'none','text')
      
      p4 <- gi_tbl_month() %>%
        plot_ly(
          x = ~month,
          y = ~Horas,
          marker = list(color = '#08A18E'),
          line = list(color = '#08A18E'),
          type = "scatter",
          mode = "lines+markers",
          text = ~paste0(str_to_title(format(month, format = "%B %Y")),'<br>Tempo: ', format(round(Horas, 1), decimal.mark = ","), ' horas'),
          hoverinfo = display_hoverinfo,
          showlegend = FALSE
        ) |>
        layout(
          shapes = list(
            list(
              type = "line",
              x0 = min(gi_tbl_month()$month, na.rm = TRUE),
              x1 = max(gi_tbl_month()$month, na.rm = TRUE),
              y0 = 24,
              y1 = 24,
              line = list(dash = "dash", color = "red")
            )
          ),
          xaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = gi_x_label_month(),
            type = "date",
            tickformat = "%b %Y",
            tickangle = 45,
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = "M1"  # One month intervals
          ),
          yaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "Tempo (horas)",
            range = c(0, max_dif_isolamento_month$Horas)
          ),
          annotations = list(
            list(
              x = max(gi_tbl_month()$month, na.rm = TRUE),
              y = 26,
              text = "24 horas",
              showarrow = FALSE,
              font = list(color = "red", size = 12),
              xanchor = "right"
            )
          ),
          margin = list(l = 50, r = 50, t = 50, b = 50)
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
    } else {
      
      color_pallete <- viridis::viridis(length(unique(gi_tbl_sector()$DESCRIPTION)))
      
      display_hoverinfo = ifelse(all(is.na(gi_tbl_sector()$Horas)), 'none','text')
      
      p4 <- gi_tbl_sector() %>%
        plot_ly(
          x = ~month,
          y = ~Horas,
          color = ~DESCRIPTION,
          colors = color_pallete,
          type = "scatter",
          mode = "lines+markers",
          text = ~paste0(DESCRIPTION, ': ', format(round(Horas, 1), decimal.mark = ","), ' horas'),
          hoverinfo = display_hoverinfo,
          showlegend = TRUE
        ) |>
        layout(
          shapes = list(
            list(
              type = "line",
              x0 = min(gi_tbl_sector()$month, na.rm = TRUE),
              x1 = max(gi_tbl_sector()$month, na.rm = TRUE),
              y0 = 24,
              y1 = 24,
              line = list(dash = "dash", color = "red")
            )
          ),
          xaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = gi_x_label_month(),
            type = "date",
            tickformat = "%b %Y",
            tickangle = 45,
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = "M1"  # One month intervals
          ),
          yaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "Tempo (horas)",
            range = c(0, max_dif_isolamento_month$Horas)
          )
          
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
      # Mostra legenda somente se existirem valores para os filtros selecionados
      if (!all(is.na(gi_tbl_sector()$Horas))) {
        p4 <- p4 %>%
          layout(
            annotations = list(
              list(
                x = max(gi_tbl_sector()$month, na.rm = TRUE),
                y = 26,
                text = "24 horas",
                showarrow = FALSE,
                font = list(color = "red", size = 12),
                xanchor = "right"
              )
            ),
            hovermode = "x unified",
            legend = list(
              traceorder = "grouped", # Agrupa itens na legenda
              font = list(family = "Open Sans", size = 12)
            ),
            margin = list(l = 50, r = 50, t = 50, b = 50)
          )
      }
      
    }
    
    return(p4)
    
  })
})
}

server_gi_dif_amostra_result_plot_week <- function(id, gi_tbl_week, gi_tbl_sector_week, max_dif_isolamento_week, filter_gi, gi_x_label_week) {
  moduleServer(id, function(input, output, session) {
    
  output$gi_dif_amostra_result_plot_week <- renderPlotly({
    
    gi_dar_x_axis_range <- as.numeric(diff(range(gi_tbl_week()$week)))
    gi_dar_x_tick_label_breaks <- case_when(gi_dar_x_axis_range <= 60 ~ 1,
                                            gi_dar_x_axis_range > 60 & gi_dar_x_axis_range <= 300 ~ 2,
                                            gi_dar_x_axis_range > 300 & gi_dar_x_axis_range <= 540 ~ 3,
                                            gi_dar_x_axis_range > 540 ~ 4)
    
    if(filter_gi$gi_button_sector == "Agregados") {
      
      display_hoverinfo = ifelse(all(is.na(gi_tbl_week()$Horas)), 'none','text')
      
      p4 <- gi_tbl_week() %>%
        plot_ly(
          x = ~week,
          y = ~Horas,
          marker = list(color = '#08A18E'),
          line = list(color = '#08A18E'),
          type = "scatter",
          mode = "lines+markers",
          text = ~paste0(str_to_title(format(week, format = "%d %b %Y")),'<br>Tempo: ', format(round(Horas, 1), decimal.mark = ","), ' horas'),
          hoverinfo = display_hoverinfo,
          showlegend = FALSE
        ) |>
        layout(
          shapes = list(
            list(
              type = "line",
              x0 = min(gi_tbl_week()$week, na.rm = TRUE),
              x1 = max(gi_tbl_week()$week, na.rm = TRUE),
              y0 = 24,
              y1 = 24,
              line = list(dash = "dash", color = "red")
            )
          ),
          xaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = gi_x_label_week(),
            type = "date",
            tickformat = "%d %b %Y",
            tickangle = 45,
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = gi_dar_x_tick_label_breaks * 7 * 24 * 60 * 60 * 1000  # semanas em ms #distância entre ticks
          ),
          yaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "Tempo (horas)",
            range = c(0, max_dif_isolamento_week$Horas)
          ),
          annotations = list(
            list(
              x = max(gi_tbl_week()$week, na.rm = TRUE),
              y = 26,
              text = "24 horas",
              showarrow = FALSE,
              font = list(color = "red", size = 12),
              xanchor = "right"
            )
          ),
          margin = list(l = 50, r = 50, t = 50, b = 50)
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
    } else {
      
      color_pallete <- viridis::viridis(length(unique(gi_tbl_sector_week()$DESCRIPTION)))
      
      display_hoverinfo = ifelse(all(is.na(gi_tbl_sector_week()$Horas)), 'none','text')
      
      p4 <- gi_tbl_sector_week() %>%
        plot_ly(
          x = ~week,
          y = ~Horas,
          color = ~DESCRIPTION,
          colors = color_pallete,
          type = "scatter",
          mode = "lines+markers",
          text = ~paste0(DESCRIPTION, ': ', format(round(Horas, 1), decimal.mark = ","), ' horas (',format(week, format = "%d %B %Y"),')'),
          hoverinfo = display_hoverinfo,
          showlegend = TRUE
        ) |>
        layout(
          shapes = list(
            list(
              type = "line",
              x0 = min(gi_tbl_sector_week()$week, na.rm = TRUE),
              x1 = max(gi_tbl_sector_week()$week, na.rm = TRUE),
              y0 = 24,
              y1 = 24,
              line = list(dash = "dash", color = "red")
            )
          ),
          xaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = gi_x_label_week(),
            hoverformat = "Semana: %d %B %Y",
            type = "date",
            tickformat = "%d %b %Y",
            tickangle = 45,
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = gi_dar_x_tick_label_breaks * 7 * 24 * 60 * 60 * 1000  # semanas em ms #distância entre ticks
          ),
          yaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "Tempo (horas)",
            range = c(0, max_dif_isolamento_week$Horas)
          )
          
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
      # Mostra legenda somente se existirem valores para os filtros selecionados
      if (!all(is.na(gi_tbl_sector_week()$Horas))) {
        p4 <- p4 %>%
          layout(
            annotations = list(
              list(
                x = max(gi_tbl_sector_week()$week, na.rm = TRUE),
                y = 26,
                text = "24 horas",
                showarrow = FALSE,
                font = list(color = "red", size = 12),
                xanchor = "right"
              )
            ),
            hovermode = "x unified",
            legend = list(
              traceorder = "grouped", # Agrupa itens na legenda
              font = list(family = "Open Sans", size = 12)
            ),
            margin = list(l = 50, r = 50, t = 50, b = 50)
          )
      }
      
    }
    
    return(p4)
    
  })
})
}
