# Módulo UI
ui_tm_ocupacao_leito_month_plot <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("tm_ocupacao_leito_month_plot"))
}

# Módulo UI
ui_tm_ocupacao_leito_week_plot <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("tm_ocupacao_leito_week_plot"))
}


# Módulo Servidor
server_tm_ocupacao_leito_month_plot <- function(id, filter_gi, gitm_month_tbl, gitm_sector_month_tbl, x_range_gi_month) {
  
  moduleServer(id, function(input, output, session) {
  
  output$tm_ocupacao_leito_month_plot <- renderPlotly({
    
    if(filter_gi$gi_button_sector == "Agregados") {
      
      if(all(is.na(gitm_month_tbl()$tm_ocupacao_leito))) {
        max_y_tm = 1
      } else {
        max_tm_value <- max(gitm_month_tbl()$tm_ocupacao_leito, na.rm = T)
        max_y_tm <- ifelse(max_tm_value < 1, 1, max_tm_value*1.1)
      }
      
      if(all(is.na(gitm_month_tbl()$tm_ocupacao_leito))) {
        ptm <- gitm_month_tbl() %>%
          plot_ly(
            x = ~month,
            y = ~tm_ocupacao_leito,
            hoverinfo = 'none',
            showlegend = FALSE
          )
        
      } else {
        ptm <- gitm_month_tbl() %>%
          plot_ly(
            x = ~month,
            y = ~tm_ocupacao_leito,
            marker = list(color = '#0667B5'),
            line = list(color = '#0667B5'),
            type = "scatter",
            mode = "lines+markers",
            text = ~paste0(str_to_title(format(month, format = "%B %Y")),
                           '<br>Tempo Médio: ', format(round(tm_ocupacao_leito,1), decimal.mark = ",")),
            hoverinfo = 'text',
            showlegend = FALSE
          )
      }
      
      ptm <- ptm %>%
        layout(
          xaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "Mês de Entrada do Paciente",
            type = "date",
            tickformat = "%b %Y",
            tickangle = 45,
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = "M1",  # One month intervals
            range = x_range_gi_month() + c(-15, 15)
          ),
          yaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "Média de dias de ocupação do leito",
            range = c(0, max_y_tm)
          ),
          margin = list(l = 50, r = 50, t = 50, b = 50)
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
    } else {
      
      color_pallete_trm <- viridis::viridis(length(unique(gitm_sector_month_tbl()$DESCRIPTION)))
      
      display_hoverinfo_tm = ifelse(all(is.na(gitm_sector_month_tbl()$tm_ocupacao_leito)), 'none','text')
      
      if(all(is.na(gitm_sector_month_tbl()$tm_ocupacao_leito))) {
        max_y_tm = 1
      } else {
        max_tm_value <- max(gitm_sector_month_tbl()$tm_ocupacao_leito, na.rm = T)
        max_y_tm <- ifelse(max_tm_value < 1, 1, max_tm_value*1.1)
      }
      
      ptm <- gitm_sector_month_tbl() %>%
        plot_ly(
          x = ~month,
          y = ~tm_ocupacao_leito,
          color = ~DESCRIPTION,
          colors = color_pallete_trm,
          type = "scatter",
          mode = "lines+markers",
          text = ~paste0(DESCRIPTION,': ', format(round(tm_ocupacao_leito,1), decimal.mark = ","), ' dias'),
          hoverinfo = display_hoverinfo_tm,
          showlegend = TRUE
        ) |>
        layout(
          xaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "Mês de Entrada do Paciente",
            type = "date",
            tickformat = "%b %Y",
            tickangle = 45,
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = "M1",  # One month intervals
            range = x_range_gi_month() + c(-15, 15)
          ),
          yaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "Média de dias de ocupação do leito",
            range = c(0, max_y_tm)
          )
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
      # Mostra legenda somente se existirem valores para os filtros selecionados
      if (!all(is.na(gitm_sector_month_tbl()$tm_ocupacao_leito))) {
        ptm <- ptm %>%
          layout(
            hovermode = "x unified",
            legend = list(
              traceorder = "grouped", # Agrupa itens na legenda
              font = list(family = "Open Sans", size = 12)
            ),
            margin = list(l = 50, r = 50, t = 50, b = 50)
          )
      }
      
    }
    
    return(ptm)
    
  })
   
})
}

# Módulo Servidor
server_tm_ocupacao_leito_week_plot <- function(id, filter_gi, gitm_week_tbl, gitm_sector_week_tbl, x_range_gi_week) {
  
  moduleServer(id, function(input, output, session) {
    
    output$tm_ocupacao_leito_week_plot <- renderPlotly({
      
      tmw_x_axis_range <- as.numeric(diff(range(gitm_week_tbl()$week)))
      tmw_x_tick_label_breaks <- case_when(tmw_x_axis_range <= 60 ~ 1,
                                           tmw_x_axis_range > 60 & tmw_x_axis_range <= 300 ~ 2,
                                           tmw_x_axis_range > 300 & tmw_x_axis_range <= 540 ~ 3,
                                           tmw_x_axis_range > 540 ~ 4)
      
      if(filter_gi$gi_button_sector == "Agregados") {
        
        if(all(is.na(gitm_week_tbl()$tm_ocupacao_leito))) {
          max_y_tm = 1
        } else {
          max_tm_value <- max(gitm_week_tbl()$tm_ocupacao_leito, na.rm = T)
          max_y_tm <- ifelse(max_tm_value < 1, 1, max_tm_value*1.1)
        }
        
        if(all(is.na(gitm_week_tbl()$tm_ocupacao_leito))) {
          ptmm <- gitm_week_tbl() %>%
            plot_ly(
              x = ~week,
              y = ~tm_ocupacao_leito,
              hoverinfo = 'none',
              showlegend = FALSE
            )
          
        } else {
          ptmm <- gitm_week_tbl() %>%
            plot_ly(
              x = ~week,
              y = ~tm_ocupacao_leito,
              marker = list(color = '#0667B5'),
              line = list(color = '#0667B5'),
              type = "scatter",
              mode = "lines+markers",
              text = ~paste0(str_to_title(format(week, format = "%d %B %Y")),
                             '<br>Tempo Médio: ', format(round(tm_ocupacao_leito,1), decimal.mark = ","), ' dias'),
              hoverinfo = 'text',
              showlegend = FALSE
            )
        }
        
        ptmm <- ptmm |>
          layout(
            xaxis = list(
              showline= T, linewidth=1, linecolor='black',
              title = "Semana de Entrada do Paciente",
              type = "date",
              tickformat = "%d %b %Y",
              tickangle = 45,
              tickmode = "linear",  # Define espaçamento uniforme
              dtick = tmw_x_tick_label_breaks * 7 * 24 * 60 * 60 * 1000,  # semanas em ms #distância entre ticks
              range = x_range_gi_week() + c(-15, 15)
            ),
            yaxis = list(
              showline= T, linewidth=1, linecolor='black',
              title = "Média de dias de ocupação do leito",
              range = c(0, max_y_tm)
            ),
            margin = list(l = 50, r = 50, t = 50, b = 50)
          ) %>%
          config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
        
      } else {
        
        if(all(is.na(gitm_sector_week_tbl()$tm_ocupacao_leito))) {
          max_y_tm = 1
        } else {
          max_tm_value <- max(gitm_sector_week_tbl()$tm_ocupacao_leito, na.rm = T)
          max_y_tm <- ifelse(max_tm_value < 1, 1, max_tm_value*1.1)
        }
        
        color_pallete_trm <- viridis::viridis(length(unique(gitm_sector_week_tbl()$DESCRIPTION)))
        
        if(all(is.na(gitm_sector_week_tbl()$tm_ocupacao_leito))) {
          ptmm <- gitm_sector_week_tbl() %>%
            plot_ly(
              x = ~week,
              y = ~tm_ocupacao_leito,
              hoverinfo = 'none',
              showlegend = FALSE
            )
          
        } else {
          ptmm <- gitm_sector_week_tbl() %>%
            plot_ly(
              x = ~week,
              y = ~tm_ocupacao_leito,
              color = ~DESCRIPTION,
              colors = color_pallete_trm,
              type = "scatter",
              mode = "lines+markers",
              text = ~paste0(DESCRIPTION, ': ', tm_ocupacao_leito,' dias (',format(week, format = "%d %B %Y"),')'),
              hoverinfo = 'text',
              showlegend = TRUE
            )
        }
        
        ptmm <- ptmm |>
          layout(
            xaxis = list(
              showline= T, linewidth=1, linecolor='black',
              title = "Semana de Entrada do Paciente",
              type = "date",
              tickformat = "%d %b %Y",
              tickangle = 45,
              tickmode = "linear",  # Define espaçamento uniforme
              dtick = tmw_x_tick_label_breaks * 7 * 24 * 60 * 60 * 1000,  # semanas em ms #distância entre ticks
              range = x_range_gi_week() + c(-15, 15)
            ),
            yaxis = list(
              showline= T, linewidth=1, linecolor='black',
              title = "Média de dias de ocupação do leito",
              range = c(0, max_y_tm)
            )
            
          ) %>%
          config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
        
        # Mostra legenda somente se existirem valores para os filtros selecionados
        if (!all(is.na(gitm_sector_week_tbl()$tm_ocupacao_leito))) {
          ptmm <- ptmm %>%
            layout(
              hovermode = "x unified",
              legend = list(
                traceorder = "grouped", # Agrupa itens na legenda
                font = list(family = "Open Sans", size = 12)
              ),
              margin = list(l = 50, r = 50, t = 50, b = 50)
            )
        }
        
      }
      
      return(ptmm)
      
    })
  })
}