# Módulo UI
ui_saving_cost_ocupacao_leito_month <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("saving_cost_ocupacao_leito_month"))
}

ui_saving_cost_ocupacao_leito_week <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("saving_cost_ocupacao_leito_week"))
}

# Módulo Servidor
server_saving_cost_ocupacao_leito_month <- function(id, filter_gi, saving_giro_tbl_month, saving_giro_tbl_sector_month, 
                                                    gi_x_label_month, x_range_gi_month, format_currency_br) {
  moduleServer(id, function(input, output, session) {
    
    output$saving_cost_ocupacao_leito_month <- renderPlotly({
      
      if(filter_gi$gi_button_sector == "Agregados") {
        
        if(all(is.na(saving_giro_tbl_month()$saving_cost))) {
          max_y_scm = 100
        } else {
          max_sc_value <- max(saving_giro_tbl_month()$saving_cost, na.rm = T)
          max_y_scm <- ifelse(max_sc_value < 100, 100, max_sc_value*1.1)
        }      
        
        if(all(is.na(saving_giro_tbl_month()$saving_cost))) {
          pgisc <- saving_giro_tbl_month() %>%
            plot_ly(
              x = ~month,
              y = ~saving_cost,
              hoverinfo = 'none',
              showlegend = FALSE
            )
          
        } else {
          pgisc <- saving_giro_tbl_month() %>%
            plot_ly(
              x = ~month,
              y = ~saving_cost,
              marker = list(color = '#008080'),
              line = list(color = '#008080'),
              type = "scatter",
              mode = "lines+markers",
              text = ~paste0(str_to_title(format(month, format = "%B %Y")),
                             '<br>Economia: ', format_currency_br(saving_cost)),
              hoverinfo = 'text',
              showlegend = FALSE
            )
        }
        
        pgisc <- pgisc %>%
          layout(
            xaxis = list(
              showline= T, linewidth=1, linecolor='black',
              title = gi_x_label_month(),
              type = "date",
              tickformat = "%b %Y",
              tickangle = 45,
              tickmode = "linear",  # Define espaçamento uniforme
              dtick = "M1",  # One month intervals
              range = x_range_gi_month() + c(-15, 15)
            ),
            yaxis = list(
              showline= T, linewidth=1, linecolor='black',
              title = "Valor economizado (Reais)",
              range = c(0, max_y_scm),
              tickformat = ",.2f"  # Format numbers as currency with 2 decimal places
            ),
            margin = list(l = 50, r = 50, t = 50, b = 50)
          ) %>%
          config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
        
      } else {
        
        color_pallete_sc <- viridis::viridis(length(unique(saving_giro_tbl_sector_month()$DESCRIPTION)))
        
        display_hoverinfo_sc = ifelse(all(is.na(saving_giro_tbl_sector_month()$saving_cost)), 'none','text')
        
        if(all(is.na(saving_giro_tbl_sector_month()$saving_cost))) {
          max_y_scm = 100
        } else {
          max_sc_value <- max(saving_giro_tbl_sector_month()$saving_cost, na.rm = T)
          max_y_scm <- ifelse(max_sc_value < 100, 100, max_sc_value*1.1)
        } 
        
        pgisc <- saving_giro_tbl_sector_month() %>%
          plot_ly(
            x = ~month,
            y = ~saving_cost,
            color = ~DESCRIPTION,
            colors = color_pallete_sc,
            type = "scatter",
            mode = "lines+markers",
            text = ~paste0(DESCRIPTION, ': ', format_currency_br(saving_cost)),
            hoverinfo = display_hoverinfo_sc,
            showlegend = TRUE
          ) |>
          layout(
            xaxis = list(
              showline= T, linewidth=1, linecolor='black',
              title = gi_x_label_month(),
              type = "date",
              tickformat = "%b %Y",
              tickangle = 45,
              tickmode = "linear",  # Define espaçamento uniforme
              dtick = "M1",  # One month intervals
              range = x_range_gi_month() + c(-15, 15)
            ),
            yaxis = list(
              showline= T, linewidth=1, linecolor='black',
              title = "Valor economizado (Reais)",
              range = c(0, max_y_scm),
              tickformat = ",.2f"  # Format numbers as currency with 2 decimal places
            )
          ) %>%
          config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
        
        # Mostra legenda somente se existirem valores para os filtros selecionados
        if (!all(is.na(saving_giro_tbl_sector_month()$saving_cost))) {
          pgisc <- pgisc %>%
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
      
      return(pgisc)
      
    })
    
  })
}

# Módulo Servidor
server_saving_cost_ocupacao_leito_week <- function(id, filter_gi, gi_tbl_week, saving_giro_tbl_week, saving_giro_tbl_sector_week, 
                                                   gi_x_label_week, x_range_gi_week, format_currency_br) {
  moduleServer(id, function(input, output, session) {
    output$saving_cost_ocupacao_leito_week <- renderPlotly({
      
      gisc_x_axis_range <- as.numeric(diff(range(gi_tbl_week()$week)))
      gisc_x_tick_label_breaks <- case_when(gisc_x_axis_range <= 60 ~ 1,
                                            gisc_x_axis_range > 60 & gisc_x_axis_range <= 300 ~ 2,
                                            gisc_x_axis_range > 300 & gisc_x_axis_range <= 540 ~ 3,
                                            gisc_x_axis_range > 540 ~ 4)
      
      if(filter_gi$gi_button_sector == "Agregados") {
        
        if(all(is.na(saving_giro_tbl_week()$saving_cost))) {
          max_y_scm = 100
        } else {
          max_sc_value <- max(saving_giro_tbl_week()$saving_cost, na.rm = T)
          max_y_scm <- ifelse(max_sc_value < 100, 100, max_sc_value*1.1)
        }
        
        if(all(is.na(saving_giro_tbl_week()$saving_cost))) {
          pgisc <- saving_giro_tbl_week() %>%
            plot_ly(
              x = ~week,
              y = ~saving_cost,
              hoverinfo = 'none',
              showlegend = FALSE
            )
          
        } else {
          pgisc <- saving_giro_tbl_week() %>%
            plot_ly(
              x = ~week,
              y = ~saving_cost,
              marker = list(color = '#008080'),
              line = list(color = '#008080'),
              type = "scatter",
              mode = "lines+markers",
              text = ~paste0(str_to_title(format(week, format = "%d %B %Y")),
                             '<br>Economia: ', format_currency_br(saving_cost)),
              hoverinfo = 'text',
              showlegend = FALSE
            )
        }
        
        pgisc <- pgisc |>
          layout(
            xaxis = list(
              showline= T, linewidth=1, linecolor='black',
              title = gi_x_label_week(),
              type = "date",
              tickformat = "%b %Y",
              tickangle = 45,
              tickmode = "linear",  # Define espaçamento uniforme
              dtick = gisc_x_tick_label_breaks * 7 * 24 * 60 * 60 * 1000,  # semanas em ms #distância entre ticks
              range = x_range_gi_week() + c(-15, 15)
            ),
            yaxis = list(
              showline= T, linewidth=1, linecolor='black',
              title = "Valor economizado (Reais)",
              range = c(0, max_y_scm),
              tickformat = ",.2f"  # Format numbers as currency with 2 decimal places
            ),
            margin = list(l = 50, r = 50, t = 50, b = 50)
          ) %>%
          config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
        
      } else {
        
        if(all(is.na(saving_giro_tbl_sector_week()$saving_cost))) {
          max_y_scm = 100
        } else {
          max_sc_value <- max(saving_giro_tbl_sector_week()$saving_cost, na.rm = T)
          max_y_scm <- ifelse(max_sc_value < 100, 100, max_sc_value*1.1)
        }
        
        color_pallete_sc <- viridis::viridis(length(unique(saving_giro_tbl_sector_week()$DESCRIPTION)))
        
        display_hoverinfo_sc = ifelse(all(is.na(saving_giro_tbl_sector_week()$saving_cost)), 'none','text')
        
        
        if(all(is.na(saving_giro_tbl_sector_week()$saving_cost))) {
          pgisc <- saving_giro_tbl_sector_week() %>%
            plot_ly(
              x = ~week,
              y = ~saving_cost,
              hoverinfo = 'none',
              showlegend = FALSE
            )
          
        } else {
          pgisc <- saving_giro_tbl_sector_week() %>%
            plot_ly(
              x = ~week,
              y = ~saving_cost,
              color = ~DESCRIPTION,
              colors = color_pallete_sc,
              type = "scatter",
              mode = "lines+markers",
              text = ~paste0(DESCRIPTION, ': ', format_currency_br(saving_cost),' (',format(week, format = "%d %B %Y"),')'),
              hoverinfo = display_hoverinfo_sc,
              showlegend = TRUE
            )
        }
        
        pgisc <- pgisc |>
          layout(
            xaxis = list(
              showline= T, linewidth=1, linecolor='black',
              title = gi_x_label_week(),
              type = "date",
              tickformat = "%d %b %Y",
              tickangle = 45,
              tickmode = "linear",  # Define espaçamento uniforme
              dtick = gisc_x_tick_label_breaks * 7 * 24 * 60 * 60 * 1000,  # semanas em ms #distância entre ticks
              range = x_range_gi_week() + c(-15, 15)
            ),
            yaxis = list(
              showline= T, linewidth=1, linecolor='black',
              title = "Valor economizado (Reais)",
              range = c(0, max_y_scm),
              tickformat = ",.2f"  # Format numbers as currency with 2 decimal places
            )
            
          ) %>%
          config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
        
        # Mostra legenda somente se existirem valores para os filtros selecionados
        if (!all(is.na(saving_giro_tbl_sector_week()$saving_cost))) {
          pgisc <- pgisc %>%
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
      
      return(pgisc)
      
    })
    
  })
}