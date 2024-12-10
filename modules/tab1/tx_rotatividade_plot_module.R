# Módulo UI
ui_tx_rotatividade_month_plot <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("tx_rotatividade_month_plot"))
}

ui_tx_rotatividade_week_plot <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("tx_rotatividade_week_plot"))
}

# Módulo Servidor
server_tx_rotatividade_month_plot <- function(id, filter_gi, saving_giro_tx_rot_month_tbl, saving_giro_tx_rot_sector_month_tbl, 
                                            gi_x_label_month, x_range_gi_month) {
  moduleServer(id, function(input, output, session) {

    output$tx_rotatividade_month_plot <- renderPlotly({
      
      saving_giro_tx_rot_month_tbl <- data.frame(saving_giro_tx_rot_month_tbl())
      
      if(filter_gi$gi_button_sector == "Agregados") {

        if(all(is.na(saving_giro_tx_rot_month_tbl$tx_rotatividade))) {
          max_y_trm = 50
        } else {
          max_trm_value <- max(saving_giro_tx_rot_month_tbl$tx_rotatividade, na.rm = T)
          max_y_trm <- ifelse(max_trm_value < 50, 50, max_trm_value*1.1)
        }

        if(all(is.na(saving_giro_tx_rot_month_tbl$tx_rotatividade))) {
          ptr <- saving_giro_tx_rot_month_tbl %>%
            plot_ly(
              x = ~month,
              y = ~tx_rotatividade,
              hoverinfo = 'none',
              showlegend = FALSE
            )
          
        } else {

          ptr <- saving_giro_tx_rot_month_tbl %>%
            plot_ly(
              x = ~month,
              y = ~tx_rotatividade,
              marker = list(color = '#FFAF42'),
              line = list(color = '#FFAF42'),
              type = "scatter",
              mode = "lines+markers",
              text = ~paste0(str_to_title(format(month, format = "%B %Y")),
                             '<br>Taxa: ', format(round(tx_rotatividade,1), decimal.mark = ",")),
              hoverinfo = 'text',
              showlegend = FALSE
            )
        }

        ptr <- ptr %>%
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
              title = "Taxa de Rotatividade",
              range = c(0, max_y_trm)
            ),
            margin = list(l = 50, r = 50, t = 50, b = 50)
          ) %>%
          config(locale = "pt-BR") # Set the locale to Brazilian Portuguese

      } else {

        color_pallete_trm <- viridis::viridis(length(unique(saving_giro_tx_rot_sector_month_tbl()$DESCRIPTION)))
        
        display_hoverinfo_trm = ifelse(all(is.na(saving_giro_tx_rot_sector_month_tbl()$tx_rotatividade)), 'none','text')

        if(all(is.na(saving_giro_tx_rot_sector_month_tbl()$tx_rotatividade))) {
          max_y_trm = 20
        } else {
          max_trm_value <- max(saving_giro_tx_rot_sector_month_tbl()$tx_rotatividade, na.rm = T)
          max_y_trm <- ifelse(max_trm_value < 20, 20, max_trm_value*1.1)
        }

        ptr <- saving_giro_tx_rot_sector_month_tbl() %>%
          plot_ly(
            x = ~month,
            y = ~tx_rotatividade,
            color = ~DESCRIPTION,
            colors = color_pallete_trm,
            type = "scatter",
            mode = "lines+markers",
            text = ~paste0(DESCRIPTION,': ', format(round(tx_rotatividade,1), decimal.mark = ",")),
            hoverinfo = display_hoverinfo_trm,
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
              title = "Taxa de Rotatividade",
              range = c(0, max_y_trm)
            )
          ) %>%
          config(locale = "pt-BR") # Set the locale to Brazilian Portuguese

        # Mostra legenda somente se existirem valores para os filtros selecionados
        if (!all(is.na(saving_giro_tx_rot_sector_month_tbl()$tx_rotatividade))) {
          ptr <- ptr %>%
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

      return(ptr)

    })
  })
}


server_tx_rotatividade_week_plot <- function(id, filter_gi, gi_tbl_week, saving_giro_tx_rot_week_tbl, saving_giro_tx_rot_sector_week_tbl, 
                                             gi_x_label_week, x_range_gi_week) {
  moduleServer(id, function(input, output, session) {
    
    output$tx_rotatividade_week_plot <- renderPlotly({
      
      trm_x_axis_range <- as.numeric(diff(range(gi_tbl_week()$week)))
      trm_x_tick_label_breaks <- case_when(trm_x_axis_range <= 60 ~ 1,
                                           trm_x_axis_range > 60 & trm_x_axis_range <= 300 ~ 2,
                                           trm_x_axis_range > 300 & trm_x_axis_range <= 540 ~ 3,
                                           trm_x_axis_range > 540 ~ 4)
      
      if(filter_gi$gi_button_sector == "Agregados") {
        
        if(all(is.na(saving_giro_tx_rot_week_tbl()$tx_rotatividade))) {
          max_y_trm = 10
        } else {
          max_trm_value <- max(saving_giro_tx_rot_week_tbl()$tx_rotatividade, na.rm = T)
          max_y_trm <- ifelse(max_trm_value < 10, 10, max_trm_value*1.1)
        }
        
        if(all(is.na(saving_giro_tx_rot_week_tbl()$tx_rotatividade))) {
          ptrm <- saving_giro_tx_rot_week_tbl() %>%
            plot_ly(
              x = ~week,
              y = ~tx_rotatividade,
              hoverinfo = 'none',
              showlegend = FALSE
            )
          
        } else {
          ptrm <- saving_giro_tx_rot_week_tbl() %>%
            plot_ly(
              x = ~week,
              y = ~tx_rotatividade,
              marker = list(color = '#FFAF42'),
              line = list(color = '#FFAF42'),
              type = "scatter",
              mode = "lines+markers",
              text = ~paste0(str_to_title(format(week, format = "%d %B %Y")),
                             '<br>Taxa: ', format(round(tx_rotatividade,1), decimal.mark = ",")),
              hoverinfo = 'text',
              showlegend = FALSE
            )
        }
        
        ptrm <- ptrm |>
          layout(
            xaxis = list(
              showline= T, linewidth=1, linecolor='black',
              title = gi_x_label_week(),
              type = "date",
              tickformat = "%d %b %Y",
              tickangle = 45,
              tickmode = "linear",  # Define espaçamento uniforme
              dtick = trm_x_tick_label_breaks * 7 * 24 * 60 * 60 * 1000,  # semanas em ms #distância entre ticks
              range = x_range_gi_week() + c(-15, 15)
            ),
            yaxis = list(
              showline= T, linewidth=1, linecolor='black',
              title = "Taxa de Rotatividade",
              range = c(0, max_y_trm)
            ),
            margin = list(l = 50, r = 50, t = 50, b = 50)
          ) %>%
          config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
        
      } else {
        
        if(all(is.na(saving_giro_tx_rot_sector_week_tbl()$tx_rotatividade))) {
          max_y_trm = 15
        } else {
          max_trm_value <- max(saving_giro_tx_rot_sector_week_tbl()$tx_rotatividade, na.rm = T)
          max_y_trm <- ifelse(max_trm_value < 15, 15, max_trm_value*1.1)
        }
        
        color_pallete_trm <- viridis::viridis(length(unique(saving_giro_tx_rot_sector_week_tbl()$DESCRIPTION)))
        
        if(all(is.na(saving_giro_tx_rot_sector_week_tbl()$tx_rotatividade))) {
          ptrm <- saving_giro_tx_rot_sector_week_tbl() %>%
            plot_ly(
              x = ~week,
              y = ~tx_rotatividade,
              hoverinfo = 'none',
              showlegend = FALSE
            )
          
        } else {
          ptrm <- saving_giro_tx_rot_sector_week_tbl() %>%
            plot_ly(
              x = ~week,
              y = ~tx_rotatividade,
              color = ~DESCRIPTION,
              colors = color_pallete_trm,
              type = "scatter",
              mode = "lines+markers",
              text = ~paste0(DESCRIPTION, ': ', tx_rotatividade,' (',format(week, format = "%d %B %Y"),')'),
              hoverinfo = 'text',
              showlegend = TRUE
            )
        }
        
        ptrm <- ptrm |>
          layout(
            xaxis = list(
              showline= T, linewidth=1, linecolor='black',
              title = gi_x_label_week(),
              type = "date",
              tickformat = "%d %b %Y",
              tickangle = 45,
              tickmode = "linear",  # Define espaçamento uniforme
              dtick = trm_x_tick_label_breaks * 7 * 24 * 60 * 60 * 1000,  # semanas em ms #distância entre ticks
              range = x_range_gi_week() + c(-15, 15)
            ),
            yaxis = list(
              showline= T, linewidth=1, linecolor='black',
              title = "Taxa de Rotatividade",
              range = c(0, max_y_trm)
            )
            
          ) %>%
          config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
        
        # Mostra legenda somente se existirem valores para os filtros selecionados
        if (!all(is.na(saving_giro_tx_rot_sector_week_tbl()$tx_rotatividade))) {
          ptrm <- ptrm %>%
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
      
      return(ptrm)
      
    })
    
  })
}