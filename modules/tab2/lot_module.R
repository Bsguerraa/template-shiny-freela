# Módulo UI
ui_lot_plot <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("lot_plot"))
}

ui_lot_plot_month <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("lot_plot_month"))
}

ui_lot_table <- function(id) {
  ns <- NS(id)
  dataTableOutput(ns("lot_datatable"))
}

# Módulo Servidor
server_lot_plot <- function(id, filter_mic, LOT_tbl, filter_mic_lot) {
  
  moduleServer(id, function(input, output, session) {
    output$lot_plot <- renderPlotly({
      
      # Opção A: Filtrar medicamentos
      
      if(filter_mic_lot$lot_plot_button == "Ver somente itens selecionados") {
        
        if (is.null(filter_mic$setor_mic)) {
          atb_lot_tbl_filter_sector <- LOT_tbl()
        } else {
          atb_lot_tbl_filter_sector <- LOT_tbl() %>%
            dplyr::filter(DESCRIPTION %in% filter_mic$setor_mic)
        }
        
        # Calcular LOT por setor hospitalar
        atb_lot_tbl <- atb_lot_tbl_filter_sector %>%
          group_by(ADMISSION_ID) %>%
          mutate(THERAPY_DURATION = as.numeric(max(THERAPY_END_DATE, na.rm = T) - min(THERAPY_START_DATE, na.rm = T))) %>%
          ungroup() %>%
          group_by(DESCRIPTION) %>%
          reframe(LOT = sum(THERAPY_DURATION)/sum(ADMISSION_DURATION)*1000) %>%
          ungroup()
        
        plot_lot <- atb_lot_tbl %>%
          arrange(desc(LOT)) %>%
          ggplot(aes(y = fct_reorder(DESCRIPTION, LOT), x = LOT,
                     labels = "",
                     text = paste0(DESCRIPTION,
                                   '<br>LOT: ', round(LOT,2)))) +
          geom_col(fill = "#1F77B4") +
          theme_minimal() +
          ylab("") +
          xlab("dias/1000 pacientes-dia") +
          labs(title = "LOT (Length of Treatment)") +
          theme(axis.line.y = element_line()) +
          scale_x_continuous(expand = c(0, 0))
        
        ggplotly(plot_lot, tooltip = "text")
      } else { 
        #if(filter_mic_lot$lot_plot_button == "Destacar itens")
        # Opção B: Highlight nos medicamentos selecionados
        
        # Calcular LOT por setor hospitalar
        atb_lot_tblb <- LOT_tbl() %>%
          group_by(ADMISSION_ID) %>%
          mutate(THERAPY_DURATION = as.numeric(max(THERAPY_END_DATE, na.rm = T) - min(THERAPY_START_DATE, na.rm = T))) %>%
          ungroup() %>%
          group_by(DESCRIPTION) %>%
          reframe(LOT = sum(THERAPY_DURATION)/sum(ADMISSION_DURATION)*1000) %>%
          ungroup() %>%
          mutate(highlight_medication = ifelse(DESCRIPTION %in% filter_mic$setor_mic, T, F))
        
        plot_lot2 <- atb_lot_tblb %>%
          arrange(desc(LOT)) %>%
          ggplot(aes(y = fct_reorder(DESCRIPTION, LOT), x = LOT,
                     fill = highlight_medication,
                     labels = "",
                     text = paste0(DESCRIPTION,
                                   '<br>LOT: ', round(LOT,2)))) +
          geom_col() +
          scale_fill_manual(values = c("#1F77B4", "orange")) +
          theme_minimal() +
          ylab("") +
          xlab("dias/1000 pacientes-dia") +
          labs(title = "LOT (Length of Treatment)") +
          theme(axis.line.y = element_line(),
                legend.position = "none") +
          scale_x_continuous(expand = c(0, 0))
        
        ggplotly(plot_lot2, tooltip = "text")
      }
    })

  })
}

###
server_lot_plot_month <- function(id, LOT_month_tbl, antimicrob_date_range_floor) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$lot_plot_month <- renderPlotly({
      
      if(nrow(LOT_month_tbl())==0) {
        LOT_month_tbl_ <- data.frame(ADM_START_MONTH = as.Date("2024-01-01"),
                                     LOT = NA,
                                     DESCRIPTION = NA)
      } else {
        LOT_month_tbl_ <- LOT_month_tbl()
      }
      
      # Se desejar, definir a paleta de cores aqui
      #color_pallete_lot <- rainbow(length(unique(LOT_month_tbl_$DESCRIPTION)))
      
      display_hoverinfo_lot = ifelse(all(is.na(LOT_month_tbl_$LOT)), 'none','text')
      
      if(all(is.na(LOT_month_tbl_$LOT))) {
        max_y_lot = 10
      } else {
        max_lot_value <- max(LOT_month_tbl_$LOT, na.rm = T)
        max_y_lot <- ifelse(max_lot_value < 10, 10, max_lot_value*1.1)
      }
      
      plotm <- LOT_month_tbl_ %>%
        plot_ly(
          x = ~ADM_START_MONTH,
          y = ~LOT,
          color = ~DESCRIPTION,
          #colors = color_pallete_lot, # Se desejar, definir a paleta de cores aqui
          type = "scatter",
          mode = "lines+markers",
          text = ~paste0(DESCRIPTION,': ', format(round(LOT,1), decimal.mark = ",")),
          hoverinfo = display_hoverinfo_lot,
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
            range = antimicrob_date_range_floor() + c(-15, 15)
          ),
          yaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "dias/1000 pacientes-dia",
            range = c(0, max_y_lot)
          )
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
      # Mostra legenda somente se existirem valores para os filtros selecionados
      if (!all(is.na(LOT_month_tbl_$LOT))) {
        plotm <- plotm %>%
          layout(
            hovermode = "x unified",
            legend = list(
              traceorder = "grouped", # Agrupa itens na legenda
              font = list(family = "Open Sans", size = 12)
            ),
            margin = list(l = 50, r = 50, t = 50, b = 50)
          )
      }
      
      return(plotm)
      
    })
    
  })
}

#
server_lot_table <- function(id, LOT_month_tbl) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$lot_datatable <- renderDataTable({
      
      LOT_month_tbl_wide = LOT_month_tbl() %>%
        mutate(MONTH_YEAR = str_to_title(format(ADM_START_MONTH, format = "%b %Y"))) %>%
        select(DESCRIPTION, MONTH_YEAR, LOT) %>%
        pivot_wider(names_from = MONTH_YEAR, values_from = LOT, id_cols = c(DESCRIPTION)) %>%
        rename('Setor Hospitalar' = DESCRIPTION) %>%
        mutate(across(where(is.numeric), ~ round(.x, digits = 1)))
      
      datatable(LOT_month_tbl_wide, rownames = FALSE,
                options = list(pageLength = 8,
                               dom = 'tpi',
                               language = list(
                                 info = 'Mostrando registros _START_ - _END_ de _TOTAL_',
                                 paginate = list(previous = 'Anterior', `next` = 'Próximo')
                               ) ))
    })
  
    
  })
}