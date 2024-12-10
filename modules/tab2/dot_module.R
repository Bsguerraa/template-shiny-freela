# Módulo UI
ui_dot_plot <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("dot_plot"))
}

ui_dot_plot_month <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("dot_plot_month"))
}

ui_dot_table <- function(id) {
  ns <- NS(id)
  dataTableOutput(ns("dot_datatable"))
}

# Módulo Servidor
server_dot_plot <- function(id, filter_mic, DOT_tbl, filter_mic_dot) {
  moduleServer(id, function(input, output, session) {
    
    output$dot_plot <- renderPlotly({
      
      # Opção A: Filtrar medicamentos
      
      if(filter_mic_dot$dot_plot_button == "Ver somente itens selecionados") {
        
        if (is.null(filter_mic$medicamento_mic)) {
          antimicrob_tbl_final3 <- DOT_tbl()
        } else {
          antimicrob_tbl_final3 <- DOT_tbl() %>%
            dplyr::filter(MEDICATION_NAME %in% filter_mic$medicamento_mic)
        }
        
        # Calcular DOT por medicamento
        
        # Calculo Dias totais de terapia por hospitalização
        antimicrob_tbl_final4 <- antimicrob_tbl_final3 %>%
          # group_by(ADMISSION_ID) %>%
          # mutate(dias_uso_atm_total = sum(as.numeric(THERAPY_END_DATE - THERAPY_START_DATE))) %>%
          # ungroup() %>%
          mutate(dias_uso_atm = as.numeric(THERAPY_END_DATE - THERAPY_START_DATE) ) %>%
          group_by(MEDICATION_NAME, MEDICATION_CLASS_CODE) %>%
          reframe(dias_uso_atm_sum = sum(dias_uso_atm),
                  P = sum(ADMISSION_DURATION)) %>%
          ungroup() %>%
          mutate(DOT = 1000*dias_uso_atm_sum/P) %>%
          # Filtrar para deixar nomes de medicamentos únicos (verificar como será o tratamento com os dados reais)
          group_by(MEDICATION_NAME) %>%
          mutate(n_med_name = 1:n()) %>%
          ungroup() %>%
          filter(n_med_name == 1)  
        
        plot_DOT <- antimicrob_tbl_final4 %>%
          arrange(desc(DOT)) %>%
          ggplot(aes(y = fct_reorder(MEDICATION_NAME, DOT), x = DOT,
                     labels = "",
                     text = paste0(MEDICATION_NAME,
                                   '<br>DOT: ', round(DOT,2)))) +
          geom_col(fill = "#1F77B4") +
          theme_minimal() +
          ylab("") +
          xlab("dias/1000 pacientes-dia") +
          labs(title = "DOT (Days of Therapy)") +
          theme(axis.line.y = element_line()) +
          scale_x_continuous(expand = c(0, 0))
        
        ggplotly(plot_DOT, tooltip = "text")
      } else { 
        #if(filter_mic_dot$dot_plot_button == "Destacar itens")
        # Opção B: Highlight nos medicamentos selecionados
        
        # Calcular DOT por medicamento
        antimicrob_tbl_final3b <- DOT_tbl() %>%
          # group_by(ADMISSION_ID) %>%
          # mutate(dias_uso_atm_total = sum(as.numeric(THERAPY_END_DATE - THERAPY_START_DATE))) %>%
          # ungroup() %>%
          mutate(dias_uso_atm = as.numeric(THERAPY_END_DATE - THERAPY_START_DATE) ) %>%
          
          group_by(MEDICATION_NAME, MEDICATION_CLASS_CODE) %>%
          reframe(dias_uso_atm_sum = sum(dias_uso_atm),
                  P = sum(ADMISSION_DURATION)) %>%
          ungroup() %>%
          mutate(DOT = 1000*dias_uso_atm_sum/P) %>%
          # Filtrar para deixar nomes de medicamentos únicos (verificar como será o tratamento com os dados reais)
          group_by(MEDICATION_NAME) %>%
          mutate(n_med_name = 1:n()) %>%
          ungroup() %>%
          filter(n_med_name == 1) %>%
          mutate(highlight_medication = ifelse(MEDICATION_NAME %in% filter_mic$medicamento_mic, T, F))
        
        plot_DOT2 <- antimicrob_tbl_final3b %>%
          arrange(desc(DOT)) %>%
          ggplot(aes(y = fct_reorder(MEDICATION_NAME, DOT), x = DOT,
                     fill = highlight_medication,
                     labels = "",
                     text = paste0(MEDICATION_NAME,
                                   '<br>DOT: ', round(DOT,2)))) +
          geom_col() +
          scale_fill_manual(values = c("#1F77B4", "orange")) +
          theme_minimal() +
          ylab("") +
          xlab("dias/1000 pacientes-dia") +
          labs(title = "DOT (Days of Therapy)") +
          theme(axis.line.y = element_line(),
                legend.position = "none") +
          scale_x_continuous(expand = c(0, 0))
        
        ggplotly(plot_DOT2, tooltip = "text")
      }
    })
 
  })
}

###
server_dot_plot_month <- function(id, DOT_month_tbl, antimicrob_date_range_floor) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Renderizar o gráfico
    output$dot_plot_month <- renderPlotly({
      
      if(nrow(DOT_month_tbl())==0) {
        DOT_month_tbl_ <- data.frame(ADM_START_MONTH = as.Date("2024-01-01"),
                                     DOT = NA,
                                     MEDICATION_NAME = NA)
      } else {
        DOT_month_tbl_ <- DOT_month_tbl()
      }
      
      # Se desejar, definir a paleta de cores aqui
      #color_pallete_dot <- rainbow(length(unique(DOT_month_tbl_$MEDICATION_NAME)))
      
      display_hoverinfo_dot = ifelse(all(is.na(DOT_month_tbl_$DOT)), 'none','text')
      
      if(all(is.na(DOT_month_tbl_$DOT))) {
        max_y_dot = 10
      } else {
        max_dot_value <- max(DOT_month_tbl_$DOT, na.rm = T)
        max_y_dot <- ifelse(max_dot_value < 10, 10, max_dot_value*1.1)
      }
      
      pdotm <- DOT_month_tbl_ %>%
        plot_ly(
          x = ~ADM_START_MONTH,
          y = ~DOT,
          color = ~MEDICATION_NAME,
          #colors = color_pallete_dot, # Se desejar, definir a paleta de cores aqui
          type = "scatter",
          mode = "lines+markers",
          text = ~paste0(MEDICATION_NAME,': ', format(round(DOT,1), decimal.mark = ",")),
          hoverinfo = display_hoverinfo_dot,
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
            range = c(0, max_y_dot)
          )
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
      # Mostra legenda somente se existirem valores para os filtros selecionados
      if (!all(is.na(DOT_month_tbl_$DOT))) {
        pdotm <- pdotm %>%
          layout(
            hovermode = "x unified",
            legend = list(
              traceorder = "grouped", # Agrupa itens na legenda
              font = list(family = "Open Sans", size = 12)
            ),
            margin = list(l = 50, r = 50, t = 50, b = 50)
          )
      }
      
      return(pdotm)
      
    })
  })
}

#
server_dot_table <- function(id, DOT_month_tbl) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$dot_datatable <- renderDataTable({
      
      DOT_month_tbl_wide = DOT_month_tbl() %>%
        mutate(MONTH_YEAR = str_to_title(format(ADM_START_MONTH, format = "%b %Y"))) %>%
        select(MEDICATION_NAME, MEDICATION_CLASS_CODE, MONTH_YEAR, DOT) %>%
        pivot_wider(names_from = MONTH_YEAR, values_from = DOT, id_cols = c(MEDICATION_NAME, MEDICATION_CLASS_CODE)) %>%
        rename(Antimicrobiano = MEDICATION_NAME,
               'Código ATC' = MEDICATION_CLASS_CODE) %>%
        mutate(across(where(is.numeric), ~ round(.x, digits = 1)))
      
      datatable(DOT_month_tbl_wide, rownames = FALSE,
                options = list(pageLength = 8,
                               dom = 'tpi',
                               language = list(
                                 info = 'Mostrando registros _START_ - _END_ de _TOTAL_',
                                 paginate = list(previous = 'Anterior', `next` = 'Próximo')
                               ) ))
    })    

  })
}