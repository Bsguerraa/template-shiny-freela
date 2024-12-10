# Módulo UI
ui_ddd_plot <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("ddd_plot"))
}

ui_ddd_plot_month <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("ddd_plot_month"))
}

ui_ddd_table <- function(id) {
  ns <- NS(id)
  dataTableOutput(ns("ddd_datatable"))
}

# Módulo Servidor
server_ddd_plot <- function(id, filter_mic, DDD_tbl, filter_mic_ddd) {
  moduleServer(id, function(input, output, session) {
    
    output$ddd_plot <- renderPlotly({
      
      # Opção A: Filtrar medicamentos
      
      if(filter_mic_ddd$ddd_plot_button == "Ver somente itens selecionados") {
        
        if (is.null(filter_mic$medicamento_mic)) {
          antimicrob_tbl_final3 <- DDD_tbl()
        } else {
          antimicrob_tbl_final3 <- DDD_tbl() %>%
            dplyr::filter(MEDICATION_NAME %in% filter_mic$medicamento_mic)
        }
        
        # Calcular DDD por medicamento
        antimicrob_tbl_final4 <- antimicrob_tbl_final3 %>%
          group_by(MEDICATION_NAME, MEDICATION_CLASS_CODE) %>%
          reframe(A_ratio_B = sum(DD)/unique(DDD_OMS),
                  P = sum(ADMISSION_DURATION)) %>%
          ungroup() %>%
          mutate(DDD = A_ratio_B/P*1000) %>%
          # Filtrar para deixar nomes de medicamentos únicos (verificar como será o tratamento com os dados reais)
          group_by(MEDICATION_NAME) %>%
          mutate(n_med_name = 1:n()) %>%
          ungroup() %>%
          filter(n_med_name == 1)  
        
        plot_ddd <- antimicrob_tbl_final4 %>%
          arrange(desc(DDD)) %>%
          ggplot(aes(y = fct_reorder(MEDICATION_NAME, DDD), x = DDD,
                     labels = "",
                     text = paste0(MEDICATION_NAME,
                                   '<br>DDD: ', round(DDD,2)))) +
          geom_col(fill = "#1F77B4") +
          theme_minimal() +
          ylab("") +
          xlab("g/1000 pacientes-dia") +
          labs(title = "DDD (Dose Diária Definida)") +
          theme(axis.line.y = element_line()) +
          scale_x_continuous(expand = c(0, 0))
        
        ggplotly(plot_ddd, tooltip = "text")
      } else { 
        #if(filter_mic_ddd$ddd_plot_button == "Destacar itens")
        # Opção B: Highlight nos medicamentos selecionados
        
        # Calcular DDD por medicamento
        antimicrob_tbl_final3b <- DDD_tbl() %>%
          group_by(MEDICATION_NAME, MEDICATION_CLASS_CODE) %>%
          reframe(A_ratio_B = sum(DD)/unique(DDD_OMS),
                  P = sum(ADMISSION_DURATION)) %>%
          ungroup() %>%
          mutate(DDD = A_ratio_B/P*1000) %>%
          # Filtrar para deixar nomes de medicamentos únicos (verificar como será o tratamento com os dados reais)
          group_by(MEDICATION_NAME) %>%
          mutate(n_med_name = 1:n()) %>%
          ungroup() %>%
          filter(n_med_name == 1) %>%
          mutate(highlight_medication = ifelse(MEDICATION_NAME %in% filter_mic$medicamento_mic, T, F))
        
        plot_ddd2 <- antimicrob_tbl_final3b %>%
          arrange(desc(DDD)) %>%
          ggplot(aes(y = fct_reorder(MEDICATION_NAME, DDD), x = DDD,
                     fill = highlight_medication,
                     labels = "",
                     text = paste0(MEDICATION_NAME,
                                   '<br>DDD: ', round(DDD,2)))) +
          geom_col() +
          scale_fill_manual(values = c("#1F77B4", "orange")) +
          theme_minimal() +
          ylab("") +
          xlab("g/1000 pacientes-dia") +
          labs(title = "DDD (Dose Diária Definida)") +
          theme(axis.line.y = element_line(),
                legend.position = "none") +
          scale_x_continuous(expand = c(0, 0))
        
        ggplotly(plot_ddd2, tooltip = "text")
      }
    })
  })
}

###
server_ddd_plot_month <- function(id, DDD_month_tbl, antimicrob_date_range_floor) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Renderizar o gráfico
    output$ddd_plot_month <- renderPlotly({
      
      if (nrow(DDD_month_tbl()) == 0) {
        DDD_month_tbl_ <- data.frame(
          ADM_START_MONTH = as.Date("2024-01-01"),
          DDD = NA,
          MEDICATION_NAME = NA
        )
       } else {
        DDD_month_tbl_ <- DDD_month_tbl()
      }
      
      display_hoverinfo_ddd <- ifelse(all(is.na(DDD_month_tbl_$DDD)), "none", "text")
      
      max_y_ddd <- if (all(is.na(DDD_month_tbl_$DDD))) {
        10
      } else {
        max_ddd_value <- max(DDD_month_tbl_$DDD, na.rm = TRUE)
        ifelse(max_ddd_value < 10, 10, max_ddd_value * 1.1)
      }
      
      pdddm <- DDD_month_tbl_ %>%
        plot_ly(
          x = ~ADM_START_MONTH,
          y = ~DDD,
          color = ~MEDICATION_NAME,
          type = "scatter",
          mode = "lines+markers",
          text = ~paste0(
            MEDICATION_NAME, ": ",
            format(round(DDD, 2), decimal.mark = ",")
          ),
          hoverinfo = display_hoverinfo_ddd,
          showlegend = TRUE
        ) |>
        layout(
          xaxis = list(
            showline = TRUE, linewidth = 1, linecolor = "black",
            title = "Mês de Entrada do Paciente",
            type = "date",
            tickformat = "%b %Y",
            tickangle = 45,
            tickmode = "linear",
            dtick = "M1",
            range = antimicrob_date_range_floor() + c(-15, 15)
          ),
          yaxis = list(
            showline = TRUE, linewidth = 1, linecolor = "black",
            title = "g/1000 pacientes-dia",
            range = c(0, max_y_ddd)
          )
        ) %>%
        config(locale = "pt-BR")
      
      if (!all(is.na(DDD_month_tbl_$DDD))) {
        pdddm <- pdddm %>%
          layout(
            hovermode = "x unified",
            legend = list(
              traceorder = "grouped",
              font = list(family = "Open Sans", size = 12)
            ),
            margin = list(l = 50, r = 50, t = 50, b = 50)
          )
      }
      
      return(pdddm)
    })
  })
}

#
server_ddd_table <- function(id, DDD_month_tbl) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$ddd_datatable <- renderDataTable({
      
      DDD_month_tbl_wide = DDD_month_tbl() %>%
        mutate(MONTH_YEAR = str_to_title(format(ADM_START_MONTH, format = "%b %Y"))) %>%
        select(MEDICATION_NAME, MEDICATION_CLASS_CODE, MONTH_YEAR, DDD) %>%
        pivot_wider(names_from = MONTH_YEAR, values_from = DDD, id_cols = c(MEDICATION_NAME, MEDICATION_CLASS_CODE)) %>%
        rename(Antimicrobiano = MEDICATION_NAME,
               'Código ATC' = MEDICATION_CLASS_CODE) %>%
        mutate(across(where(is.numeric), ~ round(.x, digits = 1)))
      
      datatable(DDD_month_tbl_wide, rownames = FALSE,
                options = list(pageLength = 8,
                               dom = 'tpi',
                               language = list(
                                 info = 'Mostrando registros _START_ - _END_ de _TOTAL_',
                                 paginate = list(previous = 'Anterior', `next` = 'Próximo')
                               ) ))
    })
    
  })
}