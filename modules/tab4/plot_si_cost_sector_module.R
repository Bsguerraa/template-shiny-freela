# UI:
plot_si_cost_sector_ui <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("plot_si_cost_sector"))
}

# SERVER:
plot_si_cost_sector_server <- function(id, saving_int_tbl_prev, filter_saving_int, filter_si_sector) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$plot_si_cost_sector <- renderPlotly({
      
      # Opção A: Filtrar Setor
      if(filter_si_sector$si_sector_button == "Ver somente itens selecionados") {
        
        if (is.null(filter_saving_int$saving_int_setor)) {
          saving_int_tbl_filter_sector <- saving_int_tbl_prev()
        } else {
          saving_int_tbl_filter_sector <- saving_int_tbl_prev() %>%
            dplyr::filter(DESCRIPTION %in% filter_saving_int$saving_int_setor)
        }
        
        # Calcular custo total salvo por Setor
        saving_int_cost_tbl <- saving_int_tbl_filter_sector %>%
          mutate(SAVED_COST = COST - EFFECTIVE_COST) %>%
          group_by(DESCRIPTION) %>%
          reframe(SAVED_COST_TOTAL = sum(SAVED_COST, na.rm = TRUE),
                  COST_TOTAL = sum(COST, na.rm = T),
                  EFFECTIVE_COST_TOTAL = sum(EFFECTIVE_COST, na.rm = T)) %>%
          ungroup()
        
        if(nrow(saving_int_cost_tbl)== 0) {
          saving_int_cost_tbl <- data.frame(DESCRIPTION = sort(unique(aceitabilidade_tbl$DESCRIPTION)),
                                            SAVED_COST_TOTAL = 0,
                                            COST_TOTAL = 0,
                                            EFFECTIVE_COST_TOTAL = 0)
        }
        
        x_lim_upp_cost_a <- max(saving_int_cost_tbl$SAVED_COST_TOTAL, na.rm = T)
        x_lim_upp_cost_a <- ifelse(x_lim_upp_cost_a < 100, 100, x_lim_upp_cost_a)
        
        plot_si_cost_a <- 
          saving_int_cost_tbl %>%
          arrange(desc(SAVED_COST_TOTAL)) %>%
          ggplot(aes(y = fct_reorder(DESCRIPTION, SAVED_COST_TOTAL), x = SAVED_COST_TOTAL,
                     labels = "",
                     text = paste0(DESCRIPTION,
                                   '<br>Custo sem implementação de intervenções: ', format_currency_br(COST_TOTAL),
                                   '<br>Custo efetivo: ', format_currency_br(EFFECTIVE_COST_TOTAL),
                                   '<br>Redução do custo: ', format_currency_br(SAVED_COST_TOTAL)
                     ))) +
          geom_col(fill = "#1F968BFF") +
          theme_minimal() +
          ylab("") +
          xlab("Redução de custo (R$)") +
          labs(title = "Redução do Custo Total de Antibióticos por Setor") +
          theme(axis.line.y = element_line()) +
          scale_x_continuous(limits = c(0,x_lim_upp_cost_a),
                             expand = c(0, 0),
                             labels = f <- function(x) format_currency_no_prefix_br(x))
        
        ggplotly(plot_si_cost_a, tooltip = "text")
      } else { 
        # Opção B: Highlight nos setores selecionados
        
        # Calcular custo total salvo por Setor
        saving_int_cost_tbl <- saving_int_tbl_prev() %>%
          mutate(SAVED_COST = COST - EFFECTIVE_COST) %>%
          group_by(DESCRIPTION) %>%
          reframe(SAVED_COST_TOTAL = sum(SAVED_COST, na.rm = TRUE),
                  COST_TOTAL = sum(COST, na.rm = T),
                  EFFECTIVE_COST_TOTAL = sum(EFFECTIVE_COST, na.rm = T)) %>%
          ungroup() %>%
          mutate(highlight_medication = ifelse(DESCRIPTION %in% filter_saving_int$saving_int_setor, T, F)) %>%
          mutate(DESCRIPTION = factor(DESCRIPTION, levels = sort(unique(aceitabilidade_tbl$DESCRIPTION)))) %>%
          complete(DESCRIPTION, fill = list(SAVED_COST_TOTAL = 0, COST_TOTAL = 0, 
                                            EFFECTIVE_COST_TOTAL = 0,
                                            highlight_medication = FALSE))
        
        
        x_lim_upp_cost_b <- max(saving_int_cost_tbl$SAVED_COST_TOTAL, na.rm = T)
        x_lim_upp_cost_b <- ifelse(x_lim_upp_cost_b < 100, 100, x_lim_upp_cost_b)
        
        plot_si_cost_color <- c("#1F968BFF", "#404788FF")
        
        plot_si_cost_b <- saving_int_cost_tbl %>%
          arrange(desc(SAVED_COST_TOTAL)) %>%
          ggplot(aes(y = fct_reorder(DESCRIPTION, SAVED_COST_TOTAL), x = SAVED_COST_TOTAL,
                     fill = highlight_medication,
                     labels = "",
                     text = paste0(DESCRIPTION,
                                   '<br>Custo sem implementação de intervenções: ', format_currency_br(COST_TOTAL),
                                   '<br>Custo efetivo: ', format_currency_br(EFFECTIVE_COST_TOTAL),
                                   '<br>Redução do custo: ', format_currency_br(SAVED_COST_TOTAL)
                     )
          )) +
          geom_col() +
          scale_fill_manual(values = plot_si_cost_color) +
          theme_minimal() +
          ylab("") +
          xlab("Redução de custo (R$)") +
          labs(title = "Redução do Custo Total de Antibióticos por Setor") +
          theme(axis.line.y = element_line(),
                legend.position='none') +
          scale_x_continuous(limits = c(0,x_lim_upp_cost_b),
                             expand = c(0, 0),
                             labels = f <- function(x) format_currency_no_prefix_br(x))
        
        ggplotly(plot_si_cost_b, tooltip = "text")
      }
    })
    
  })
  
}