# UI:
atb_cost_sector_plot_ui <- function(id) {
  ns <- NS(id)
  
  card(
    height = 450,
    full_screen = TRUE,
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        width = 180,
        open = "closed",
        radioGroupButtons(
          inputId = ns("atb_cost_plot_button"),
          label = "Escolha a visualização",
          choices = c("Destacar itens", 
                      "Ver somente itens selecionados"),
          size = "sm",
          direction = "vertical"
        )
      ),
      plotlyOutput(ns("atb_cost_sector_plot"))
    )
  )
}

# SERVER:
atb_cost_sector_plot_server <- function(id, filter_atb_cost, atb_cost_tbl, atb_cost_tbl_prev, format_currency_br) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Convert input to reactive value for module
    filter_atb_sector_cost_ts <- reactiveValues(
      atb_cost_plot_button = "Destacar itens"
    )
    
    # Update reactive values
    observeEvent(input$atb_cost_plot_button, {
      filter_atb_sector_cost_ts$atb_cost_plot_button <- input$atb_cost_plot_button
    })
    
    output$atb_cost_sector_plot <- renderPlotly({
      
      # Opção A: Filtrar medicamentos
      
      if(filter_atb_sector_cost_ts$atb_cost_plot_button == "Ver somente itens selecionados") {
        
        plot_atb_cost <- atb_cost_tbl() %>%
          group_by(DESCRIPTION) %>%
          reframe(TOTAL_COST = sum(TOTAL_COST, na.rm = TRUE)) %>%
          ungroup() %>%
          arrange(desc(TOTAL_COST)) %>%
          ggplot(aes(y = fct_reorder(DESCRIPTION, TOTAL_COST), x = TOTAL_COST,
                     labels = "",
                     text = paste0(DESCRIPTION,
                                   '<br>Custo total: ', format_currency_br(TOTAL_COST)))) +
          geom_col(fill = "#1F77B4") +
          theme_minimal() +
          ylab("") +
          xlab("Custo total (R$)") +
          theme(axis.line.y = element_line()) +
          scale_x_continuous(expand = c(0, 0),
                             labels = label_number(
                               accuracy = 0.01,
                               big.mark = ".", 
                               decimal.mark = ","
                             ))
        
        ggplotly(plot_atb_cost, tooltip = "text")
        
      } else { 
        # Opção B: Highlight nos medicamentos selecionados
        plot_atb_cost2 =  atb_cost_tbl_prev() %>%
          group_by(DESCRIPTION) %>%
          reframe(TOTAL_COST = sum(TOTAL_COST, na.rm = TRUE)) %>%
          ungroup() %>%
          arrange(desc(TOTAL_COST)) %>%
          mutate(highlight_sector = ifelse(DESCRIPTION %in% filter_atb_cost$atb_cost_setor, T, F)) %>%
          ggplot(aes(y = fct_reorder(DESCRIPTION, TOTAL_COST), x = TOTAL_COST,
                     fill = highlight_sector,
                     labels = "",
                     text = paste0(DESCRIPTION,
                                   '<br>Custo total: ', format_currency_br(TOTAL_COST)))) +
          geom_col() +
          scale_fill_manual(values = c("#1F77B4", "orange")) +
          theme_minimal() +
          ylab("") +
          xlab("Custo total (R$)") +
          theme(axis.line.y = element_line(),
                legend.position = 'none') +
          scale_x_continuous(expand = c(0, 0),
                             labels = label_number(
                               accuracy = 0.01,
                               big.mark = ".", 
                               decimal.mark = ","
                             ))
        
        ggplotly(plot_atb_cost2, tooltip = "text")
      }
    })
    
  })
}