# Módulo para calcular e exibir a taxa de rotatividade
saving_giro_tx_rotatividade_ui <- function(id) {
  # Retorna o UI para exibição da taxa de rotatividade
  ns <- NS(id)
  tagList(
    textOutput(ns("saving_giro_tx_rotatividade_value"))
  )
}

saving_giro_tx_rotatividade_server <- function(id, gi_tbl_prev, bed_availability_tbl, filter_gi) {
  
  # Define a lógica do servidor para calcular a taxa de rotatividade
  moduleServer(id, function(input, output, session) {
    
    # Tabela reativa para calcular a taxa de rotatividade
    saving_giro_tx_rot_tbl <- reactive({ 
      gi_tbl_prev() %>%
        filter(ISOLATED == "Sim") %>%
        mutate(RELEASE_DATE = as.Date(RELEASE_DATE)) %>%
        filter(RELEASE_DATE >= filter_gi$gestao_isolamento_data[1] &
                 RELEASE_DATE <= filter_gi$gestao_isolamento_data[2]) %>%
        left_join(bed_availability_tbl, by = c("RELEASE_DATE" = "bed_date")) %>%
        reframe(tx_rotatividade = n()/mean(bed_n, na.rm = T)) # Calcula a taxa de rotatividade
    })
    
    output$saving_giro_tx_rotatividade_value <- renderText({
      # Verifica se há dados e exibe a taxa de rotatividade formatada
      if(nrow(gi_tbl_prev()) == 0) {
        sgtx_result <- "0" # Se não houver dados, exibe 0
      } else {
        tx_giro_value <- round(saving_giro_tx_rot_tbl()$tx_rotatividade, 1) * 100
        sgtx_result <- format(tx_giro_value, decimal.mark = ",", big.mark = ".", nsmall = 1) # Formata o valor da taxa
      }
      sgtx_result
    })
  })
}
