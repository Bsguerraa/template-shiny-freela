# Módulo para calcular e exibir a taxa de ocupação de leitos
saving_tm_ocupacao_leito_ui <- function(id) {
  # Retorna o UI para exibição da taxa de ocupação de leitos
  ns <- NS(id)
  tagList(
    textOutput(ns("saving_tm_ocupacao_leito_value"))
  )
}

saving_tm_ocupacao_leito_server <- function(id, gi_tbl_prev, filter_gi) {
  # Define a lógica do servidor para calcular a taxa de ocupação de leitos
  moduleServer(id, function(input, output, session) {
    
    # Tabela reativa para calcular a taxa de ocupação de leitos
    tm_ocupacao_leito_tbl <- reactive({ 
      gi_tbl_prev() %>%
        filter(RELEASE_DATE >= filter_gi$gestao_isolamento_data[1]) %>%
        filter(RELEASE_DATE <= filter_gi$gestao_isolamento_data[2]) %>%
        filter(ISOLATED == "Sim") %>%
        reframe(tm_ocupacao_leito = sum(bed_occupation_days, na.rm = T) / n()) # Calcula a taxa de ocupação de leitos
    })
    
    output$saving_tm_ocupacao_leito_value <- renderText({
      # Verifica se há dados e exibe a taxa de ocupação de leitos formatada
      if(nrow(gi_tbl_prev()) == 0) {
        sgtmvlue <- "" # Se não houver dados, não exibe nada
      } else {
        tm_value <- round(tm_ocupacao_leito_tbl()$tm_ocupacao_leito, 1)
        tm_value_format <- format(tm_value, decimal.mark = ",", big.mark = ".", nsmall = 1) # Formata o valor
        sgtmvlue <- paste(tm_value_format, "dias") # Exibe o valor com a unidade "dias"
      }
      sgtmvlue
    })
  })
}
