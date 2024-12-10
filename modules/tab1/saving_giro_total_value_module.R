# Módulo para calcular e exibir o valor total de economia
saving_giro_total_value_ui <- function(id) {
  # Retorna o UI para exibição do valor total de economia
  ns <- NS(id)
  tagList(
    textOutput(ns("saving_giro_total_value"))
  )
}

saving_giro_total_value_server <- function(id, saving_giro_tbl) {
  # Define a lógica do servidor para calcular o valor total de economia
  moduleServer(id, function(input, output, session) {
    
    output$saving_giro_total_value <- renderText({
      # Verifica se a tabela de economia tem dados e calcula o valor total
      if(nrow(saving_giro_tbl()) == 0) {
        sgtvalue <- format_currency_br(0) # Se não houver dados, exibe 0
      } else {
        sgtvalue <- format_currency_br(sum(saving_giro_tbl()$saving_cost, na.rm = TRUE)) # Calcula a soma dos custos de economia
      }
      sgtvalue
    })
  })
}
