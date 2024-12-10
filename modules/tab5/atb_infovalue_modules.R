# UI

##  Custo Evitado Total
atb_cost_total_value_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("atb_cost_total_value"))
  )
}

##  Custo Médio por Tratamento
atb_mean_cost_value_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("atb_mean_cost_value"))
  )
}

# SERVER

##  Custo Total com Antibióticos
atb_cost_total_value_server <- function(id, atb_cost_tbl, format_currency_br) {
  
  moduleServer(id, function(input, output, session) {
    output$atb_cost_total_value <- renderText({
      atb_cost_tbl() %>%
        reframe(TOTAL_COST = format_currency_br(sum(TOTAL_COST, na.rm = TRUE))) %>%
        unlist()
    })
  })
}

##  Custo Médio por Tratamento
atb_mean_cost_value_server <- function(id, atb_cost_tbl, format_currency_br) {
  
  moduleServer(id, function(input, output, session) {
    
    output$atb_mean_cost_value <- renderText({
      
      if(nrow(atb_cost_tbl())==0) {
        format_currency_br(0)
      } else {
        atb_cost_tbl() %>%
          reframe(mean_cost_treatment = format_currency_br(sum(TOTAL_COST, na.rm = TRUE)/nrow(.))) %>%
          unlist()
      }
    }) 
  })
}