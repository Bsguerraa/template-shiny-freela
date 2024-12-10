# UI

##  Custo Evitado Total
saving_total_value_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("saving_total_value"))
  )
}

## Custo Médio Evitado por Conversão
saving_per_conversion_value_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("saving_per_conversion_value"))
  )
}

## Taxa de Conversão Efetivada das Sugestões IV-VO
saving_ratio_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("saving_ratio"))
  )
}

# SERVER

##  Custo Evitado Total
saving_total_value_server <- function(id, saving_int_tbl) {
  
  moduleServer(id, function(input, output, session) {
    
    output$saving_total_value <- renderText({
      saving_int_tbl() %>%
        filter(IMPLEMENTATION_ADHERENCE) %>%
        reframe(TOTAL_COST = format_currency_br(sum(SAVING_COST))) %>%
        unlist()
    })
  })
}

## Custo Médio Evitado por Conversão
saving_per_conversion_value_server <- function(id, saving_int_tbl) {
  
  moduleServer(id, function(input, output, session) {
    
    output$saving_per_conversion_value <- renderText({
      pc_value <- saving_int_tbl() %>%
        filter(IMPLEMENTATION_ADHERENCE) %>%
        reframe(TOTAL_COST = format_currency_br(sum(SAVING_COST)/n())) %>%
        unlist()
      ifelse(is.na(pc_value),format_currency_br(0),pc_value)
    })
  })
}

## Taxa de Conversão Efetivada das Sugestões IV-VO
saving_ratio_server <- function(id, saving_int_tbl) {
  
  moduleServer(id, function(input, output, session) {
    
    output$saving_ratio <- renderText({
      
      if(nrow(saving_int_tbl())==0) {
        "0,0%"
      } else {
        acpt_ratio_value <- saving_int_tbl() %>%
          reframe(ratio = round(sum(IMPLEMENTATION_ADHERENCE)/n()*100,1)) %>%
          unlist()
        
        paste0(format(acpt_ratio_value, decimal.mark = ","), "%") 
      }
    })
  })
}