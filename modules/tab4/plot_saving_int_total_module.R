# UI:
plot_saving_int_total_ui <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("plot_saving_int_total"))
}

# SERVER:
plot_saving_int_total_server <- function(id, saving_int_tbl, format_currency_br) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$plot_saving_int_total <- renderPlotly({
      
      if(nrow(saving_int_tbl())== 0) {
        saving_comparison_tbl <- data.frame(IMPLEMENTATION_ADHERENCE = factor(c("Alerta convertido","Alerta não aceito")),
                                            TOTAL_COST = c(0,0)
        )  %>%
          arrange(IMPLEMENTATION_ADHERENCE)
        
        y_upper_limit_si <- 100
        sc_title = ""
        
      } else {
        
        saving_comparison_tbl = saving_int_tbl() %>%
          mutate(IMPLEMENTATION_ADHERENCE = factor(IMPLEMENTATION_ADHERENCE,
                                                   levels = c(T,F),
                                                   labels = c("Economia Efetivada","Economia Potencial"))) %>%
          group_by(IMPLEMENTATION_ADHERENCE, .drop = FALSE) %>%
          reframe(TOTAL_COST = sum(SAVING_COST)) %>%
          arrange(IMPLEMENTATION_ADHERENCE)
        
        y_upper_limit_si <- ifelse(max(saving_comparison_tbl$TOTAL_COST, na.rm = T) < 100, 
                                   100, 
                                   max(saving_comparison_tbl$TOTAL_COST, na.rm = T))
        
        if(!(any(is.na(saving_comparison_tbl$TOTAL_COST)) | any(saving_comparison_tbl$TOTAL_COST == 0))){
          saving_cost_reduction = saving_comparison_tbl$TOTAL_COST[1] - saving_comparison_tbl$TOTAL_COST[2]
          
          sc_title = paste0("Diferença entre economia efetivada e potencial: ", format_currency_br(saving_cost_reduction))
        } else {
          sc_title = "" 
        }
      }
      
      # Create the plotly plot
      # acpt_plotly <- 
      plot_ly(
        data = saving_comparison_tbl,
        x = ~IMPLEMENTATION_ADHERENCE,
        y = ~TOTAL_COST,
        type = "bar",
        color = ~IMPLEMENTATION_ADHERENCE,
        colors = c("Economia Efetivada" = "#008080","Economia Potencial" = "#EFBF04"),
        hovertemplate = ~paste0(IMPLEMENTATION_ADHERENCE, ":<br>", format_currency_br(TOTAL_COST),"<extra></extra>"),
        showlegend= FALSE
      ) %>%
        layout(
          legend = 'none',
          barmode = 'stack',
          title = list(text =sc_title, x = 0.5), # Center the title
          xaxis = list(showline= T, linewidth=1, linecolor='black',
                       title = "",
                       tickmode = "linear",
                       ticklen = 10,              # Increase tick length to push labels further
                       tickfont = list(size = 14) # Define espaçamento uniforme
          ),
          yaxis = list(showline= T, linewidth=1, linecolor='black',
                       title = list(text = "Valor Total (Reais)",
                                    standoff = 20),          # Increase the distance (in pixels)
                       tickformat = ",.2f",  # Format numbers as currency with 2 decimal places
                       range = c(0,y_upper_limit_si*1.2)
          ),
          margin = list(t = 50, r = 20, b = 50, l = 80), # Adjust plot margins
          legend = list(title = list(text = ""), orientation = "h")
        )  %>%
        config(locale = "pt-BR", # Set the locale to Brazilian Portuguese 
               displayModeBar = FALSE) %>% 
        hide_colorbar() 
    })
    
})
}