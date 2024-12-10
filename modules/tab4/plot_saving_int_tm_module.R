# UI:
plot_saving_int_tm_ui <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("plot_saving_int_tm_total"))
}

# SERVER:
plot_saving_int_tm_server <- function(id, saving_int_tbl) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$plot_saving_int_tm_total <- renderPlotly({
      
      if(nrow(saving_int_tbl())== 0) {
        saving_tm_comparison_tbl <- data.frame(IMPLEMENTATION_ADHERENCE = factor(c("Alerta convertido","Alerta não aceito")),
                                               MEAN_THERAPY_DURATION = c(0,0)
        )  %>%
          arrange(IMPLEMENTATION_ADHERENCE)
        
        stm_title <- ""
        y_upper_limit_si_tm <- 10
        
      } else {
        saving_tm_comparison_tbl = saving_int_tbl() %>%
          mutate(IMPLEMENTATION_ADHERENCE = factor(IMPLEMENTATION_ADHERENCE,
                                                   levels = c(T,F),
                                                   labels = c("Alerta convertido","Alerta não aceito"))) %>%
          group_by(IMPLEMENTATION_ADHERENCE, .drop = FALSE) %>%
          reframe(MEAN_THERAPY_DURATION = round(mean(THERAPY_DURATION, na.rm = TRUE),1)) %>%
          ungroup() %>%
          arrange(IMPLEMENTATION_ADHERENCE)
        
        y_upper_limit_si_tm <- ifelse(max(saving_tm_comparison_tbl$MEAN_THERAPY_DURATION, na.rm = T) < 10, 
                                      10, 
                                      max(saving_tm_comparison_tbl$MEAN_THERAPY_DURATION, na.rm = T))
        
        if(!any(is.na(saving_tm_comparison_tbl$MEAN_THERAPY_DURATION))){
          saving_tm_reduction = saving_tm_comparison_tbl$MEAN_THERAPY_DURATION[2] - saving_tm_comparison_tbl$MEAN_THERAPY_DURATION[1]
          
          if(saving_tm_reduction>0) {
            stm_title = paste0("Redução média de ", saving_tm_reduction, " dias")
          } else {
            stm_title =   paste0("Aumento média de ", abs(saving_tm_reduction), " dias")
          }
          if(saving_tm_reduction==0) stm_title = "Sem mudança no tempo médio de terapia"
          
        } else {
          
          saving_tm_comparison_tbl <- saving_tm_comparison_tbl %>%
            replace_na(list(MEAN_THERAPY_DURATION = 0))
          
          stm_title = ""
        }
      }
      
      plot_ly(
        data = saving_tm_comparison_tbl,
        x = ~IMPLEMENTATION_ADHERENCE,
        y = ~MEAN_THERAPY_DURATION,
        type = "bar",
        color = ~IMPLEMENTATION_ADHERENCE,
        colors = c("Alerta convertido" = "#4a934a","Alerta não aceito" = "#568b87"),
        hovertemplate = ~paste0(IMPLEMENTATION_ADHERENCE, ":<br>", MEAN_THERAPY_DURATION," dias<extra></extra>"),
        showlegend= FALSE
      ) %>%
        layout(
          legend = 'none',
          barmode = 'stack',
          title = list(text =stm_title, x = 0.5), # Center the title
          xaxis = list(showline= T, linewidth=1, linecolor='black',
                       title = "",
                       tickmode = "linear",  # Define espaçamento uniforme
                       ticklen = 10,              # Increase tick length to push labels further
                       tickfont = list(size = 14) # Define espaçamento uniforme
          ),
          yaxis = list(showline= T, linewidth=1, linecolor='black',
                       title = "Tempo Médio de Uso de IV (Dias)",
                       range = c(0,y_upper_limit_si_tm*1.2)
                       #tickformat = ",.2f"  # Format numbers as currency with 2 decimal places
          ),
          margin = list(t = 50, r = 20, b = 50, l = 80), # Adjust plot margins
          legend = list(title = list(text = ""), orientation = "h")
        )  %>%
        config(locale = "pt-BR",
               displayModeBar = FALSE) %>% # Set the locale to Brazilian Portuguese 
        hide_colorbar()  
    })
    
  })
}