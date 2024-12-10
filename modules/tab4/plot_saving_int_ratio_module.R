# UI:
plot_saving_int_ratio_ui <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("plot_saving_int_ratio"))
}

# SERVER:
plot_saving_int_ratio_server <- function(id, saving_int_tbl, filter_saving_int, filter_si_ratio) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$plot_saving_int_ratio <- renderPlotly({
      
      si_y_lab_ratio = "Implementação (%)"
      
      if(filter_si_ratio$plot_saving_int_time_setting == "Mensal") {
        
        # x axis range for months
        if(as.numeric(diff(as.Date(filter_saving_int$saving_int_date))) < 150) {
          si_range_x_month_ratio <- c(as.Date(filter_saving_int$saving_int_date)[1], as.Date(filter_saving_int$saving_int_date)[1]+150)
        } else {
          si_range_x_month_ratio <- as.Date(filter_saving_int$saving_int_date)
        }
        
        si_range_x_month_ratio[1] <- floor_date(si_range_x_month_ratio[1], unit = "month")
        si_range_x_month_ratio[2] <- ceiling_date(si_range_x_month_ratio[2], unit = "month")
        
        if(nrow(saving_int_tbl()) == 0) {
          
          saving_int_tbl_month <- data.frame(ALERT_MONTH = as.Date(filter_saving_int$saving_int_date),
                                             IMPLEMENTATION_ADHERENCE = NA,
                                             ratio = 0)
        } else {
          saving_int_tbl_month <- saving_int_tbl() %>%
            group_by(ALERT_MONTH) %>%
            reframe(ratio = round(sum(IMPLEMENTATION_ADHERENCE)/n()*100,1)) %>%
            ungroup() %>%
            tidyr::complete(ALERT_MONTH=seq(min(ALERT_MONTH), max(ALERT_MONTH), by="month"),
                            fill = list(ratio = 0))
        }
        
        saving_int_tbl_month <- saving_int_tbl_month %>%
          mutate(ALERT_MONTH_PT = format(ALERT_MONTH, "%B %Y") %>%
                   tools::toTitleCase())
        
        # Create the plotly plot
        saving_int_plotly_ratio <- plot_ly(
          data = saving_int_tbl_month,
          x = ~ALERT_MONTH,
          y = ~ratio,
          type = "bar",
          marker = list(color = '#FFA500'),
          hovertemplate = ~paste0(str_to_title(format(ALERT_MONTH, format = "%B %Y")), 
                                  "<br>Taxa: ", ratio,"%<extra></extra>")
        ) %>%
          layout(
            legend = list(x = 100, y = 0.5),
            xaxis = list(showline= T, linewidth=1, linecolor='black',
                         title = "",
                         tickangle = 45,
                         type = "date",
                         tickmode = "linear",  # Define espaçamento uniforme
                         dtick = "M1",  # One month intervals
                         tickformat = "%b %Y",  # Full month name and year
                         range = si_range_x_month_ratio + c(-15,-1)
            ),
            yaxis = list(showline= T, linewidth=1, linecolor='black',
                         title = si_y_lab_ratio,
                         range = c(0, 110)
            ),
            margin = list(t = 20, r = 20, b = 20, l = 20), # Adjust plot margins
            legend = list(title = list(text = ""), orientation = "h")
          )  %>%
          config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
        
        saving_int_plotly_ratio
        ### ELSE
      } else {
        
        # Configurações para gráfico por semana
        # x axis range for weeks
        if(as.numeric(diff(as.Date(filter_saving_int$saving_int_date))) < 60) {
          si_range_x_week_ratio <- c(as.Date(filter_saving_int$saving_int_date)[1], as.Date(filter_saving_int$saving_int_date)[1]+60)
        } else {
          si_range_x_week_ratio <- as.Date(filter_saving_int$saving_int_date)
        }
        
        si_range_x_week_ratio[1] <- floor_date(si_range_x_week_ratio[1], unit = "week")
        si_range_x_week_ratio[2] <- ceiling_date(si_range_x_week_ratio[2], unit = "week")
        
        si_x_axis_range_ratio <- as.numeric(diff(range(si_range_x_week_ratio)))
        si_x_tick_label_breaks_ratio <- case_when(si_x_axis_range_ratio <= 60 ~ 1,
                                                  si_x_axis_range_ratio > 60 & si_x_axis_range_ratio <= 300 ~ 2,
                                                  si_x_axis_range_ratio > 300 & si_x_axis_range_ratio <= 540 ~ 3,
                                                  si_x_axis_range_ratio > 540 ~ 4)
        
        if(nrow(saving_int_tbl()) == 0) {
          
          saving_int_ratio_tbl_week <- data.frame(ALERT_WEEK = as.Date(filter_saving_int$saving_int_date),
                                                  IMPLEMENTATION_ADHERENCE = NA,
                                                  ratio = 0)
        } else {
          saving_int_ratio_tbl_week <- saving_int_tbl() %>%
            group_by(ALERT_WEEK) %>%
            reframe(ratio = round(sum(IMPLEMENTATION_ADHERENCE)/n()*100,1)) %>%
            ungroup() %>%
            tidyr::complete(ALERT_WEEK=seq(min(ALERT_WEEK), max(ALERT_WEEK), by="week"),
                            fill = list(ratio = 0))
        }
        
        saving_int_ratio_tbl_week <- saving_int_ratio_tbl_week %>%
          mutate(ALERT_WEEK_PT = format(ALERT_WEEK, "%d-%m-%y") %>%
                   tools::toTitleCase())
        
        # Create the plotly plot
        saving_int_plotly_ratio <- plot_ly(
          data = saving_int_ratio_tbl_week,
          x = ~ALERT_WEEK,
          y = ~ratio,
          type = "bar",
          marker = list(color = '#FFA500'),
          hovertemplate = ~paste0("Semana: ", format(ALERT_WEEK_PT, format = "%d-%m-%y"), 
                                  "<br>Taxa: ", ratio,"%<extra></extra>")
        ) %>%
          layout(
            legend = list(x = 100, y = 0.5),
            barmode = 'stack',
            xaxis = list(showline= T, linewidth=1, linecolor='black',
                         title = "",
                         tickangle = 45,
                         type = "date",
                         tickmode = "linear",  # Define espaçamento uniforme
                         dtick = si_x_tick_label_breaks_ratio * 7 * 24 * 60 * 60 * 1000,  # semanas em ms
                         tickformat = "%d %b %Y",  # Formato dos rótulos
                         range = si_range_x_week_ratio + c(-5,-1)
            ),
            yaxis = list(showline= T, linewidth=1, linecolor='black',
                         title = si_y_lab_ratio,
                         range = c(0, 110)
            ),
            margin = list(t = 20, r = 20, b = 10, l = 20), # Adjust plot margins
            legend = list(title = list(text = ""), orientation = "h")
          ) %>%
          config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
        
      } # else semana
      
      saving_int_plotly_ratio
      
    })
    
  })
  
}