# UI:
plot_aceitabilidade_tm_ui <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("plot_aceitabilidade_tm"))
}

plot_aceitabilidade_implemen_tm_ui <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("plot_aceitabilidade_implem_tm"))
}

# Servidor:
plot_aceitabilidade_tm_server <- function(id, tm_aceitabilidade_tbl_filtered_max_month, tm_aceitabilidade_tbl_filtered_max_week, 
                                          filter_aceitabilidade, aceitabilidade_tbl_filtered, filter_acpt_i_n) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$plot_aceitabilidade_tm <- renderPlotly({
      
      y_lab = "Horas"
      plot_title_acpt = "Intervalo Médio de Resposta às Intervenções Sugeridas"
      
      if(filter_acpt_i_n$plot_acpt_tm_time_setting == "Mensal") {
        
        max_y_month_tm <- ifelse(tm_aceitabilidade_tbl_filtered_max_month()$max_value_t_acpt_mean < 10, 10, tm_aceitabilidade_tbl_filtered_max_month()$max_value_t_acpt_mean)
        
        # x axis range for months
        if(as.numeric(diff(as.Date(filter_aceitabilidade$aceitabilidade_date))) < 150) {
          range_x_month_tm <- c(as.Date(filter_aceitabilidade$aceitabilidade_date)[1], as.Date(filter_aceitabilidade$aceitabilidade_date)[1]+150)
        } else {
          range_x_month_tm <- as.Date(filter_aceitabilidade$aceitabilidade_date)
        }
        
        range_x_month_tm[1] <- floor_date(range_x_month_tm[1], unit = "month")
        range_x_month_tm[2] <- ceiling_date(range_x_month_tm[2], unit = "month")
        
        if(nrow(aceitabilidade_tbl_filtered()) == 0) {
          
          aceitabilidade_tm_tbl_month <- data.frame(ALERT_MONTH = as.Date(filter_aceitabilidade$aceitabilidade_date),
                                                    ALERT_ADHERENCE = NA,
                                                    tempo_resposta_medio = 0)
        } else {
          aceitabilidade_tm_tbl_month <- aceitabilidade_tbl_filtered() %>%
            mutate(tempo_resposta = as.numeric(difftime(ALERT_ADHERENCE_DATE, ALERT_DATE, units  = "hours"))) %>%
            group_by(ALERT_MONTH, ALERT_ADHERENCE) %>%
            reframe(tempo_resposta_medio = round(mean(tempo_resposta, na.rm = TRUE),2)) %>%
            ungroup() %>%
            mutate(ALERT_ADHERENCE = factor(ALERT_ADHERENCE,
                                            levels = c(T,F),
                                            labels = c("Aceitas","Rejeitadas"))) %>%
            tidyr::complete(ALERT_ADHERENCE,
                            ALERT_MONTH=seq(min(ALERT_MONTH), max(ALERT_MONTH), by="month"),
                            fill = list(tempo_resposta_medio = 0))
        }
        
        label_color_tbl_acpt_tm <- data.frame(label_name = c("Aceitas", "Rejeitadas"),
                                              color = c('#1f77b4','#AA1D30')) %>%
          filter(label_name %in% unique(aceitabilidade_tm_tbl_month$ALERT_ADHERENCE))
        
        aceitabilidade_tm_tbl_month <- aceitabilidade_tm_tbl_month %>%
          mutate(ALERT_MONTH_PT = format(ALERT_MONTH, "%B %Y") %>%
                   tools::toTitleCase())
        
        # Create the plotly plot
        acpt_tm_plotly <- plot_ly(
          data = aceitabilidade_tm_tbl_month,
          x = ~ALERT_MONTH,
          y = ~tempo_resposta_medio,
          type = "bar",
          color = ~ALERT_ADHERENCE,
          colors = setNames(label_color_tbl_acpt_tm$color, label_color_tbl_acpt_tm$label_name),
          hovertemplate = ~paste0(str_to_title(format(ALERT_MONTH, format = "%B %Y")), "<br>", ALERT_ADHERENCE,": ", tempo_resposta_medio," horas<extra></extra>")
        ) %>%
          layout(
            legend = list(x = 100, y = 0.5),
            barmode = 'stack',
            title = list(text = plot_title_acpt, x = 0.5), # Center the title
            xaxis = list(showline= T, linewidth=1, linecolor='black',
                         title = "",
                         tickangle = 45,
                         type = "date",
                         tickmode = "linear",  # Define espaçamento uniforme
                         dtick = "M1",  # One month intervals
                         tickformat = "%b %Y",  # Full month name and year
                         range = range_x_month_tm + c(-15,-1)
            ),
            yaxis = list(showline= T, linewidth=1, linecolor='black',
                         title = y_lab,
                         range = c(0, max_y_month_tm)
            ),
            margin = list(t = 50, r = 20, b = 120, l = 20), # Adjust plot margins
            legend = list(title = list(text = ""), orientation = "h")
          )  %>%
          config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
        
        ### ELSE
      } else {
        
        # Configurações para gráfico por semana
        
        max_y_week_tm <- ifelse(tm_aceitabilidade_tbl_filtered_max_week()$max_value_t_acpt_mean < 10, 10,tm_aceitabilidade_tbl_filtered_max_week()$max_value_t_acpt_mean)
        
        # x axis range for weeks
        if(as.numeric(diff(as.Date(filter_aceitabilidade$aceitabilidade_date))) < 60) {
          range_x_week_tm <- c(as.Date(filter_aceitabilidade$aceitabilidade_date)[1], as.Date(filter_aceitabilidade$aceitabilidade_date)[1]+60)
        } else {
          range_x_week_tm <- as.Date(filter_aceitabilidade$aceitabilidade_date)
        }
        
        range_x_week_tm[1] <- floor_date(range_x_week_tm[1], unit = "week")
        range_x_week_tm[2] <- ceiling_date(range_x_week_tm[2], unit = "week")
        
        x_axis_range_tm <- as.numeric(diff(range(range_x_week_tm)))
        x_tick_label_breaks_tm <- case_when(x_axis_range_tm <= 60 ~ 1,
                                            x_axis_range_tm > 60 & x_axis_range_tm <= 300 ~ 2,
                                            x_axis_range_tm > 300 & x_axis_range_tm <= 540 ~ 3,
                                            x_axis_range_tm > 540 ~ 4)
        
        if(nrow(aceitabilidade_tbl_filtered()) == 0) {
          
          aceitabilidade_tm_tbl_week <- data.frame(ALERT_WEEK = as.Date(filter_aceitabilidade$aceitabilidade_date),
                                                   ALERT_ADHERENCE = NA,
                                                   tempo_resposta_medio = 0)
        } else {
          aceitabilidade_tm_tbl_week<- aceitabilidade_tbl_filtered() %>%
            mutate(tempo_resposta = as.numeric(difftime(ALERT_ADHERENCE_DATE, ALERT_DATE, units  = "hours"))) %>%
            group_by(ALERT_WEEK, ALERT_ADHERENCE) %>%
            reframe(tempo_resposta_medio = round(mean(tempo_resposta, na.rm = TRUE),2)) %>%
            ungroup() %>%
            mutate(ALERT_ADHERENCE = factor(ALERT_ADHERENCE,
                                            levels = c(T,F),
                                            labels = c("Aceitas","Rejeitadas"))) %>%
            tidyr::complete(ALERT_ADHERENCE,
                            ALERT_WEEK=seq(min(ALERT_WEEK), max(ALERT_WEEK), by="week"),
                            fill = list(tempo_resposta_medio = 0))
        }
        
        label_color_tbl_acpt_tm <- data.frame(label_name = c("Aceitas", "Rejeitadas"),
                                              color = c('#1f77b4','#AA1D30')) %>%
          filter(label_name %in% unique(aceitabilidade_tm_tbl_week$ALERT_ADHERENCE))
        
        aceitabilidade_tm_tbl_week <- aceitabilidade_tm_tbl_week %>%
          mutate(ALERT_WEEK_PT = format(ALERT_WEEK, "%d-%m-%y") %>%
                   tools::toTitleCase())
        
        # Create the plotly plot
        acpt_tm_plotly <- plot_ly(
          data = aceitabilidade_tm_tbl_week,
          x = ~ALERT_WEEK,
          y = ~tempo_resposta_medio,
          type = "bar",
          color = ~ALERT_ADHERENCE,
          colors = setNames(label_color_tbl_acpt_tm$color, label_color_tbl_acpt_tm$label_name),
          hovertemplate = ~paste0("Semana: ", format(ALERT_WEEK_PT, format = "%d-%m-%y"), "<br>", ALERT_ADHERENCE,": ", tempo_resposta_medio," horas<extra></extra>")
        ) %>%
          layout(
            legend = list(x = 100, y = 0.5),
            barmode = 'stack',
            title = list(text = plot_title_acpt, x = 0.5), # Center the title
            xaxis = list(showline= T, linewidth=1, linecolor='black',
                         title = "",
                         tickangle = 45,
                         type = "date",
                         tickmode = "linear",  # Define espaçamento uniforme
                         dtick = x_tick_label_breaks_tm * 7 * 24 * 60 * 60 * 1000,  # semanas em ms
                         tickformat = "%d %b %Y",  # Formato dos rótulos
                         range = range_x_week_tm + c(-5,-1)
            ),
            yaxis = list(showline= T, linewidth=1, linecolor='black',
                         title = y_lab,
                         range = c(0, max_y_week_tm)
            ),
            margin = list(t = 50, r = 20, b = 120, l = 20), # Adjust plot margins
            legend = list(title = list(text = ""), orientation = "h")
          ) %>%
          config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
        
      } # else semana
      
      acpt_tm_plotly
      
    })
  })
}

plot_aceitabilidade_implemen_tm_server <- function(id, tm_aceitabilidade_implem_tbl_filtered_max_month, 
                                                   tm_aceitabilidade_implem_tbl_filtered_max_week, filter_aceitabilidade, 
                                                   aceitabilidade_implem_tbl_filtered, filter_acpt_i_n) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Implementação ao longo do tempo (Tempo médio de resposta)
    
    output$plot_aceitabilidade_implem_tm <- renderPlotly({
      
      y_lab = "Horas"
      plot_title_acpt = "Intervalo Médio de Resposta às Intervenções Sugeridas"
      
      if(filter_acpt_i_n$plot_acpt_tm_time_setting == "Mensal") {
        
        max_y_month_tm_i <- ifelse(tm_aceitabilidade_implem_tbl_filtered_max_month()$max_value_t_acpt_mean < 10, 10, tm_aceitabilidade_implem_tbl_filtered_max_month()$max_value_t_acpt_mean)
        
        # x axis range for months
        if(as.numeric(diff(as.Date(filter_aceitabilidade$aceitabilidade_date))) < 150) {
          range_x_month_tm_i <- c(as.Date(filter_aceitabilidade$aceitabilidade_date)[1], as.Date(filter_aceitabilidade$aceitabilidade_date)[1]+150)
        } else {
          range_x_month_tm_i <- as.Date(filter_aceitabilidade$aceitabilidade_date)
        }
        
        range_x_month_tm_i[1] <- floor_date(range_x_month_tm_i[1], unit = "month")
        range_x_month_tm_i[2] <- ceiling_date(range_x_month_tm_i[2], unit = "month")
        
        if(nrow(aceitabilidade_implem_tbl_filtered()) == 0) {
          
          aceitabilidade_implem_tm_i_tbl_month <- data.frame(IMPLEMENTATION_MONTH = as.Date(filter_aceitabilidade$aceitabilidade_date),
                                                             IMPLEMENTATION_ADHERENCE = NA,
                                                             tempo_resposta_medio = 0)
          
        } else {
          aceitabilidade_implem_tm_i_tbl_month <- aceitabilidade_implem_tbl_filtered() %>%
            mutate(tempo_resposta = as.numeric(difftime(ALERT_IMPLEMENTATION_DATE, ALERT_ADHERENCE_DATE, units  = "hours"))) %>%
            group_by(IMPLEMENTATION_MONTH, IMPLEMENTATION_ADHERENCE) %>%
            reframe(tempo_resposta_medio = round(mean(tempo_resposta, na.rm = TRUE),2)) %>%
            ungroup() %>%
            mutate(IMPLEMENTATION_ADHERENCE = factor(IMPLEMENTATION_ADHERENCE,
                                                     levels = c(T,F),
                                                     labels = c("Aceitas","Rejeitadas"))) %>%
            tidyr::complete(IMPLEMENTATION_ADHERENCE,
                            IMPLEMENTATION_MONTH=seq(min(IMPLEMENTATION_MONTH), max(IMPLEMENTATION_MONTH), by="month"),
                            fill = list(tempo_resposta_medio = 0))
        }
        
        label_color_tbl_acpt_tm_i <- data.frame(label_name = c("Aceitas", "Rejeitadas"),
                                                color = c('#1f77b4','#AA1D30')) %>%
          filter(label_name %in% unique(aceitabilidade_implem_tm_i_tbl_month$IMPLEMENTATION_ADHERENCE))
        
        aceitabilidade_implem_tm_i_tbl_month <- aceitabilidade_implem_tm_i_tbl_month %>%
          mutate(IMPLEMENTATION_MONTH_PT = format(IMPLEMENTATION_MONTH, "%B %Y") %>%
                   tools::toTitleCase())
        
        # Create the plotly plot
        acpt_tm_i_plotly <- plot_ly(
          data = aceitabilidade_implem_tm_i_tbl_month,
          x = ~IMPLEMENTATION_MONTH,
          y = ~tempo_resposta_medio,
          type = "bar",
          color = ~IMPLEMENTATION_ADHERENCE,
          colors = setNames(label_color_tbl_acpt_tm_i$color, label_color_tbl_acpt_tm_i$label_name),
          hovertemplate = ~paste0(str_to_title(format(IMPLEMENTATION_MONTH, format = "%B %Y")), "<br>", IMPLEMENTATION_ADHERENCE,": ", tempo_resposta_medio," horas<extra></extra>")
        ) %>%
          layout(
            legend = list(x = 100, y = 0.5),
            barmode = 'stack',
            title = list(text = plot_title_acpt, x = 0.5), # Center the title
            xaxis = list(showline= T, linewidth=1, linecolor='black',
                         title = "",
                         tickangle = 45,
                         type = "date",
                         tickmode = "linear",  # Define espaçamento uniforme
                         dtick = "M1",  # One month intervals
                         tickformat = "%b %Y",  # Full month name and year
                         range = range_x_month_tm_i + c(-15,-1)
            ),
            yaxis = list(showline= T, linewidth=1, linecolor='black',
                         title = y_lab,
                         range = c(0, max_y_month_tm_i)
            ),
            margin = list(t = 50, r = 20, b = 120, l = 20), # Adjust plot margins
            legend = list(title = list(text = ""), orientation = "h")
          )  %>%
          config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
        
        ### ELSE
      } else {
        
        # Configurações para gráfico por semana
        max_y_week_tm_i <- ifelse(tm_aceitabilidade_implem_tbl_filtered_max_week()$max_value_t_acpt_mean < 10,
                                  10,
                                  tm_aceitabilidade_implem_tbl_filtered_max_week()$max_value_t_acpt_mean)
        
        # x axis range for weeks
        if(as.numeric(diff(as.Date(filter_aceitabilidade$aceitabilidade_date))) < 60) {
          range_x_week_tm_i <- c(as.Date(filter_aceitabilidade$aceitabilidade_date)[1], as.Date(filter_aceitabilidade$aceitabilidade_date)[1]+60)
        } else {
          range_x_week_tm_i <- as.Date(filter_aceitabilidade$aceitabilidade_date)
        }
        
        range_x_week_tm_i[1] <- floor_date(range_x_week_tm_i[1], unit = "week")
        range_x_week_tm_i[2] <- ceiling_date(range_x_week_tm_i[2], unit = "week")
        
        x_axis_range_tm_i <- as.numeric(diff(range(range_x_week_tm_i)))
        x_tick_label_breaks_tm_i <- case_when(x_axis_range_tm_i <= 60 ~ 1,
                                              x_axis_range_tm_i > 60 & x_axis_range_tm_i <= 300 ~ 2,
                                              x_axis_range_tm_i > 300 & x_axis_range_tm_i <= 540 ~ 3,
                                              x_axis_range_tm_i > 540 ~ 4)
        
        if(nrow(aceitabilidade_implem_tbl_filtered()) == 0) {
          
          aceitabilidade_implem_tm_i_tbl_week <- data.frame(IMPLEMENTATION_WEEK = as.Date(filter_aceitabilidade$aceitabilidade_date),
                                                            IMPLEMENTATION_ADHERENCE = NA,
                                                            tempo_resposta_medio = 0)
          
        } else {
          aceitabilidade_implem_tm_i_tbl_week<- aceitabilidade_implem_tbl_filtered() %>%
            mutate(tempo_resposta = as.numeric(difftime(ALERT_IMPLEMENTATION_DATE, ALERT_ADHERENCE_DATE, units  = "hours"))) %>%
            group_by(IMPLEMENTATION_WEEK, IMPLEMENTATION_ADHERENCE) %>%
            reframe(tempo_resposta_medio = round(mean(tempo_resposta, na.rm = TRUE),2)) %>%
            ungroup() %>%
            mutate(IMPLEMENTATION_ADHERENCE = factor(IMPLEMENTATION_ADHERENCE,
                                                     levels = c(T,F),
                                                     labels = c("Aceitas","Rejeitadas"))) %>%
            tidyr::complete(IMPLEMENTATION_ADHERENCE,
                            IMPLEMENTATION_WEEK=seq(min(IMPLEMENTATION_WEEK), max(IMPLEMENTATION_WEEK), by="week"),
                            fill = list(tempo_resposta_medio = 0))
        }
        
        label_color_tbl_acpt_tm_i <- data.frame(label_name = c("Aceitas", "Rejeitadas"),
                                                color = c('#1f77b4','#AA1D30')) %>%
          filter(label_name %in% unique(aceitabilidade_implem_tm_i_tbl_week$IMPLEMENTATION_ADHERENCE))
        print("3116")
        aceitabilidade_implem_tm_i_tbl_week <- aceitabilidade_implem_tm_i_tbl_week %>%
          mutate(IMPLEMENTATION_WEEK_PT = format(IMPLEMENTATION_WEEK, "%d-%m-%y") %>%
                   tools::toTitleCase())
        
        # Create the plotly plot
        acpt_tm_i_plotly <- plot_ly(
          data = aceitabilidade_implem_tm_i_tbl_week,
          x = ~IMPLEMENTATION_WEEK,
          y = ~tempo_resposta_medio,
          type = "bar",
          color = ~IMPLEMENTATION_ADHERENCE,
          colors = setNames(label_color_tbl_acpt_tm_i$color, label_color_tbl_acpt_tm_i$label_name),
          hovertemplate = ~paste0("Semana: ", format(IMPLEMENTATION_WEEK_PT, format = "%d-%m-%y"), "<br>", IMPLEMENTATION_ADHERENCE,": ", tempo_resposta_medio," horas<extra></extra>")
        ) %>%
          layout(
            legend = list(x = 100, y = 0.5),
            barmode = 'stack',
            title = list(text = plot_title_acpt, x = 0.5), # Center the title
            xaxis = list(showline= T, linewidth=1, linecolor='black',
                         title = "",
                         tickangle = 45,
                         type = "date",
                         tickmode = "linear",  # Define espaçamento uniforme
                         dtick = x_tick_label_breaks_tm_i * 7 * 24 * 60 * 60 * 1000,  # semanas em ms
                         tickformat = "%d %b %Y",  # Formato dos rótulos
                         range = range_x_week_tm_i + c(-5,-1)
            ),
            yaxis = list(showline= T, linewidth=1, linecolor='black',
                         title = y_lab,
                         range = c(0, max_y_week_tm_i)
            ),
            margin = list(t = 50, r = 20, b = 120, l = 20), # Adjust plot margins
            legend = list(title = list(text = ""), orientation = "h")
          ) %>%
          config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      } # else semana
      
      acpt_tm_i_plotly
      
    })
  })
}