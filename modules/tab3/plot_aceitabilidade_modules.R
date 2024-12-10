# UI:
plot_aceitabilidade_ui <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("plot_aceitabilidade_n"))
}

plot_aceitabilidade_implemen_ui <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("plot_aceitabilidade_implemen_n"))
}

# Servidor:
plot_aceitabilidade_server <- function(id, aceitabilidade_tbl_filtered, aceitabilidade_tbl_filtered_max_month, 
                                       aceitabilidade_tbl_filtered_max_week, filter_aceitabilidade, filter_acpt_n) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    create_monthly_plot <- function() {
      max_y_month <- ifelse(aceitabilidade_tbl_filtered_max_month()$max_value < 10, 10, aceitabilidade_tbl_filtered_max_month()$max_value)
      
      range_x_month <- if (as.numeric(diff(as.Date(filter_aceitabilidade$aceitabilidade_date))) < 150) {
        c(as.Date(filter_aceitabilidade$aceitabilidade_date)[1], as.Date(filter_aceitabilidade$aceitabilidade_date)[1] + 150)
      } else {
        as.Date(filter_aceitabilidade$aceitabilidade_date)
      }
      range_x_month[1] <- floor_date(range_x_month[1], unit = "month")
      range_x_month[2] <- ceiling_date(range_x_month[2], unit = "month")
      
      aceitabilidade_count_tbl_month <- if (nrow(aceitabilidade_tbl_filtered()) == 0) {
        data.frame(ALERT_MONTH = as.Date(filter_aceitabilidade$aceitabilidade_date), ALERT_ADHERENCE = NA, n = 0)
      } else {
        aceitabilidade_tbl_filtered() %>%
          count(ALERT_MONTH, ALERT_ADHERENCE) %>%
          mutate(ALERT_ADHERENCE = factor(ALERT_ADHERENCE, levels = c(TRUE, FALSE), labels = c("Aceitas", "Rejeitadas"))) %>%
          tidyr::complete(ALERT_ADHERENCE, ALERT_MONTH = seq(min(ALERT_MONTH), max(ALERT_MONTH), by = "month"), fill = list(n = 0))
      }
      
      label_color_tbl_acpt <- data.frame(label_name = c("Aceitas", "Rejeitadas"), color = c("#1f77b4", "#AA1D30")) %>%
        filter(label_name %in% unique(aceitabilidade_count_tbl_month$ALERT_ADHERENCE))
      
      plot_ly(
        data = aceitabilidade_count_tbl_month,
        x = ~ALERT_MONTH,
        y = ~n,
        type = "bar",
        color = ~ALERT_ADHERENCE,
        colors = setNames(label_color_tbl_acpt$color, label_color_tbl_acpt$label_name),
        hovertemplate = ~paste0(str_to_title(format(ALERT_MONTH, format = "%B %Y")), "<br>", ALERT_ADHERENCE, ": ", n, "<extra></extra>")
      ) %>%
        layout(
          legend = list(x = 100, y = 0.5),
          barmode = "stack",
          title = list(text = "Contagem de Adesão às Intervenções Sugeridas", x = 0.5),
          xaxis = list(
            showline = TRUE, linewidth = 1, linecolor = "black", title = "", tickangle = 45,
            type = "date", tickmode = "linear", dtick = "M1", tickformat = "%b %Y", range = range_x_month + c(-15, -1)
          ),
          yaxis = list(showline = TRUE, linewidth = 1, linecolor = "black", title = "Sugestões enviadas", range = c(0, max_y_month)),
          margin = list(t = 50, r = 20, b = 120, l = 20),
          legend = list(title = list(text = ""), orientation = "h")
        ) %>%
        config(locale = "pt-BR")
    }
    
    create_weekly_plot <- function() {
      max_y_week <- ifelse(aceitabilidade_tbl_filtered_max_week()$max_value < 10, 10, aceitabilidade_tbl_filtered_max_week()$max_value)
      
      range_x_week <- if (as.numeric(diff(as.Date(filter_aceitabilidade$aceitabilidade_date))) < 60) {
        c(as.Date(filter_aceitabilidade$aceitabilidade_date)[1], as.Date(filter_aceitabilidade$aceitabilidade_date)[1] + 60)
      } else {
        as.Date(filter_aceitabilidade$aceitabilidade_date)
      }
      range_x_week[1] <- floor_date(range_x_week[1], unit = "week")
      range_x_week[2] <- ceiling_date(range_x_week[2], unit = "week")
      
      aceitabilidade_count_tbl_week <- if (nrow(aceitabilidade_tbl_filtered()) == 0) {
        data.frame(ALERT_WEEK = as.Date(filter_aceitabilidade$aceitabilidade_date), ALERT_ADHERENCE = NA, n = 0)
      } else {
        aceitabilidade_tbl_filtered() %>%
          count(ALERT_WEEK, ALERT_ADHERENCE) %>%
          mutate(ALERT_ADHERENCE = factor(ALERT_ADHERENCE, levels = c(TRUE, FALSE), labels = c("Aceitas", "Rejeitadas"))) %>%
          tidyr::complete(ALERT_ADHERENCE, ALERT_WEEK = seq(min(ALERT_WEEK), max(ALERT_WEEK), by = "week"), fill = list(n = 0))
      }
      
      label_color_tbl_acpt <- data.frame(label_name = c("Aceitas", "Rejeitadas"), color = c("#1f77b4", "#AA1D30")) %>%
        filter(label_name %in% unique(aceitabilidade_count_tbl_week$ALERT_ADHERENCE))
      
      plot_ly(
        data = aceitabilidade_count_tbl_week,
        x = ~ALERT_WEEK,
        y = ~n,
        type = "bar",
        color = ~ALERT_ADHERENCE,
        colors = setNames(label_color_tbl_acpt$color, label_color_tbl_acpt$label_name),
        hovertemplate = ~paste0("Semana: ", format(ALERT_WEEK, format = "%d-%m-%y"), "<br>", ALERT_ADHERENCE, ": ", n, "<extra></extra>")
      ) %>%
        layout(
          legend = list(x = 100, y = 0.5),
          barmode = "stack",
          title = list(text = "Contagem de Adesão às Intervenções Sugeridas", x = 0.5),
          xaxis = list(
            showline = TRUE, linewidth = 1, linecolor = "black", title = "", tickangle = 45,
            type = "date", tickmode = "linear", dtick = 7 * 24 * 60 * 60 * 1000, tickformat = "%d %b %Y", range = range_x_week + c(-5, -1)
          ),
          yaxis = list(showline = TRUE, linewidth = 1, linecolor = "black", title = "Sugestões enviadas", range = c(0, max_y_week)),
          margin = list(t = 50, r = 20, b = 120, l = 20),
          legend = list(title = list(text = ""), orientation = "h")
        ) %>%
        config(locale = "pt-BR")
    }
    
    output$plot_aceitabilidade_n <- renderPlotly({
      if (filter_acpt_n$plot_acpt_time_setting == "Mensal") {
        create_monthly_plot()
      } else {
        create_weekly_plot()
      }
    })
  })
}

plot_aceitabilidade_implemen_server <- function(id, aceitabilidade_implem_tbl_filtered, aceitabilidade_implem_tbl_filtered_max_month, 
                                                aceitabilidade_implem_tbl_filtered_max_week, filter_aceitabilidade, filter_acpt_n) {
  moduleServer(id, function(input, output, session) {
    
    plot_title_acpt_i = "Contagem de Implementação das Intervenções Sugeridas"
    y_lab_i = "Sugestões aceitas"
    
    create_monthly_plot_i <- function() {
      max_y_month_i <- ifelse(aceitabilidade_implem_tbl_filtered_max_month()$max_value < 10, 10, aceitabilidade_implem_tbl_filtered_max_month()$max_value)
      
      # x axis range for months
      if(as.numeric(diff(as.Date(filter_aceitabilidade$aceitabilidade_date))) < 150) {
        range_x_month_i <- c(as.Date(filter_aceitabilidade$aceitabilidade_date)[1], as.Date(filter_aceitabilidade$aceitabilidade_date)[1]+150)
      } else {
        range_x_month_i <- as.Date(filter_aceitabilidade$aceitabilidade_date)
      }
      
      range_x_month_i[1] <- floor_date(range_x_month_i[1], unit = "month")
      range_x_month_i[2] <- ceiling_date(range_x_month_i[2], unit = "month")
      
      if(nrow(aceitabilidade_implem_tbl_filtered()) == 0) {
        
        aceitabilidade_implem_count_tbl_month <- data.frame(IMPLEMENTATION_MONTH = as.Date(filter_aceitabilidade$aceitabilidade_date),
                                                            IMPLEMENTATION_ADHERENCE = NA,
                                                            n = 0)
      } else {
        aceitabilidade_implem_count_tbl_month <- aceitabilidade_implem_tbl_filtered() %>%
          count(IMPLEMENTATION_MONTH, IMPLEMENTATION_ADHERENCE) %>%
          mutate(IMPLEMENTATION_ADHERENCE = factor(IMPLEMENTATION_ADHERENCE,
                                                   levels = c(T,F),
                                                   labels = c("Aceitas","Rejeitadas"))) %>%
          tidyr::complete(IMPLEMENTATION_ADHERENCE,
                          IMPLEMENTATION_MONTH=seq(min(IMPLEMENTATION_MONTH), max(IMPLEMENTATION_MONTH), by="month"),
                          fill = list(n = 0))
      }
      
      label_color_tbl_acpt_i <- data.frame(label_name = c("Aceitas", "Rejeitadas"),
                                           color = c('#1f77b4','#AA1D30')) %>%
        filter(label_name %in% unique(aceitabilidade_implem_count_tbl_month$IMPLEMENTATION_ADHERENCE))
      
      aceitabilidade_implem_count_tbl_month <- aceitabilidade_implem_count_tbl_month %>%
        mutate(IMPLEMENTATION_MONTH_PT = format(IMPLEMENTATION_MONTH, "%B %Y") %>%
                 tools::toTitleCase())
      
      # Create the plotly plot
      acpt_plotly_i <- plot_ly(
        data = aceitabilidade_implem_count_tbl_month,
        x = ~IMPLEMENTATION_MONTH,
        y = ~n,
        type = "bar",
        color = ~IMPLEMENTATION_ADHERENCE,
        colors = setNames(label_color_tbl_acpt_i$color, label_color_tbl_acpt_i$label_name),
        hovertemplate =  ~paste0(str_to_title(format(IMPLEMENTATION_MONTH, format = "%B %Y")), "<br>", IMPLEMENTATION_ADHERENCE,": ", n,"<extra></extra>")
      ) %>%
        layout(
          legend = list(x = 100, y = 0.5),
          barmode = 'stack',
          title = list(text = plot_title_acpt_i, x = 0.5), # Center the title
          xaxis = list(showline= T, linewidth=1, linecolor='black',
                       title = "",
                       tickangle = 45,
                       type = "date",
                       tickmode = "linear",  # Define espaçamento uniforme
                       dtick = "M1",  # One month intervals
                       tickformat = "%b %Y",  # Full month name and year
                       range = range_x_month_i + c(-15,-1)
          ),
          yaxis = list(showline= T, linewidth=1, linecolor='black',
                       title = y_lab_i,
                       range = c(0, max_y_month_i)
          ),
          margin = list(t = 50, r = 20, b = 120, l = 20), # Adjust plot margins
          legend = list(title = list(text = ""), orientation = "h")
        )  %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
    }
    create_weekly_plot_i <- function() {
      max_y_week <- ifelse(aceitabilidade_implem_tbl_filtered_max_week()$max_value < 10, 10,aceitabilidade_implem_tbl_filtered_max_week()$max_value)
      
      # x axis range for weeks
      if(as.numeric(diff(as.Date(filter_aceitabilidade$aceitabilidade_date))) < 60) {
        range_x_week <- c(as.Date(filter_aceitabilidade$aceitabilidade_date)[1], as.Date(filter_aceitabilidade$aceitabilidade_date)[1]+60)
      } else {
        range_x_week <- as.Date(filter_aceitabilidade$aceitabilidade_date)
      }
      
      range_x_week[1] <- floor_date(range_x_week[1], unit = "week")
      range_x_week[2] <- ceiling_date(range_x_week[2], unit = "week")
      
      x_axis_range_i <- as.numeric(diff(range(range_x_week)))
      x_tick_label_breaks_i <- case_when(x_axis_range_i <= 60 ~ 1,
                                         x_axis_range_i > 60 & x_axis_range_i <= 300 ~ 2,
                                         x_axis_range_i > 300 & x_axis_range_i <= 540 ~ 3,
                                         x_axis_range_i > 540 ~ 4)
      
      if(nrow(aceitabilidade_implem_tbl_filtered()) == 0) {
        
        aceitabilidade_implem_count_tbl_week <- data.frame(ALERT_WEEK = as.Date(filter_aceitabilidade$aceitabilidade_date),
                                                           IMPLEMENTATION_ADHERENCE = NA,
                                                           n = 0)
      } else {
        aceitabilidade_implem_count_tbl_week<- aceitabilidade_implem_tbl_filtered() %>%
          count(ALERT_WEEK, IMPLEMENTATION_ADHERENCE) %>%
          mutate(IMPLEMENTATION_ADHERENCE = factor(IMPLEMENTATION_ADHERENCE,
                                                   levels = c(T,F),
                                                   labels = c("Aceitas","Rejeitadas"))) %>%
          tidyr::complete(IMPLEMENTATION_ADHERENCE,
                          ALERT_WEEK=seq(min(ALERT_WEEK), max(ALERT_WEEK), by="week"),
                          fill = list(n = 0))
      }
      
      label_color_tbl_acpt_i <- data.frame(label_name = c("Aceitas", "Rejeitadas"),
                                           color = c('#1f77b4','#AA1D30')) %>%
        filter(label_name %in% unique(aceitabilidade_implem_count_tbl_week$IMPLEMENTATION_ADHERENCE))
      
      aceitabilidade_implem_count_tbl_week <- aceitabilidade_implem_count_tbl_week %>%
        mutate(ALERT_WEEK_PT = format(ALERT_WEEK, "%d-%m-%y") %>%
                 tools::toTitleCase())
      
      # Create the plotly plot
      acpt_plotly_i <- plot_ly(
        data = aceitabilidade_implem_count_tbl_week,
        x = ~ALERT_WEEK,
        y = ~n,
        type = "bar",
        color = ~IMPLEMENTATION_ADHERENCE,
        colors = setNames(label_color_tbl_acpt_i$color, label_color_tbl_acpt_i$label_name),
        hovertemplate =  ~paste0("Semana: ", format(ALERT_WEEK_PT, format = "%d-%m-%y"), "<br>", IMPLEMENTATION_ADHERENCE,": ", n,"<extra></extra>")
      ) %>%
        layout(
          legend = list(x = 100, y = 0.5),
          barmode = 'stack',
          title = list(text = plot_title_acpt_i, x = 0.5), # Center the title
          xaxis = list(showline= T, linewidth=1, linecolor='black',
                       title = "",
                       tickangle = 45,
                       type = "date",
                       tickmode = "linear",  # Define espaçamento uniforme
                       dtick = x_tick_label_breaks_i * 7 * 24 * 60 * 60 * 1000,  # semanas em ms
                       tickformat = "%d %b %Y",  # Formato dos rótulos
                       range = range_x_week + c(-5,-1)
          ),
          yaxis = list(showline= T, linewidth=1, linecolor='black',
                       title = y_lab_i,
                       range = c(0, max_y_week)
          ),
          margin = list(t = 50, r = 20, b = 120, l = 20), # Adjust plot margins
          legend = list(title = list(text = ""), orientation = "h")
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
    }
    
    output$plot_aceitabilidade_implemen_n <- renderPlotly({
      if (filter_acpt_n$plot_acpt_time_setting == "Mensal") {
        create_monthly_plot_i()
      } else {
        create_weekly_plot_i()
      }
    })
  })
}