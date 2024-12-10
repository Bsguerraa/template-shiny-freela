# UI:
plot_atb_cost_ui <- function(id) {
  ns <- NS(id)
    card(
      height = 450,
      max_height = 600,
      min_height = 350,
      full_screen = TRUE,
      card_header(
        "Custo de Antibióticos ao Longo do Tempo",
        style = "text-align: center;"
      ),
      layout_sidebar(
        fillable = TRUE,
        sidebar = sidebar(
          width = 120,
          open = "closed",
          radioGroupButtons(
            inputId = ns("plot_atb_cost_time_setting"), # Adiciona o namespace
            label = "Selecione:",
            choices = c("Mensal", "Semanal"),
            size = "sm",
            selected = "Mensal",
            direction = "vertical"
          )
        ),
        plotlyOutput(ns("plot_atb_cost")) # Adiciona o namespace
      )
    )
}

# SERVER:
plot_atb_cost_server <- function(id, filter_atb_cost, atb_cost_tbl, format_currency_br) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Convert input to reactive value for module
    filter_atb_cost_ts <- reactiveValues(
      plot_atb_cost_time_setting = "Mensal"
    )

    # Update reactive values
    observeEvent(input$plot_atb_cost_time_setting, {
      filter_atb_cost_ts$plot_atb_cost_time_setting <- input$plot_atb_cost_time_setting
    })
    
    output$plot_atb_cost <- renderPlotly({
      
      y_lab_atb_cost = "Custo Total (R$)"
      
     if(filter_atb_cost_ts$plot_atb_cost_time_setting == "Mensal") {

        # x axis range for months
        if(as.numeric(diff(as.Date(filter_atb_cost$atb_cost_date))) < 150) {
          range_x_atb_cost_month <- c(as.Date(filter_atb_cost$atb_cost_date)[1], as.Date(filter_atb_cost$atb_cost_date)[1]+150)
        } else {
          range_x_atb_cost_month <- as.Date(filter_atb_cost$atb_cost_date)
        }
        
        range_x_atb_cost_month[1] <- floor_date(range_x_atb_cost_month[1], unit = "month")
        range_x_atb_cost_month[2] <- ceiling_date(range_x_atb_cost_month[2], unit = "month")
        
        if(nrow(atb_cost_tbl()) == 0) {
          
          atb_cost_tbl_month <- data.frame(THERAPY_START_MONTH = as.Date(filter_atb_cost$atb_cost_date),
                                           TOTAL_COST = 0)
        } else {
          
          atb_cost_tbl_month <- atb_cost_tbl() %>%
            group_by(THERAPY_START_MONTH) %>%
            reframe(TOTAL_COST = sum(TOTAL_COST, na.rm = TRUE)) %>%
            ungroup() %>%
            tidyr::complete(THERAPY_START_MONTH=seq(min(THERAPY_START_MONTH), max(THERAPY_START_MONTH), by="month"),
                            fill = list(TOTAL_COST = 0))
        }
        
        atb_cost_tbl_month <- atb_cost_tbl_month %>%
          mutate(THERAPY_START_MONTH_PT = format(THERAPY_START_MONTH, "%b %Y") %>%
                   tools::toTitleCase())
        
        max_y_atb_cost_month <- ifelse(max(atb_cost_tbl_month$TOTAL_COST, na.rm = T) < 100, 
                                       100, 
                                       max(atb_cost_tbl_month$TOTAL_COST, na.rm = T) )
        
        # Create the plotly plot
        atb_cost_plotly <- plot_ly(
          data = atb_cost_tbl_month,
          x = ~THERAPY_START_MONTH,
          y = ~TOTAL_COST,
          type = 'bar',
          marker = list(color = '#08A18E'),
          hovertemplate = ~paste0("Custo total: ", format_currency_br(TOTAL_COST),"<extra></extra>")
        ) %>%
          layout(
            xaxis = list(showline= T, linewidth=1, linecolor='black',
                         title = "",
                         tickangle = 45,
                         type = "date",
                         tickmode = "linear",  # Define espaçamento uniforme
                         dtick = "M1",  # One month intervals
                         tickformat = "%b %Y",  # Full month name and year
                         range = range_x_atb_cost_month + c(-15,-10)
            ),
            yaxis = list(showline= T, linewidth=1, linecolor='black',
                         title = list(text = y_lab_atb_cost,
                                      standoff = 20),          # Increase the distance (in pixels)
                         tickformat = ",.2f",  # Format numbers as currency with 2 decimal places
                         range = c(0, max_y_atb_cost_month)
            ),
            margin = list(t = 50, r = 20, b = 20, l = 20), # Adjust plot margins
            legend = list(title = list(text = ""), orientation = "h")
          )  %>%
          config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
        
        ### ELSE
      } else {
        
        # Configurações para gráfico por semana
        
        # x axis range for weeks
        if(as.numeric(diff(as.Date(filter_atb_cost$atb_cost_date))) < 60) {
          range_x_atb_cost_week <- c(as.Date(filter_atb_cost$atb_cost_date)[1], as.Date(filter_atb_cost$atb_cost_date)[1]+60)
        } else {
          range_x_atb_cost_week <- as.Date(filter_atb_cost$atb_cost_date)
        }
        range_x_atb_cost_week <- range(atb_cost_tbl()$THERAPY_START_WEEK)
        range_x_atb_cost_week[1] <- floor_date(range_x_atb_cost_week[1], unit = "week")
        range_x_atb_cost_week[2] <- ceiling_date(range_x_atb_cost_week[2], unit = "week")
        
        atb_cost_x_axis_range <- as.numeric(diff(range(range_x_atb_cost_week)))
        ac_x_tick_label_breaks <- case_when(atb_cost_x_axis_range <= 60 ~ 1,
                                            atb_cost_x_axis_range > 60 & atb_cost_x_axis_range <= 300 ~ 2,
                                            atb_cost_x_axis_range > 300 & atb_cost_x_axis_range <= 540 ~ 3,
                                            atb_cost_x_axis_range > 540 ~ 4)
        
        if(nrow(atb_cost_tbl()) == 0) {
          
          atb_cost_tbl_week <- data.frame(THERAPY_START_WEEK = as.Date(filter_atb_cost$atb_cost_date),
                                          TOTAL_COST = 0)
        } else {
          atb_cost_tbl_week <- atb_cost_tbl() %>%
            group_by(THERAPY_START_WEEK) %>%
            reframe(TOTAL_COST = sum(TOTAL_COST, na.rm = TRUE)) %>%
            ungroup() %>%
            tidyr::complete(THERAPY_START_WEEK=seq(min(THERAPY_START_WEEK), max(THERAPY_START_WEEK), by="week"),
                            fill = list(TOTAL_COST = 0))
        }
        
        max_y_atb_cost_week <- ifelse(max(atb_cost_tbl_week$TOTAL_COST, na.rm = T) < 50, 
                                      50, 
                                      max(atb_cost_tbl_week$TOTAL_COST, na.rm = T) )
        
        atb_cost_tbl_week <- atb_cost_tbl_week %>%
          mutate(THERAPY_START_WEEK_PT = format(THERAPY_START_WEEK, "%d-%m-%y") %>%
                   tools::toTitleCase())
        
        # Create the plotly plot
        atb_cost_plotly <- plot_ly(
          data = atb_cost_tbl_week,
          x = ~THERAPY_START_WEEK,
          y = ~TOTAL_COST,
          type = "bar",
          marker = list(color = '#08A18E'),
          hovertemplate = ~paste0("Semana: ", format(THERAPY_START_WEEK, format = "%d-%m-%y"),
                                  "<br>Custo total: ", format_currency_br(TOTAL_COST),"<extra></extra>")
        ) %>%
          layout(
            xaxis = list(showline= T, linewidth=1, linecolor='black',
                         title = "",
                         tickangle = 45,
                         type = "date",
                         tickmode = "linear",  # Define espaçamento uniforme
                         dtick = ac_x_tick_label_breaks * 7 * 24 * 60 * 60 * 1000,  # semanas em ms
                         tickformat = "%d %b %Y",  # Formato dos rótulos
                         range = range_x_atb_cost_week + c(-5,-1)
            ),
            yaxis = list(showline= T, linewidth=1, linecolor='black',
                         title = list(text = y_lab_atb_cost,
                                      standoff = 20),          # Increase the distance (in pixels)
                         tickformat = ",.2f",  # Format numbers as currency with 2 decimal places
                         range = c(0, max_y_atb_cost_week)
            ),
            margin = list(t = 50, r = 20, b = 20, l = 20), # Adjust plot margins
            legend = list(title = list(text = ""), orientation = "h")
          ) %>%
          config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
        
      } # else semana
      
      atb_cost_plotly
      
    })
    
  })
}