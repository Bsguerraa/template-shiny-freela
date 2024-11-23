# output$plot_aceitabilidade_n <- renderPlotly({
# 
#   max_aceitabilidade_n_month <- aceitabilidade_tbl %>%
#     count(ALERT_MONTH) %>%
#     select(n) %>%
#     reframe(n = max(n, na.rm = T)*1.2)
# 
#   max_aceitabilidade_n_week <- aceitabilidade_tbl %>%
#     count(ALERT_WEEK) %>%
#     select(n) %>%
#     reframe(n = max(n, na.rm = T)*1.2)
# 
#   aceitabilidade_count_tbl_week<- aceitabilidade_tbl %>%
#     count(ALERT_WEEK, ALERT_ADHERENCE) %>%
#     mutate(ALERT_ADHERENCE = factor(ALERT_ADHERENCE,
#                                     levels = c(T,F),
#                                     labels = c("TRUE_","FALSE_"))) %>%
#     pivot_wider(names_from = ALERT_ADHERENCE, values_from = n, id_cols = ALERT_WEEK) %>%
#     mutate(total = TRUE_ + FALSE_) %>%
#     rename(week = ALERT_WEEK)%>%
#     pivot_longer(names_to = "resposta", values_to = "n", -week)
# 
#   aceitabilidade_count_tbl_month <- aceitabilidade_tbl %>%
#     count(ALERT_MONTH, ALERT_ADHERENCE) %>%
#     mutate(ALERT_ADHERENCE = factor(ALERT_ADHERENCE,
#                                     levels = c(T,F),
#                                     labels = c("TRUE_","FALSE_"))) %>%
#     pivot_wider(names_from = ALERT_ADHERENCE, values_from = n, id_cols = ALERT_MONTH) %>%
#     mutate(total = TRUE_ + FALSE_) %>%
#     rename(month = ALERT_MONTH) %>%
#     pivot_longer(names_to = "resposta", values_to = "n", -month)
# 
#   ### AFTER HERE
#   y_lab = "Sugestões enviadas"
#   plot_title_acpt = "Contagem de Adesão às Intervenções Sugeridas"
# 
#   total_count <- aceitabilidade_count_tbl_week %>%
#     filter(resposta == "total")
# 
#   date_range_week <- range(aceitabilidade_count_tbl_week$week)
# 
#   label_color_tbl_acpt <- data.frame(label_ = c("Aceita", "Rejeitada"),
#              color = c('#1f77b4','#AA1D30'),
#              resposta = c("TRUE_","FALSE_")) %>%
#     filter(resposta %in% unique(aceitabilidade_count_tbl_week$resposta))
# 
#   # Filter the data
#   data_filtered <- aceitabilidade_count_tbl_week %>%
#     filter(resposta != "total") %>%
#     mutate(resposta = factor(resposta, levels = c("TRUE_","FALSE_"),
#                              labels = c("Aceita","Rejeitada")))
# 
#   # Create the plotly plot
#   acpt_plotly <- plot_ly(
#     data = data_filtered,
#     x = ~week,
#     y = ~n,
#     type = "bar",
#     color = ~resposta,
#     colors = setNames(label_color_tbl_acpt$color, label_color_tbl_acpt$label_),
#     text = ~paste0("Semana: ", format(week, format = "%d-%m-%y"), "<br>", resposta,": ", n),
#     hoverinfo = "text",
#     name =
#   ) %>%
#     layout(
#       legend = list(x = 100, y = 0.5),
#       barmode = 'stack',
#       title = list(text = plot_title_acpt, x = 0.5), # Center the title
#       xaxis = list(
#         title = "",
#         tickangle = 45,
#         type = "date",
#         tickmode = "linear",  # Define espaçamento uniforme
#         dtick = 4 * 7 * 24 * 60 * 60 * 1000,  # 4 semanas em ms
#         tickformat = "%d %b %Y",  # Formato dos rótulos
#         range = as.Date(aceitabilidade_date_range) + c(-7,7)
#       ),
#       yaxis = list(
#         title = y_lab,
#         range = c(0, max_aceitabilidade_n_week$n)
#       ),
#       margin = list(t = 50, r = 20, b = 120, l = 20), # Adjust plot margins
#       legend = list(title = list(text = ""), orientation = "h")
#     )
# 
#   acpt_plotly
# 
# 
# })