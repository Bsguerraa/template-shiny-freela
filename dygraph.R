# output$plot_acpt_count <- renderDygraph({
# # Diferença máxima de tempo na gestão de isolamento (para limite do eixo y)
# max_aceitabilidade_n <- aceitabilidade_tbl %>%
#   count(ALERT_MONTH) %>%
#   select(n) %>%
#   reframe(n = max(n, na.rm = T)*1.2)
# 
# # Preparar tabela em branco para plot de gestao de isolamento
# date_range_acpt_month <- as.Date(range(aceitabilidade_tbl$ALERT_MONTH))
# date_range_acpt_week <- as.Date(range(aceitabilidade_tbl$ALERT_WEEK))
# 
# date_seq_acpt_month <- seq(date_range_acpt_month[1], date_range_acpt_month[2], by = "month")
# date_seq_acpt_week <- seq(date_range_acpt_week[1], date_range_acpt_week[2], by = "week")
# 
# blank_dataset_acpt_month = data.frame(month = date_seq_acpt_month,
#                               total = rep(NA, length(date_seq_acpt_month)))
# 
# blank_dataset_acpt_week = data.frame(week = date_seq_acpt_week,
#                                      total = rep(NA, length(date_seq_acpt_week)))
# 
# first_row_acpt_tbl_month <- blank_dataset_acpt_month[1,]
# first_row_acpt_tbl_month[,"total"] <- NA
# 
# last_row_acpt_tbl_month <- first_row_acpt_tbl_month
# 
# first_row_acpt_tbl_week <- blank_dataset_acpt_week[1,]
# first_row_acpt_tbl_week[,"total"] <- NA
# 
# last_row_acpt_tbl_week <- first_row_acpt_tbl_week
# 
# first_row_acpt_tbl_month[,"month"] = min(blank_dataset_acpt_month$month, na.rm = T) - 5
# last_row_acpt_tbl_month[,"month"] = max(blank_dataset_acpt_month$month, na.rm = T) + 5
# 
# first_row_acpt_tbl_week[,"week"] = min(blank_dataset_acpt_week$week, na.rm = T) - 5
# last_row_acpt_tbl_week[,"week"] = max(blank_dataset_acpt_week$week, na.rm = T) + 5
# 
# expanded_blank_dataset_acpt_month <- rbind(first_row_acpt_tbl_month, blank_dataset_acpt_month, last_row_acpt_tbl_month)
# expanded_blank_dataset_acpt_week <- rbind(first_row_acpt_tbl_week, blank_dataset_acpt_week, last_row_acpt_tbl_week)
# 
# xts_dataset_acpt_month <- xts(expanded_blank_dataset_acpt_month %>% select(total),
#                       order.by = expanded_blank_dataset_acpt_month$month)
# 
# xts_dataset_acpt_week <- xts(expanded_blank_dataset_acpt_week %>% select(total),
#                               order.by = expanded_blank_dataset_acpt_week$week)
# 
# aceitabilidade_count_tbl_month <- aceitabilidade_tbl %>%
#   count(ALERT_MONTH, ALERT_ADHERENCE) %>%
#   mutate(ALERT_ADHERENCE = factor(ALERT_ADHERENCE,
#                                      levels = c(T,F),
#                                      labels = c("TRUE_","FALSE_"))) %>%
#   pivot_wider(names_from = ALERT_ADHERENCE, values_from = n, id_cols = ALERT_MONTH) %>%
#   mutate(total = TRUE_ + FALSE_) %>%
#   rename(month = ALERT_MONTH)
# 
# aceitabilidade_count_tbl_month <- xts(aceitabilidade_count_tbl_month %>% select(-month),
#       order.by = aceitabilidade_count_tbl_month$month)
# 
# aceitabilidade_count_tbl_week<- aceitabilidade_tbl %>%
#   count(ALERT_WEEK, ALERT_ADHERENCE) %>%
#   mutate(ALERT_ADHERENCE = factor(ALERT_ADHERENCE,
#                                   levels = c(T,F),
#                                   labels = c("TRUE_","FALSE_"))) %>%
#   pivot_wider(names_from = ALERT_ADHERENCE, values_from = n, id_cols = ALERT_WEEK) %>%
#   mutate(total = TRUE_ + FALSE_) %>%
#   rename(week = ALERT_WEEK)
# 
# aceitabilidade_count_tbl_week <- xts(aceitabilidade_count_tbl_week %>% select(-week),
#                                       order.by = aceitabilidade_count_tbl_week$week)
# 
#   if(is.null(aceitabilidade_count_tbl_month) | nrow(aceitabilidade_count_tbl_month) == 0) {
# 
#     ### Visualização 1
#     dygraph(xts_dataset_acpt_month,
#             main = "Contagem de intervenções") |>
#       dyRangeSelector() |> # seletor inferior de tempo
#       dyCrosshair(direction = 'vertical') |> # linhas verticais pra acompanhar o mouse
#       dyAxis('y', label = 'Número de intervenções', valueRange = c(0,max_aceitabilidade_n$n)) |> # label y
#       dyOptions(useDataTimezone = TRUE, colors = '#08A18E', rightGap = 10, # colocando estilo mês/dia/ano quando passa o mouse
#                 drawPoints = TRUE,
#                 pointSize = 3,
#                 fillGraph  = TRUE,
#                 animatedZooms = TRUE,
#                 includeZero = TRUE,
#                 digitsAfterDecimal = 1) |>
#       dyUnzoom() # dando opção de desfazer o zoom
#   } else {
# 
# 
#     xts_dataset_acpt_month_plot <- xts_dataset_acpt_month$total
#     xts_dataset_acpt_month_plot$A <- xts_dataset_acpt_month_plot$total
#     xts_dataset_acpt_month_plot$B <- xts_dataset_acpt_month_plot$total
# 
#     names(xts_dataset_acpt_month_plot) <- colnames(aceitabilidade_count_tbl_month)
# 
#     xts_dataset_acpt_month_plot[2:(nrow(xts_dataset_acpt_month_plot)-1),] <- 0
#     xts_dataset_acpt_month_plot[index(aceitabilidade_count_tbl_month),]<- aceitabilidade_count_tbl_month
# 
#     dy_position(
#       data_final = xts_dataset_acpt_month_plot[,c("TRUE_","FALSE_")],
#       plot_title = "Contagem de intervenções",
#       y1_label = "Número de intervenções",
#       y2_label = "",
#       bar_color = c('#AA1D30','#1f77b4')
#     ) |> dyRangeSelector() |> # seletor inferior de tempo
#         dyCrosshair(direction = 'vertical') |> # linhas verticais pra acompanhar o mouse
#         dyAxis('y', label = 'Número de intervenções', valueRange = c(0,max_aceitabilidade_n$n)) |> # label y
#         # dyAxis('x', label = "", drawGrid = FALSE, valueRange = range(aceitabilidade_count_tbl_month$month) + c(-3,3)) |> # retirando as linhas verticais do gráfico
#         dyOptions(useDataTimezone = TRUE,# colocando estilo mês/dia/ano quando passa o mouse
#                   drawPoints = FALSE,
#                   fillGraph  = TRUE,
#                   animatedZooms = TRUE,
#                   includeZero = TRUE,
#                   digitsAfterDecimal = 1) |>
#         dyUnzoom() # dando opção de desfazer o zoom
#     }
# 
# })
