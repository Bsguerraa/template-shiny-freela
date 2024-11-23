# PRD 1
# lower_date = data.frame(month = min(dataset$month) - 3,
#                          dif = 5)
# 
# dataset <- lower_date %>% rbind(dataset)

### Visualização 1
dygraph(dataset,
        main = "Tempo entre Coleta e Resultado (Eficiência BDmax)") |>
  dyRangeSelector() |> # seletor inferior de tempo
  dySeries("dif", label = "Tempo (horas)") %>%
  # dyBarChart() |> # tipo de gráfico
  dyCrosshair(direction = 'vertical') |> # linhas verticais pra acompanhar o mouse
  dyLimit(as.numeric(25), "24 horas", color = "red", labelLoc = "right") %>%
  dyShading(from = min(dataset$month), to = '2023-4-1') %>%
  dyEvent('2023-4-1', 'Solução Go-Live', labelLoc = c('bottom'), 
          strokePattern = 'solid', color = "grey") |> # indicador de evento
  dyAxis('y', label = 'Tempo (horas)') |> # label y
  dyAxis('x', label = "", drawGrid = FALSE, valueRange = range(dataset$month) + c(-3,3)) |> # retirando as linhas verticais do gráfico
  dyOptions(useDataTimezone = TRUE, colors = '#08A18E', rightGap = 10, # colocando estilo mês/dia/ano quando passa o mouse
            drawPoints = TRUE, 
            pointSize = 3,
            fillGraph  = TRUE,
            includeZero = TRUE) |> 
  dyUnzoom() # dando opção de desfazer o zoom

# Visualização 2
dygraph(dataset,
        main = "Tempo entre Coleta e Resultado (Eficiência BDmax)") |>
  dyRangeSelector() |> # seletor inferior de tempo
  dySeries("dif", label = "Tempo (horas)") %>%
  dyBarChart() |> # tipo de gráfico
  #  dySeries('value', label = 'Total de Solicitações') |> # label do mouse
  dyCrosshair(direction = 'vertical') |> # linhas verticais pra acompanhar o mouse
  dyLimit(as.numeric(25), "24 horas", color = "red", labelLoc = "right") %>%
  dyEvent('2023-4-1', 'Solução Go-Live', labelLoc = c('bottom')) |> # indicador de evento
  dyAxis('y', label = 'Tempo (horas)') |> # label y
  dyAxis('x', label = "", drawGrid = FALSE, valueRange = range(dataset$month) + c(-3,3)) |> # retirando as linhas verticais do gráfico
  dyOptions(useDataTimezone = TRUE, colors = '#08A18E', rightGap = 10, # colocando estilo mês/dia/ano quando passa o mouse
            drawPoints = TRUE, pointSize = 2,
            fillGraph  = TRUE) |> 
  dyUnzoom() # dando opção de desfazer o zoom

# Visualização 3

dataset


p3 <- dataset %>%
  group_by(month) %>% 
  summarise(dif = mean(dif)) %>% 
  ggplot(aes(x = month, y = dif)) +
  geom_col(fill = '#08A18E') +
  geom_hline(yintercept = 24, linetype = "dashed", color = "red") +
  xlab("") +
  ylab("Tempo (horas)") +
  theme_minimal() +
  labs(title = "Tempo entre Coleta e Resultado (Eficiência BDmax)") +
  scale_x_date(breaks = "month", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplotly(p3)

###
p4 <- dataset %>%
  group_by(month) %>% 
  summarise(dif = mean(dif)) %>% 
  ggplot(aes(x = month, y = dif)) +
  geom_line(color = '#08A18E') +
  geom_point(color = '#08A18E') +
  geom_hline(yintercept = 24, linetype = "dashed", color = "red") +
  xlab("") +
  ylab("Tempo (horas)") +
  theme_minimal() +
  labs(title = "Tempo entre Coleta e Resultado (Eficiência BDmax)") +
  scale_x_date(breaks = "month", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.line = element_line(),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches"))

ggplotly(p4)


#PRD 2
DDD_tbl = antimicrob_tbl_final %>%
  group_by(MEDICATION_NAME, MEDICATION_CLASS_CODE) %>%
  reframe(A_ratio_B = sum(DD)/unique(DDD_OMS),
          P = sum(n_distinct(PATIENT_ID))) %>%
  ungroup() %>%
  mutate(DDD = A_ratio_B/P*1000) %>%
  
  # Filtrar para deixar nomes de medicamentos únicos (verificar como será o tratamento com os dados reais)
  group_by(MEDICATION_NAME) %>%
  mutate(n_med_name = 1:n()) %>%
  ungroup() %>%
  filter(n_med_name == 1)

plot_ddd <- DDD_tbl %>%
  arrange(desc(DDD)) %>%
  ggplot(aes(y = fct_reorder(MEDICATION_NAME, DDD), x = DDD,
             labels = "",
             text = paste0(MEDICATION_NAME,
                           '<br>DDD: ', round(DDD,2)))) +
  geom_col(fill = "#1F77B4") +
  theme_minimal() +
  ylab("") +
  xlab("DDD (Dose Diária Definida) (g/1000 pacientes-dia)") +
  theme(axis.line.y = element_line()) +
  scale_x_continuous(expand = c(0, 0))

ggplotly(plot_ddd, tooltip = "text")
