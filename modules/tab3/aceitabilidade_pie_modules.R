#UI:
aceitabilidade_pie_ui <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("aceitabilidade_sugest_pie"))
}

aceitabilidade_implemen_pie_ui <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("aceitabilidade_implemen_pie"))
}

# Servidor:
aceitabilidade_pie_server <- function(id, aceitabilidade_tbl_filtered) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$aceitabilidade_sugest_pie <- renderPlotly({
      
      # Caso não haja resultados nos filtros
      if (nrow(aceitabilidade_tbl_filtered()) == 0) {
        blank_aceitabilidade_count_tbl <- data.frame(n = 1)
        
        plot_ly(
          data = blank_aceitabilidade_count_tbl,
          type = "pie",
          hole = 0.7,
          values = ~n,
          textinfo = "none",
          textposition = "none", 
          hovertemplate = "<extra></extra>",
          marker = list(colors = "grey"),
          direction = "clockwise"
        ) %>%
          layout(
            showlegend = FALSE,
            annotations = list(
              list(
                text = "Sem resultado para<br>os filtros selecionados",
                showarrow = FALSE,
                font = list(size = 15, weight = "bold")
              )
            )
          )
      } else {
        # Processamento de dados
        aceitabilidade_count_tbl <- aceitabilidade_tbl_filtered() %>%
          count(ALERT_ADHERENCE) %>%
          mutate(p = n / sum(n)) %>%
          mutate(
            ALERT_ADHERENCE = factor(
              ALERT_ADHERENCE, 
              levels = c(TRUE, FALSE),
              labels = c("Aceitas", "Rejeitadas"),
              ordered = TRUE
            )
          )
        
        n_acceptance_value <- aceitabilidade_count_tbl %>%
          filter(ALERT_ADHERENCE == "Aceitas") %>%
          pull(p)
        
        if (length(n_acceptance_value) == 0) {
          n_acceptance_value <- 0
        }
        
        pie_color <- c("#AA1D30", "#1f77b4")
        
        if (n_acceptance_value == 1) {
          pie_color <- "#1f77b4"
        }
        
        plot_ly(
          data = aceitabilidade_count_tbl,
          type = "pie",
          hole = 0.7,
          rotation = 90,
          labels = ~ALERT_ADHERENCE,
          values = ~n,
          textinfo = "percent",
          textposition = "outside",
          hovertemplate = "%{label}: %{percent} </br><extra></extra>",
          marker = list(colors = pie_color),
          direction = "clockwise"
        ) %>%
          layout(
            showlegend = TRUE,
            annotations = list(
              list(
                text = paste0(round(n_acceptance_value * 100, 1), 
                              "%<br>das intervenções<br>aceitas"),
                showarrow = FALSE,
                font = list(size = 20, weight = "bold")
              )
            )
          )
      }
    })
  })
}

aceitabilidade_implemen_pie_server <- function(id, aceitabilidade_implem_tbl_filtered) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$aceitabilidade_implemen_pie <- renderPlotly({
      
      if(nrow(aceitabilidade_implem_tbl_filtered()) == 0) { 
        
        blank_aceitabilidade_count_tbl <- data.frame(n = 1)
        
        plot_ly(data = blank_aceitabilidade_count_tbl,
                type = "pie",
                hole = 0.7,
                # rotation = 90,
                values = ~n,
                textinfo = 'none',
                textposition = "none", 
                hovertemplate =  '<extra></extra> ',
                marker = list(colors = "grey"),
                direction = "clockwise"
        ) %>%
          layout(
            showlegend = F,
            annotations = list(
              list(
                text = "Sem resultado para<br>os filtros selecionados",
                showarrow = FALSE,
                font = list(size = 15, weight = "bold")
              )
            )
          )
        
      } else {
        
        aceitabilidade_count_tbl <- aceitabilidade_implem_tbl_filtered() %>%
          count(IMPLEMENTATION_ADHERENCE) %>%
          mutate(p = n/sum(n)) %>%
          mutate(IMPLEMENTATION_ADHERENCE = factor(IMPLEMENTATION_ADHERENCE, levels = c(T, F),
                                                   labels = c("Aceitas", "Rejeitadas"),
                                                   ordered = TRUE))
        
        n_acceptance_value <- aceitabilidade_count_tbl[aceitabilidade_count_tbl$IMPLEMENTATION_ADHERENCE == "Aceitas", "p"]
        
        if(nrow(n_acceptance_value)==0) {n_acceptance_value <- 0}
        
        p_aceite <- aceitabilidade_count_tbl %>% filter(IMPLEMENTATION_ADHERENCE == "Aceitas") %>% select(p)
        
        pie_color = c('#AA1D30','#1f77b4')
        
        if(nrow(p_aceite) > 0) {
          if(p_aceite$p == 1) {
            pie_color = '#1f77b4' }
        }
        
        plot_ly(data = aceitabilidade_count_tbl,
                type = "pie",
                hole = 0.7,
                rotation = 90,
                labels = ~IMPLEMENTATION_ADHERENCE,
                values = ~n,
                textinfo = "percent",  # Show both label and percent
                textposition = "outside",    # Place text outside the slices
                hovertemplate =  "%{label}: %{percent} </br><extra></extra>",
                marker = list(colors = pie_color),
                direction = "clockwise"
        ) %>%
          layout(
            showlegend = TRUE,
            annotations = list(
              list(
                text = paste(round(n_acceptance_value *100,1), "%<br>das implementações<br>aceitas"),
                showarrow = FALSE,
                font = list(size = 20, weight = "bold")
              )
            )
          )
      }
    })
  })
}