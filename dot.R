output$dot_plot <- renderPlotly({
  
  # Opção A: Filtrar medicamentos
  
  if(input$dot_plot_button == "Ver somente itens selecionados") {
    
    if (is.null(filter_mic$medicamento_mic)) {
      antimicrob_tbl_final3 <- DOT_tbl()
    } else {
      antimicrob_tbl_final3 <- DOT_tbl() %>%
        dplyr::filter(MEDICATION_NAME %in% filter_mic$medicamento_mic)
    }
    
    # Calcular DOT por medicamento
    
    # Calculo Dias totais de terapia por hospitalização
    
    asd = antimicrob_tbl_final3 %>% count(ADMISSION_ID) %>% filter(n > 1)
    
    antimicrob_tbl_final4 <- antimicrob_tbl_final3 %>%
      group_by(ADMISSION_ID) %>%
      mutate(dias_uso_atm_total = sum(as.numeric(ADM_END_DATE - ADM_START_DATE))) %>%
      ungroup() %>%
      mutate(dias_uso_atm = as.numeric(ADM_END_DATE - ADM_START_DATE) ) %>%
      group_by(MEDICATION_NAME, MEDICATION_CLASS_CODE) %>%
      reframe(dias_uso_atm_sum = sum(dias_uso_atm),
              P = sum(dias_uso_atm_total)) %>%
      ungroup() %>%
      mutate(DOT = 1000*dias_uso_atm_sum/P) %>%
      # Filtrar para deixar nomes de medicamentos únicos (verificar como será o tratamento com os dados reais)
      group_by(MEDICATION_NAME) %>%
      mutate(n_med_name = 1:n()) %>%
      ungroup() %>%
      filter(n_med_name == 1)  
    
    plot_DOT <- antimicrob_tbl_final4 %>%
      arrange(desc(DOT)) %>%
      ggplot(aes(y = fct_reorder(MEDICATION_NAME, DOT), x = DOT,
                 labels = "",
                 text = paste0(MEDICATION_NAME,
                               '<br>DOT: ', round(DOT,2)))) +
      geom_col(fill = "#1F77B4") +
      theme_minimal() +
      ylab("") +
      xlab("dias/1000 pacientes-dia") +
      labs(title = "DOT (Days of Therapy)") +
      theme(axis.line.y = element_line()) +
      scale_x_continuous(expand = c(0, 0))
    
    ggplotly(plot_DOT, tooltip = "text")
  } else { 
    #if(input$dot_plot_button == "Destacar itens")
    # Opção B: Highlight nos medicamentos selecionados
    
    # Calcular DOT por medicamento
    antimicrob_tbl_final3b <- DOT_tbl() %>%
      group_by(ADMISSION_ID) %>%
      mutate(dias_uso_atm_total = sum(as.numeric(ADM_END_DATE - ADM_START_DATE))) %>%
      ungroup() %>%
      mutate(dias_uso_atm = as.numeric(ADM_END_DATE - ADM_START_DATE) ) %>%
      
      group_by(MEDICATION_NAME, MEDICATION_CLASS_CODE) %>%
      reframe(dias_uso_atm_sum = sum(dias_uso_atm),
              P = sum(dias_uso_atm_total)) %>%
      ungroup() %>%
      mutate(DOT = 1000*dias_uso_atm_sum/P) %>%
      # Filtrar para deixar nomes de medicamentos únicos (verificar como será o tratamento com os dados reais)
      group_by(MEDICATION_NAME) %>%
      mutate(n_med_name = 1:n()) %>%
      ungroup() %>%
      filter(n_med_name == 1) %>%
      mutate(highlight_medication = ifelse(MEDICATION_NAME %in% filter_mic$medicamento_mic, T, F))
    
    plot_DOT2 <- antimicrob_tbl_final3b %>%
      arrange(desc(DOT)) %>%
      ggplot(aes(y = fct_reorder(MEDICATION_NAME, DOT), x = DOT,
                 fill = highlight_medication,
                 labels = "",
                 text = paste0(MEDICATION_NAME,
                               '<br>DOT: ', round(DOT,2)))) +
      geom_col() +
      scale_fill_manual(values = c("#1F77B4", "orange")) +
      theme_minimal() +
      ylab("") +
      xlab("dias/1000 pacientes-dia") +
      labs(title = "DOT (Days of Therapy)") +
      theme(axis.line.y = element_line(),
            legend.position = "none") +
      scale_x_continuous(expand = c(0, 0))
    
    ggplotly(plot_DOT2, tooltip = "text")
  }
})