library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(readxl)
library(lubridate)
library(stringr)
library(bit64)
library(data.table)  
library(shinyWidgets)
library(DT)
library(stringi)
library(shinyalert)
library(openssl)
library(jose)
library(tidyr)
library(RColorBrewer)
library(xts)
library(tidyverse)
library(dygraphs)

source("R/utils.R")
source("R/query_data.R")
Sys.setlocale('LC_ALL', 'pt_BR.UTF-8')

# LENDO A PUB_KEY - Deixe essa parte em branco
# PUBKEY <- "-----BEGIN PUBLIC KEY-----
# -----END PUBLIC KEY-----"
# #pub_key <- read_pubkey(PUBKEY)

# Definindo as datas coleta
#default_start_date <- min(df$data_de_coleta, na.rm = TRUE)  
#default_last_date <- max(df$data_de_coleta, na.rm = TRUE) 

# Definindo a UI
ui <- shiny::conditionalPanel(
  condition = "output.valid_token",
  
  page_navbar(
    title = "",
    id = "nav",
    fillable = FALSE,
    tags$style(
      HTML("
      .navbar {
        border-style: none;
        border-width: 0;
      }
    ")),
    
    sidebar = sidebar(
      width = 300,
      
      conditionalPanel(
        "input.nav === 'Gestão de Isolamento'",
        ### COLOQUE AQUI OS FILTROS DA PAGINA 1 ## asd
         fluidRow(
           
           selectInput(
             inputId = "gestao_isolamento_tipo_data",
             label = "Escolha a data para filtro e visualização",
             choices = c("Entrada do paciente" = "entrada",
                         "Coleta do exame" = "coleta",
                         "Resultado do exame" = "resultado",
                         "Transferência ou liberação" = "release"),
             selected = "coleta",
             multiple = FALSE,
             selectize = FALSE
           ),
           
           column(1),
           dateRangeInput('gestao_isolamento_data',
                          label = 'Data',
                          start = as.Date(gestao_isolamento_range[1]), end = as.Date(gestao_isolamento_range[2]),
                          min = gestao_isolamento_range[1], max = gestao_isolamento_range[2],
                          separator = " até ", format = "dd-mm-yy",
                          startview = 'month', language = 'pt-BR'
           ),
           column(1),
           
           pickerInput(
             inputId = "isolamento_setor",
             label = "Setor Hospitalar",
             choices = gestao_isolamento_setores_list,
             multiple = TRUE,
             selected = NULL,
             options = pickerOptions(container = "body",
                                     title = "SELECIONE",
                                     actionsBox = TRUE,
                                     liveSearch = TRUE,
                                     selectAllText = "SELECIONAR TODOS",
                                     deselectAllText = "LIMPAR TODOS",
                                     noneSelectedText= "SELECIONE",
                                     noneResultsText = "SEM CORRESPONDÊNCIAS",
                                     selectedTextFormat = "count > 3",
                                     countSelectedText = "{0} ITENS SELECIONADOS")
           ),

           pickerInput(
             inputId = "isolamento_medico_id",
             label = "Médico Prescritor",
             choices = sort(unique(gestao_isolamento_tbl$PRACTITIONER_ID)),
             multiple = TRUE,
             selected = NULL,
             options = pickerOptions(container = "body",
                                     title = "SELECIONE",
                                     actionsBox = TRUE,
                                     liveSearch = TRUE,
                                     selectAllText = "SELECIONAR TODOS",
                                     deselectAllText = "LIMPAR TODOS",
                                     noneSelectedText= "SELECIONE",
                                     noneResultsText = "SEM CORRESPONDÊNCIAS",
                                     selectedTextFormat = "count > 3",
                                     countSelectedText = "{0} ITENS SELECIONADOS")
           ),
           
           column(5, actionButton("reset_btn_gi", "Reset", width = '125%')),
           column(1),
           column(5, actionButton("filter_btn_gi", "Filtrar", width = '125%'))
         )
      ),

      conditionalPanel(
        "input.nav === 'Consumo de antimicrobianos'",
        ### COLOQUE AQUI OS FILTROS DA PAGINA 2 ##
        fluidRow(
          
          dateRangeInput('internacao_data',
                         label = 'Data de entrada do paciente',
                         start = as.Date(antimicrob_date_range[1]), end = as.Date(antimicrob_date_range[2]),
                         min = antimicrob_date_range[1], max = antimicrob_date_range[2],
                         separator = " até ", format = "dd-mm-yy",
                         startview = 'month', language = 'pt-BR'
          ),
          column(1),
          
          pickerInput(
            inputId = "antimicrobiano_setor",
            label = "Setor Hospitalar",
            choices = sort(unique(antimicrob_tbl_final$DESCRIPTION)),
            multiple = TRUE,
            selected = NULL,
            options = pickerOptions(container = "body",
                                    title = "SELECIONE",
                                    actionsBox = TRUE,
                                    liveSearch = TRUE,
                                    selectAllText = "SELECIONAR TODOS",
                                    deselectAllText = "LIMPAR TODOS",
                                    noneSelectedText= "SELECIONE",
                                    noneResultsText = "SEM CORRESPONDÊNCIAS",
                                    selectedTextFormat = "count > 3",
                                    countSelectedText = "{0} ITENS SELECIONADOS")
          ),
          
          column(1),
          
          pickerInput(
            inputId = "antimicrobiano_medicamento",
            label = "Antimicrobiano",
            choices = sort(unique(antimicrob_tbl_final$MEDICATION_NAME)),
            multiple = TRUE,
            selected = NULL,
            options = pickerOptions(container = "body",
                                    title = "SELECIONE",
                                    actionsBox = TRUE,
                                    liveSearch = TRUE,
                                    selectAllText = "SELECIONAR TODOS",
                                    deselectAllText = "LIMPAR TODOS",
                                    noneSelectedText= "SELECIONE",
                                    noneResultsText = "SEM CORRESPONDÊNCIAS",
                                    selectedTextFormat = "count > 2",
                                    countSelectedText = "{0} ITENS SELECIONADOS")
          ),
          
          column(1),
          column(5, actionButton("reset_btn_mic", "Reset", width = '110%')),
          #column(1),
          column(5, actionButton("filter_btn_mic", "Filtrar", width = '110%'))
        )
      ),
      conditionalPanel(
        "input.nav === 'Aceitabilidade de Intervenções Farmacêuticas'",
        ### COLOQUE AQUI OS FILTROS DA PAGINA 3 ##
        fluidRow(
          
          radioGroupButtons(
            inputId = "aceitabilidade_type",
            label = "Tipo de intervenção",
            choices = c("Sugestão" = "sugestao", 
                        "Implementação" = "implementacao"),
            selected = 'sugestao',
            individual = F,
            justified = TRUE,
            size = "sm",
            status = "primary"
          ),
          column(1),
          
          dateRangeInput('aceitabilidade_date',
                         label = 'Data de geração do alerta ou implementação da sugestão',
                         start = aceitabilidade_date_range[1], end = aceitabilidade_date_range[2],
                         min = aceitabilidade_date_range[1], max = aceitabilidade_date_range[2],
                         separator = " até ", format = "dd-mm-yy",
                         startview = 'month', language = 'pt-BR'
          ),
          column(1),
          
          pickerInput(
            inputId = "aceitabilidade_setor",
            label = "Setor Hospitalar",
            choices = sort(unique(aceitabilidade_tbl$DESCRIPTION)),
            multiple = TRUE,
            selected = NULL,
            options = pickerOptions(container = "body",
                                    title = "SELECIONE",
                                    actionsBox = TRUE,
                                    liveSearch = TRUE,
                                    selectAllText = "SELECIONAR TODOS",
                                    deselectAllText = "LIMPAR TODOS",
                                    noneSelectedText= "SELECIONE",
                                    noneResultsText = "SEM CORRESPONDÊNCIAS",
                                    selectedTextFormat = "count > 3",
                                    countSelectedText = "{0} ITENS SELECIONADOS")
          ),
          
          column(1),
          
          pickerInput(
            inputId = "aceitabilidade_medico_id",
            label = "Médico Prescritor",
            choices = sort(unique(aceitabilidade_tbl$PRACTITIONER_ID)),
            multiple = TRUE,
            selected = NULL,
            options = pickerOptions(container = "body",
                                    title = "SELECIONE",
                                    actionsBox = TRUE,
                                    liveSearch = TRUE,
                                    selectAllText = "SELECIONAR TODOS",
                                    deselectAllText = "LIMPAR TODOS",
                                    noneSelectedText= "SELECIONE",
                                    noneResultsText = "SEM CORRESPONDÊNCIAS",
                                    selectedTextFormat = "count > 2",
                                    countSelectedText = "{0} ITENS SELECIONADOS")
          ),
          
          column(1),
          
          pickerInput(
            inputId = "aceitabilidade_user_id",
            label = "Farmacêutico",
            choices = sort(unique(aceitabilidade_tbl$USER_ID)),
            multiple = TRUE,
            selected = NULL,
            options = pickerOptions(container = "body",
                                    title = "SELECIONE",
                                    actionsBox = TRUE,
                                    liveSearch = TRUE,
                                    selectAllText = "SELECIONAR TODOS",
                                    deselectAllText = "LIMPAR TODOS",
                                    noneSelectedText= "SELECIONE",
                                    noneResultsText = "SEM CORRESPONDÊNCIAS",
                                    selectedTextFormat = "count > 2",
                                    countSelectedText = "{0} ITENS SELECIONADOS")
          ),
          column(1),
          
          selectInput(
            inputId = "aceitabilidade_adherence",
            label = "Resposta à intervenção",
            choices = c("Todas" = "all", "Aceitas" = TRUE, "Rejeitadas" = FALSE),
            selected = "Todas",
          ),
          
          column(1),
          column(5, actionButton("reset_btn_acpt", "Reset", width = '110%')),
          #column(1),
          column(5, actionButton("filter_btn_acpt", "Filtrar", width = '110%'))
        )
      ),
      
      conditionalPanel(
        "input.nav === 'Saving de Intervenções Farmacêuticas'",
        ### COLOQUE AQUI OS FILTROS DA PAGINA 4 ##
        fluidRow(
          dateRangeInput('saving_int_date',
                         label = 'Data de geração do alerta',
                         start = aceitabilidade_date_range[1], end = aceitabilidade_date_range[2],
                         min = aceitabilidade_date_range[1], max = aceitabilidade_date_range[2],
                         separator = " até ", format = "dd-mm-yy",
                         startview = 'month', language = 'pt-BR'),
                         
                         column(1),
                         
                         pickerInput(
                           inputId = "saving_int_setor",
                           label = "Setor Hospitalar",
                           choices = sort(unique(aceitabilidade_tbl$DESCRIPTION)),
                           multiple = TRUE,
                           selected = NULL,
                           options = pickerOptions(container = "body",
                                                   title = "SELECIONE",
                                                   actionsBox = TRUE,
                                                   liveSearch = TRUE,
                                                   selectAllText = "SELECIONAR TODOS",
                                                   deselectAllText = "LIMPAR TODOS",
                                                   noneSelectedText= "SELECIONE",
                                                   noneResultsText = "SEM CORRESPONDÊNCIAS",
                                                   selectedTextFormat = "count > 3",
                                                   countSelectedText = "{0} ITENS SELECIONADOS")
                         ),
                         
                         column(1),
                         
                         pickerInput(
                           inputId = "saving_int_medicamento",
                           label = "Medicamento",
                           choices = sort(unique(aceitabilidade_tbl$MEDICATION_NAME)),
                           multiple = TRUE,
                           selected = NULL,
                           options = pickerOptions(container = "body",
                                                   title = "SELECIONE",
                                                   actionsBox = TRUE,
                                                   liveSearch = TRUE,
                                                   selectAllText = "SELECIONAR TODOS",
                                                   deselectAllText = "LIMPAR TODOS",
                                                   noneSelectedText= "SELECIONE",
                                                   noneResultsText = "SEM CORRESPONDÊNCIAS",
                                                   selectedTextFormat = "count > 2",
                                                   countSelectedText = "{0} ITENS SELECIONADOS")
                         ),
                         
                         column(1),             
                         
                         pickerInput(
                           inputId = "saving_int_medico_id",
                           label = "Médico Prescritor",
                           choices = sort(unique(aceitabilidade_tbl$PRACTITIONER_ID)),
                           multiple = TRUE,
                           selected = NULL,
                           options = pickerOptions(container = "body",
                                                   title = "SELECIONE",
                                                   actionsBox = TRUE,
                                                   liveSearch = TRUE,
                                                   selectAllText = "SELECIONAR TODOS",
                                                   deselectAllText = "LIMPAR TODOS",
                                                   noneSelectedText= "SELECIONE",
                                                   noneResultsText = "SEM CORRESPONDÊNCIAS",
                                                   selectedTextFormat = "count > 2",
                                                   countSelectedText = "{0} ITENS SELECIONADOS")
                         ),
          column(5, actionButton("reset_btn_saving_int", "Reset", width = '125%')),
          column(1),
          column(5, actionButton("filter_btn_saving_int", "Filtrar", width = '125%'))
        )
      ),
      
      conditionalPanel(
        "input.nav === 'Custos por antibióticos'",
        ### COLOQUE AQUI OS FILTROS DA PAGINA 5 ##
        fluidRow(
          dateRangeInput('atb_cost_date',
                         label = 'Data de início da terapia',
                         start = antimicrob_date_range[1], end = antimicrob_date_range[2],
                         min = antimicrob_date_range[1], max = antimicrob_date_range[2],
                         separator = " até ", format = "dd-mm-yy",
                         startview = 'month', language = 'pt-BR'),
          
          column(1),
          
          pickerInput(
            inputId = "atb_cost_setor",
            label = "Setor Hospitalar",
            choices = sort(unique(antimicrob_tbl_final$DESCRIPTION)),
            multiple = TRUE,
            selected = NULL,
            options = pickerOptions(container = "body",
                                    title = "SELECIONE",
                                    actionsBox = TRUE,
                                    liveSearch = TRUE,
                                    selectAllText = "SELECIONAR TODOS",
                                    deselectAllText = "LIMPAR TODOS",
                                    noneSelectedText= "SELECIONE",
                                    noneResultsText = "SEM CORRESPONDÊNCIAS",
                                    selectedTextFormat = "count > 3",
                                    countSelectedText = "{0} ITENS SELECIONADOS")
          ),
          
          column(1),
          
          pickerInput(
            inputId = "atb_cost_medico_id",
            label = "Médico Prescritor",
            choices = sort(unique(antimicrob_tbl_final$PRACTITIONER_ID)),
            multiple = TRUE,
            selected = NULL,
            options = pickerOptions(container = "body",
                                    title = "SELECIONE",
                                    actionsBox = TRUE,
                                    liveSearch = TRUE,
                                    selectAllText = "SELECIONAR TODOS",
                                    deselectAllText = "LIMPAR TODOS",
                                    noneSelectedText= "SELECIONE",
                                    noneResultsText = "SEM CORRESPONDÊNCIAS",
                                    selectedTextFormat = "count > 3",
                                    countSelectedText = "{0} ITENS SELECIONADOS")
          ),
          
          
          column(1),
          
          pickerInput(
            inputId = "atb_cost_tuss",
            label = "Procedimento TUSS",
            choices = sort(unique(antimicrob_tbl_final$tuss_id_completo)),
            multiple = TRUE,
            selected = NULL,
            options = pickerOptions(container = "body",
                                    title = "SELECIONE",
                                    actionsBox = TRUE,
                                    liveSearch = TRUE,
                                    selectAllText = "SELECIONAR TODOS",
                                    deselectAllText = "LIMPAR TODOS",
                                    noneSelectedText= "SELECIONE",
                                    noneResultsText = "SEM CORRESPONDÊNCIAS",
                                    selectedTextFormat = "count > 3",
                                    countSelectedText = "{0} ITENS SELECIONADOS")
          ),
          
          pickerInput(
            inputId = "atb_cost_microorganism",
            label = "Microorganismo",
            choices = sort(unique(antimicrob_tbl_final$MICROORGANISM_ID)),
            multiple = TRUE,
            selected = NULL,
            options = pickerOptions(container = "body",
                                    title = "SELECIONE",
                                    actionsBox = TRUE,
                                    liveSearch = TRUE,
                                    selectAllText = "SELECIONAR TODOS",
                                    deselectAllText = "LIMPAR TODOS",
                                    noneSelectedText= "SELECIONE",
                                    noneResultsText = "SEM CORRESPONDÊNCIAS",
                                    selectedTextFormat = "count > 3",
                                    countSelectedText = "{0} ITENS SELECIONADOS")
          ),
          
          pickerInput(
            inputId = "atb_medication",
            label = "Setor Hospitalar",
            choices = sort(unique(antimicrob_tbl_final$MEDICATION_NAME)),
            multiple = TRUE,
            selected = NULL,
            options = pickerOptions(container = "body",
                                    title = "SELECIONE",
                                    actionsBox = TRUE,
                                    liveSearch = TRUE,
                                    selectAllText = "SELECIONAR TODOS",
                                    deselectAllText = "LIMPAR TODOS",
                                    noneSelectedText= "SELECIONE",
                                    noneResultsText = "SEM CORRESPONDÊNCIAS",
                                    selectedTextFormat = "count > 3",
                                    countSelectedText = "{0} ITENS SELECIONADOS")
          ),
          
          column(5, actionButton("reset_btn_atb_cost", "Reset", width = '125%')),
          column(1),
          column(5, actionButton("filter_btn_atb_cost", "Filtrar", width = '125%'))
        )
      ),
      
      conditionalPanel(
        "input.nav === 'Nome pag 6'",
        ### COLOQUE AQUI OS FILTROS DA PAGINA 6 ##
        fluidRow(
          column(5, actionButton("reset_btn_marker", "Reset", width = '125%')),
          column(1),
          column(5, actionButton("filter_btn_marker", "Filtrar", width = '125%'))
        )
      )
    ),
    
    nav_panel("Gestão de Isolamento",
              
              navset_card_underline(
                title = "Tempo entre Coleta e Resultado (Eficiência BDmax)",
                # Panel with plot ----
                nav_panel("Opção A", dygraphOutput("plot1a")),
                
                nav_panel("Opção B", 
                          
                          card(
                            height = 450,
                            max_height = 600,
                            min_height = 350,
                            full_screen = TRUE,
                            layout_sidebar(
                              fillable = TRUE,
                              sidebar = sidebar(
                                width = 140,
                                open = "closed",
                                radioGroupButtons(
                                  inputId = "gi_button_sector",
                                  label = "Visualização dos Setores Hospitalares",
                                  choices = c("Agregado", 
                                              "Separados"),
                                  size = "sm",
                                  selected = "Agregado",
                                  direction = "vertical"
                                )
                              ),
                              plotlyOutput("plot1b")
                            )
                          )
                          
                          ),
                
                nav_panel("Opção C", plotlyOutput("plot1c"))
              )),
             # mod_p1_ui("pag_1")),
    
    nav_panel("Consumo de antimicrobianos",
             
      navset_card_underline(
        title = "",
        nav_panel("DDD", 

                  card(
                    height = 450,
                    full_screen = TRUE,
                    layout_sidebar(
                      fillable = TRUE,
                      sidebar = sidebar(
                        width = 180,
                        open = "closed",
                        radioGroupButtons(
                          inputId = "ddd_plot_button",
                          label = "Escolha a visualização",
                          choices = c("Destacar itens", 
                                      "Ver somente itens selecionados"),
                          size = "sm",
                          direction = "vertical"
                        )
                      ),
                      plotlyOutput("ddd_plot")
                    )
                  ), # card
                  
                  #plot_ddd_month
               
                  card(
                    height = 450,
                    max_height = 600,
                    min_height = 350,
                    full_screen = TRUE,
                    card_header("DDD (Dose Diária Definida) ao longo do tempo",
                                style = "text-align: center;"),
                    layout_sidebar(
                      fillable = TRUE,
                      sidebar = sidebar(
                        width = 120,
                        open = "closed",
                        radioGroupButtons(
                          inputId = "ddd_plot_month_button",
                          label = "Selecione:",
                          choices = c("Opção A", 
                                      "Opção B"),
                          size = "sm",
                          direction = "vertical"
                        )
                      ),
                      uiOutput("ddd_month_plot_dynamic")
                    )
                  ), # card                  
                  
                  card(
                    height = 450,
                    max_height = 600,
                    min_height = 350,
                    full_screen = TRUE,
                    dataTableOutput("ddd_datatable")
                  )
                     
              ),
        
        nav_panel("DOT", 
                  
                  card(
                    height = 450,
                    full_screen = TRUE,
                    layout_sidebar(
                      fillable = TRUE,
                      sidebar = sidebar(
                        width = 180,
                        open = "closed",
                        radioGroupButtons(
                          inputId = "dot_plot_button",
                          label = "Escolha a visualização",
                          choices = c("Destacar itens", 
                                      "Ver somente itens selecionados"),
                          size = "sm",
                          direction = "vertical"
                        )
                      ),
                      plotlyOutput("dot_plot")
                    )
                  ), # card
                  
                  #plot_ddd_month
                  
                  card(
                    height = 450,
                    max_height = 600,
                    min_height = 350,
                    full_screen = TRUE,
                    card_header("DOT (Days of Therapy) ao longo do tempo",
                                style = "text-align: center;"),
                    layout_sidebar(
                      fillable = TRUE,
                      sidebar = sidebar(
                        width = 120,
                        open = "closed",
                        radioGroupButtons(
                          inputId = "dot_plot_month_button",
                          label = "Selecione:",
                          choices = c("Opção A", 
                                      "Opção B"),
                          size = "sm",
                          direction = "vertical"
                        )
                      ),
                      uiOutput("dot_month_plot_dynamic")
                    )
                  ), # card                  
                  
                  card(
                    height = 450,
                    max_height = 600,
                    min_height = 350,
                    full_screen = TRUE,
                    dataTableOutput("dot_datatable")
                  )
        ),
        
        nav_panel("LOT", 
                  
                  card(
                    height = 450,
                    full_screen = TRUE,
                    layout_sidebar(
                      fillable = TRUE,
                      sidebar = sidebar(
                        width = 180,
                        open = "closed",
                        radioGroupButtons(
                          inputId = "lot_plot_button",
                          label = "Escolha a visualização",
                          choices = c("Destacar itens", 
                                      "Ver somente itens selecionados"),
                          size = "sm",
                          direction = "vertical"
                        )
                      ),
                      plotlyOutput("lot_plot")
                    )
                  ), # card
                  
                  #plot_ddd_month
                  
                  card(
                    height = 450,
                    max_height = 600,
                    min_height = 350,
                    full_screen = TRUE,
                    card_header("LOT (Length of Treatment) ao longo do tempo",
                                style = "text-align: center;"),
                    layout_sidebar(
                      fillable = TRUE,
                      sidebar = sidebar(
                        width = 120,
                        open = "closed",
                        radioGroupButtons(
                          inputId = "lot_plot_month_button",
                          label = "Selecione:",
                          choices = c("Opção A", 
                                      "Opção B"),
                          size = "sm",
                          direction = "vertical"
                        )
                      ),
                      uiOutput("lot_month_plot_dynamic")
                    )
                  ), # card                  
                  
                  card(
                    height = 450,
                    max_height = 600,
                    min_height = 350,
                    full_screen = TRUE,
                    dataTableOutput("lot_datatable")
                  )
        )
        
        )),
    
    nav_panel("Aceitabilidade de Intervenções Farmacêuticas",
              card(
                card_header("Adesão às Intervenções Sugeridas",
                            style = "text-align: center;"),
                height = 450,
                max_height = 600,
                min_height = 350,
                full_screen = TRUE,
                uiOutput("aceitabilidade_pie")
              ),

              card(
                height = 450,
                max_height = 600,
                min_height = 350,
                full_screen = TRUE,
                card_header("Adesão às Intervenções Sugeridas ao Longo do Tempo",
                            style = "text-align: center;"),
                layout_sidebar(
                  fillable = TRUE,
                  sidebar = sidebar(
                    width = 120,
                    open = "closed",
                    radioGroupButtons(
                      inputId = "plot_acpt_time_setting",
                      label = "Selecione:",
                      choices = c("Mensal", 
                                  "Semanal"),
                      size = "sm",
                      selected = "Mensal",
                      direction = "vertical"
                    )
                  ),
                  uiOutput("plot_aceitabilidade_n_dynamic")
                )
              ), # card    
              
              card(
                height = 450,
                max_height = 600,
                min_height = 350,
                full_screen = TRUE,
                card_header("Tempo Médio de Resposta às Intervenções Sugeridas ao Longo do Tempo",
                            style = "text-align: center;"),
                layout_sidebar(
                  fillable = TRUE,
                  sidebar = sidebar(
                    width = 120,
                    open = "closed",
                    radioGroupButtons(
                      inputId = "plot_acpt_tm_time_setting",
                      label = "Selecione:",
                      choices = c("Mensal", 
                                  "Semanal"),
                      size = "sm",
                      selected = "Mensal",
                      direction = "vertical"
                    )
                  ),
                  uiOutput("plot_aceitabilidade_tm_dynamic")
                )
              ), # card    
              
              plotlyOutput("plot_aceitabilidade_implem_ratio")
              ),

    nav_panel("Saving de Intervenções Farmacêuticas",

                          layout_column_wrap(
                            width = 1/3,
                            height = 120,
                         
                            value_box(
                              title = "Custo Evitado Total",
                              value = textOutput("saving_total_value"),
                              showcase = bsicons::bs_icon("cash-coin"),
                              theme = value_box_theme(bg = "white", fg = "#0B538E"),
                              class = "border"
                            ),
                            
                            value_box(
                              title = "Custo Médio Evitado por Conversão",
                              value = textOutput("saving_per_conversion_value"),
                              showcase = bsicons::bs_icon("currency-dollar"),
                              theme = value_box_theme(bg = "white", fg = "#0B538E"),
                              class = "border"
                            ),
                            
                            value_box(
                              title = "Taxa de Conversão Efetivada das Sugestões IV-VO",
                              value = textOutput("saving_ratio"),
                              showcase = bsicons::bs_icon("arrow-right-square"),
                              theme = value_box_theme(bg = "white", fg = "#0B538E"),
                              class = "border"
                            )
                          ),
                            
                            
                          layout_column_wrap(
                            width = 1/2,
                            height = 450,
                            card(full_screen = TRUE, 
                                 card_header("Efetividade Econômica das Intervenções",
                                             style = "text-align: center;"), 
                                 plotlyOutput("plot_saving_int_total")
                                 ),
                            card(full_screen = TRUE, 
                                 card_header("Redução no Tempo Médio de Uso Intravenoso (IV) Antes da Conversão",
                                             style = "text-align: center;"), 
                                 card_body(class = "p-0", 
                                           plotlyOutput("plot_saving_int_tm_total"))
                                 )
                          ),
                          
                          card(
                            height = 450,
                            max_height = 600,
                            min_height = 350,
                            full_screen = TRUE,
                            card_header("Taxa de Implementação das Intervenções Sugeridas",
                                        style = "text-align: center;"),
                            layout_sidebar(
                              fillable = TRUE,
                              sidebar = sidebar(
                                width = 120,
                                open = "closed",
                                radioGroupButtons(
                                  inputId = "plot_saving_int_time_setting",
                                  label = "Selecione:",
                                  choices = c("Mensal", 
                                              "Semanal"),
                                  size = "sm",
                                  selected = "Mensal",
                                  direction = "vertical"
                                )
                              ),
                              plotlyOutput("plot_saving_int_ratio")
                            )
                          ), # card
              
              card(
                height = 450,
                full_screen = TRUE,
                layout_sidebar(
                  fillable = TRUE,
                  sidebar = sidebar(
                    width = 180,
                    open = "closed",
                    radioGroupButtons(
                      inputId = "si_sector_button",
                      label = "Escolha a visualização",
                      choices = c("Destacar itens", 
                                  "Ver somente itens selecionados"),
                      size = "sm",
                      direction = "vertical"
                    )
                  ),
                  plotlyOutput("plot_si_cost_sector")
                )
              )
    ),

    nav_panel("Custos por antibióticos",
              
              layout_column_wrap(
                width = 1/2,
                height = 120,
                
                value_box(
                  title = "Custo Total com Antibióticos",
                  value = textOutput("atb_cost_total_value"),
                  showcase = bsicons::bs_icon("prescription2"),
                  theme = value_box_theme(bg = "white", fg = "#0B538E"),
                  class = "border"
                ),
                
                value_box(
                  title = "Custo Médio por Tratamento",
                  value = textOutput("atb_mean_cost_value"),
                  showcase = bsicons::bs_icon("clipboard2-pulse"),
                  theme = value_box_theme(bg = "white", fg = "#0B538E"),
                  class = "border"
                )
              ),
              
              card(
                height = 450,
                max_height = 600,
                min_height = 350,
                full_screen = TRUE,
                card_header("Custo de Antibióticos ao Longo do Tempo",
                            style = "text-align: center;"),
                layout_sidebar(
                  fillable = TRUE,
                  sidebar = sidebar(
                    width = 120,
                    open = "closed",
                    radioGroupButtons(
                      inputId = "plot_atb_cost_time_setting",
                      label = "Selecione:",
                      choices = c("Mensal", 
                                  "Semanal"),
                      size = "sm",
                      selected = "Mensal",
                      direction = "vertical"
                    )
                  ),
                  plotlyOutput("plot_atb_cost")
                )
              )
              ),
    # 
    # nav_panel("Saving Giro de Leito",
    #         #  mod_p6_ui("pag_6")
    #           ),
    
    theme = bs_theme(
      version = 5,
      bootswatch = "lumen",
      base_font = font_collection(
        font_google("Open Sans"),
        "-apple-system", 
        "BlinkMacSystemFont", 
        "Segoe UI", 
        font_google("Roboto"), 
        "Helvetica Neue", 
        "Arial", 
        "sans-serif"
      ),
      code_font = font_collection(
        font_google("Open Sans"),
        "-apple-system", 
        "BlinkMacSystemFont", 
        "Segoe UI", 
        font_google("Roboto"),
        "Helvetica Neue", 
        "Arial", 
        "sans-serif"
      ),
      font_scale = NULL
    )
  ))

# Definindo o servidor
server <- function(input, output, session) {

### Carregando dados

  #PRD 1
  # gestao_isolamento_tbl_csv <- read.csv(file = "data/gestao_isolamento_tbl.csv")
  # setores_hospitalares <- read.csv("data/setor_hospitalar.csv")
  # 
  # aceitabilidade_tbl <- read_csv(file = "data/aceitabilidade_tbl.csv")
  # 
  # aceitabilidade_date_range <- range(c(range(aceitabilidade_tbl$ALERT_DATE, na.rm = TRUE),
  #                                      range(aceitabilidade_tbl$ALERT_ADHERENCE_DATE, na.rm = TRUE)))
  # 
  # gestao_isolamento_tbl <- gestao_isolamento_tbl_csv %>%
  #   mutate(
  #     ADMISSION_DATE = as.POSIXct(ADMISSION_DATE, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  #     collection_date_time = as.POSIXct(collection_date_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  #     result_status_date_time = as.POSIXct(result_status_date_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  #     RELEASE_DATE = as.POSIXct(RELEASE_DATE, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  #     dif = as.numeric(difftime(result_status_date_time, collection_date_time, units = "hours")),
  #     month = as.Date(floor_date(collection_date_time, unit = "month"))) %>%
  #   left_join(setores_hospitalares,  by = "UNIT_ID")
  # 
  # gestao_isolamento_setores_list <- c("Todos", sort(unique(gestao_isolamento_tbl$DESCRIPTION)))
  # 
  # gestao_isolamento_range <- as.Date(range(c(range(gestao_isolamento_tbl$ADMISSION_DATE),
  #                              range(gestao_isolamento_tbl$collection_date_time),
  #                              range(gestao_isolamento_tbl$result_status_date_time),
  #                              range(gestao_isolamento_tbl$RELEASE_DATE))))
  
  #PRD 2
  
  # antimicrob_tbl_final <- read_csv(file = "data/antimicrob_tbl.csv")
  # 
  # antimicrob_tbl_codes <- read.csv(file = "data/antimicrob_tbl_codes.csv")
  # 
  # antimicrob_date_range <- range(antimicrob_tbl_final$THERAPY_START_DATE, na.rm = TRUE)
  # 
  # antimicrob_setores_list <- sort(unique(antimicrob_tbl_final$DESCRIPTION))
  
  # PRD 4
  
# #### Carregando os Modulos do Server ####

  output$valid_token <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    token <- NULL
    if(!is.null(query[['token']])){
      token <- query[['token']]
    }
    token_id <- tryCatch(
      jwt_decode_sig(token, pubkey = pub_key),
      error = function(e) {
        return(list(iat = FALSE))
      })
    if(!token_id$iat) {
      vals$auth <- TRUE   ### MUDE ESSA PARTE PARA FALSE QUANDO QUISER ATIVAR A SEGURANÇA - Não precisa mexer
    } else {
      vals$auth <- TRUE
    }
    return(vals$auth)
  })

  outputOptions(output, 'valid_token', suspendWhenHidden = FALSE)

  vals <- reactiveValues(auth = FALSE)

  # observeEvent("", {
  #   if(vals$auth){
  #     mod_p1_server("pag_1", filtered_data_p1) # PASSE SEUS DADOS AQUI PARA COMUNICAR COM OS MODULOS
  #     mod_p2_server("pag_2", filtered_data_p2) # PASSE SEUS DADOS AQUI PARA COMUNICAR COM OS MODULOS
  #     mod_p3_server("pag_3", filtered_data_p3) # PASSE SEUS DADOS AQUI PARA COMUNICAR COM OS MODULOS
  #   } else {
  #     shinyalert("Erro de Acesso", "Entre no Munai BI através da Plataforma da Munai", type = "error")
  #   }
  # })
  
  
  ##### FILTROS PAG_1 ##### DESENVOLVA POR DATA-SET REATIVOS
  # Reactive values to store filters
  filter_gi <- reactiveValues(
    gestao_isolamento_tipo_data = "coleta",
    gestao_isolamento_data = gestao_isolamento_range,
    isolamento_setor = NULL,
    isolamento_medico_id = NULL
  )
  
  # Update reactive values when the button is pressed
  observeEvent(input$filter_btn_gi, {
    filter_gi$gestao_isolamento_tipo_data <- input$gestao_isolamento_tipo_data
    filter_gi$gestao_isolamento_data <- input$gestao_isolamento_data
    filter_gi$isolamento_setor <- input$isolamento_setor
    filter_gi$isolamento_medico_id <- input$isolamento_medico_id
  })
  
  ## Atualização de Botões ##
  # Pag 1
  
  observeEvent(input$reset_btn_gi, {
    
    updateDateRangeInput(session, "gestao_isolamento_data",
                         start = gestao_isolamento_range[1],
                         end = gestao_isolamento_range[2],
                         min = gestao_isolamento_range[1], 
                         max = gestao_isolamento_range[2])
    updatePickerInput(session, "isolamento_setor", selected = "")
    updatePickerInput(session, "isolamento_medico_id", selected = "")
  
  })
  
  # Pag 2
  observeEvent(input$reset_btn_mic, {
    
    updateDateRangeInput(session, "internacao_data",
                         start = antimicrob_date_range[1],
                         end = antimicrob_date_range[2],
                         min = antimicrob_date_range[1], 
                         max = antimicrob_date_range[2])
    
    updatePickerInput(session, "antimicrobiano_setor", selected = "")
    
    updatePickerInput(session, "antimicrobiano_medicamento", selected = "")
    
  })

  # Pag 3
  observeEvent(input$reset_btn_acpt, {
    
    updateDateRangeInput(session, "aceitabilidade_date",
                         start = aceitabilidade_date_range[1],
                         end = aceitabilidade_date_range[2],
                         min = aceitabilidade_date_range[1], 
                         max = aceitabilidade_date_range[2])

    updatePickerInput(session, "aceitabilidade_setor", selected = "")

    updatePickerInput(session, "aceitabilidade_medico_id", selected = "")

    updatePickerInput(session, "aceitabilidade_user_id", selected = "")

    updateSelectInput(session, "aceitabilidade_adherence", selected = "all")

  })
  
  # Pag 4
  observeEvent(input$reset_btn_saving_int, {
    
    updateDateRangeInput(session, "saving_int_date",
                         start = aceitabilidade_date_range[1],
                         end = aceitabilidade_date_range[2],
                         min = aceitabilidade_date_range[1], 
                         max = aceitabilidade_date_range[2])
    
    updatePickerInput(session, "saving_int_setor", selected = "")
    
    updatePickerInput(session, "saving_int_medicamento", selected = "")
    
    updatePickerInput(session, "saving_int_medico_id", selected = "")
    
  })
  
  # Pag 5
  observeEvent(input$reset_btn_atb_cost, {
    
    updateDateRangeInput(session, "atb_cost_date",
                         start = antimicrob_date_range[1],
                         end = antimicrob_date_range[2],
                         min = antimicrob_date_range[1], 
                         max = antimicrob_date_range[2])
    
    updatePickerInput(session, "atb_cost_setor", selected = "")
    
    updatePickerInput(session, "atb_cost_medico_id", selected = "")
    
    updatePickerInput(session, "atb_cost_tuss", selected = "")
    
    updatePickerInput(session, "atb_cost_microorganism", selected = "")
    
    updatePickerInput(session, "atb_medication", selected = "")
    
  })
  
  ##### FILTROS PAG_2 ##### DESENVOLVA POR DATA-SET REATIVOS
  filtered_data_p2 <- reactive({
    input$filter_btn  
    isolate({  
      
      df_filtered <- df 
      
      return(df_filtered)
    })
  })
  
  ## Atualização de Botões ##
  observeEvent(input$reset_btn, {
    updateDateRangeInput(session, "date_range",
                         start = default_start_date,
                         end = default_last_date,
                         min = default_start_date, 
                         max = default_last_date)
    updateDateRangeInput(session, "date_range_resultado",
                         start = default_start_date,
                         end = default_last_date,
                         min = default_start_date, 
                         max = default_last_date)
    
    
  })
  
  ##### FILTROS PAG_3 ##### DESENVOLVA POR DATA-SET REATIVOS
  filtered_data_p3 <- reactive({
    input$filter_btn  
    isolate({  
      
      df_filtered <- df 
      
      return(df_filtered)
    })
  })
  
  ## Atualização de Botões ##
  observeEvent(input$reset_btn, {
    updateDateRangeInput(session, "date_range",
                         start = default_start_date,
                         end = default_last_date,
                         min = default_start_date, 
                         max = default_last_date)
    updateDateRangeInput(session, "date_range_resultado",
                         start = default_start_date,
                         end = default_last_date,
                         min = default_start_date, 
                         max = default_last_date)
  })
  
  gi_tbl_prev_date <- reactive({
    
    # Filtro por tipo de data
    if (filter_gi$gestao_isolamento_tipo_data == "entrada") {
      
      gestao_isolamento_tbl_filter_data <- gestao_isolamento_tbl %>%
        dplyr::filter(ADMISSION_DATE >= filter_gi$gestao_isolamento_data[1] &
                        ADMISSION_DATE <= filter_gi$gestao_isolamento_data[2])
    }
    
    if (filter_gi$gestao_isolamento_tipo_data == "coleta") {
      
      gestao_isolamento_tbl_filter_data <- gestao_isolamento_tbl %>%
        dplyr::filter(collection_date_time >= filter_gi$gestao_isolamento_data[1] &
                        collection_date_time <= filter_gi$gestao_isolamento_data[2])
    }
    
    if (filter_gi$gestao_isolamento_tipo_data == "resultado") {
      
      gestao_isolamento_tbl_filter_data <- gestao_isolamento_tbl %>%
        dplyr::filter(result_status_date_time >= filter_gi$gestao_isolamento_data[1] &
                        result_status_date_time <= filter_gi$gestao_isolamento_data[2])
    }
    
    if (filter_gi$gestao_isolamento_tipo_data == "release") {
      
      gestao_isolamento_tbl_filter_data <- gestao_isolamento_tbl %>%
        dplyr::filter(RELEASE_DATE >= filter_gi$gestao_isolamento_data[1] &
                        RELEASE_DATE <= filter_gi$gestao_isolamento_data[2])
    }
    gestao_isolamento_tbl_filter_data
  })

    gi_tbl_prev <- reactive({
      
    # Filtro pro Setor
    if (is.null(filter_gi$isolamento_setor)) {
      gestao_isolamento_tbl_filter_setor <- gi_tbl_prev_date()
    } else {
      gestao_isolamento_tbl_filter_setor <- gi_tbl_prev_date() %>%
          dplyr::filter(DESCRIPTION %in% filter_gi$isolamento_setor)
    }
    
    if (is.null(filter_gi$isolamento_medico_id)) {
      gestao_isolamento_tbl_filter_medico <- gestao_isolamento_tbl_filter_setor
    } else {
      gestao_isolamento_tbl_filter_medico <- gestao_isolamento_tbl_filter_setor %>%
        dplyr::filter(PRACTITIONER_ID %in% filter_gi$isolamento_medico_id)
    }
    
    # Calcular horas e mês de acordo com o tipo de data escolhida
    
    if (filter_gi$gestao_isolamento_tipo_data == "entrada") {
      gestao_isolamento_tbl_output <- gestao_isolamento_tbl_filter_medico %>%
        mutate(Horas = as.numeric(difftime(result_status_date_time, collection_date_time, units = "hours")),
               month = as.Date(floor_date(ADMISSION_DATE, unit = "month")))
    }
    
    if (filter_gi$gestao_isolamento_tipo_data == "coleta") {
      gestao_isolamento_tbl_output <- gestao_isolamento_tbl_filter_medico %>%
        mutate(Horas = as.numeric(difftime(result_status_date_time, collection_date_time, units = "hours")),
               month = as.Date(floor_date(collection_date_time, unit = "month")))
    }
    
    if (filter_gi$gestao_isolamento_tipo_data == "resultado") {
      gestao_isolamento_tbl_output <- gestao_isolamento_tbl_filter_medico %>%
        mutate(Horas = as.numeric(difftime(result_status_date_time, collection_date_time, units = "hours")),
               month = as.Date(floor_date(result_status_date_time, unit = "month")))
    }
    
    if (filter_gi$gestao_isolamento_tipo_data == "release") {
      gestao_isolamento_tbl_output <- gestao_isolamento_tbl_filter_medico %>%
        mutate(Horas = as.numeric(difftime(result_status_date_time, collection_date_time, units = "hours")),
               month = as.Date(floor_date(RELEASE_DATE, unit = "month")))
    }
    
    gestao_isolamento_tbl_output

  })
  
  gi_tbl <- reactive({
    gi_tbl_prev() %>% 
      mutate(Horas = as.numeric(difftime(result_status_date_time, collection_date_time, units = "hours")),
             month = as.Date(floor_date(collection_date_time, unit = "month"))) |>
      group_by(month) |>
      summarise(Horas = mean(Horas, na.rm = TRUE)) %>%
      select(month, Horas) %>%
      arrange(month)
    })
  
  gi_tbl_sector <- reactive({
    
    if(nrow(gi_tbl_prev())==0) {
      data.frame(month = range(gestao_isolamento_tbl$month),
                                         Horas = 0,
                                         DESCRIPTION = NA)
    } else {

    gi_tbl_prev() %>%
        mutate(Horas = as.numeric(difftime(result_status_date_time, collection_date_time, units = "hours")),
               month = as.Date(floor_date(collection_date_time, unit = "month"))) |>
        group_by(month, DESCRIPTION) |>
        reframe(Horas = mean(Horas, na.rm = TRUE)) %>%
        select(month, DESCRIPTION, Horas) %>%
        arrange(DESCRIPTION, month) %>%
        mutate(DESCRIPTION = factor(DESCRIPTION)) %>%
        complete(DESCRIPTION,
                 month = seq(min(month), max(month), by = "month"),
                 fill = list(Horas = 0))
    }
  })
  
  # Diferença máxima de tempo na gestão de isolamento (para limite do eixo y)
  max_dif_isolamento <- gestao_isolamento_tbl %>% 
    mutate(Horas = as.numeric(difftime(result_status_date_time, collection_date_time, units = "hours")),
           month = as.Date(floor_date(collection_date_time, unit = "month"))) |>
    group_by(month, DESCRIPTION) |>
    summarise(Horas = mean(Horas)) %>%
    ungroup() %>%
    filter(Horas == max(Horas, na.rm = TRUE)) %>%
    select(Horas) %>%
    mutate(Horas = Horas*1.2)
  
  
  xts_dataset_gi <- reactive({ 
    
  # Preparar tabela em branco para plot de gestao de isolamento
  date_range_gi <- range(gi_tbl_prev_date()$month)
  
  date_seq_gi <- seq(floor_date(date_range_gi[1], unit="month"), 
                     ceiling_date(date_range_gi[2], unit = "month"), 
                     by = "month")
  
  blank_dataset_gi = data.frame(month = date_seq_gi,
                                Horas = rep(NA, length(date_seq_gi)))
  
  ####
  first_row_isol_tbl <- blank_dataset_gi[1,]
  first_row_isol_tbl[,"Horas"] <- NA
  
  last_row_isol_tbl <- first_row_isol_tbl
  
  first_row_isol_tbl[,"month"] = min(blank_dataset_gi$month, na.rm = T) - 2
  last_row_isol_tbl[,"month"] = max(blank_dataset_gi$month, na.rm = T) + 2
  
  expanded_blank_dataset_gi <- rbind(first_row_isol_tbl, blank_dataset_gi, last_row_isol_tbl)
  
  xts(expanded_blank_dataset_gi %>% select(Horas),
                        order.by = expanded_blank_dataset_gi$month)
  })
  
  
  output$plot1a <- renderDygraph({

    if(is.null(gi_tbl()) | nrow(gi_tbl()) == 0) {

      ### Visualização 1
      dygraph(xts_dataset_gi()) |>
        dyRangeSelector() |> # seletor inferior de tempo
        dyCrosshair(direction = 'vertical') |> # linhas verticais pra acompanhar o mouse
        dyLimit(as.numeric(24), "24 horas", color = "red", labelLoc = "right") %>%
        dyEvent('2023-4-1', 'Solução Go-Live', labelLoc = c('bottom'), 
                strokePattern = 'solid', color = "grey") |> # indicador de evento
        dyAxis('y', label = 'Tempo (horas)', valueRange = c(0,max_dif_isolamento$Horas)) |> # label y
        # dyAxis('x', label = "", drawGrid = FALSE, valueRange = range(gi_tbl()$month) + c(-3,3)) |> # retirando as linhas verticais do gráfico
        dyOptions(useDataTimezone = TRUE, colors = '#08A18E', rightGap = 10, # colocando estilo mês/dia/ano quando passa o mouse
                  drawPoints = TRUE, 
                  pointSize = 3,
                  fillGraph  = TRUE,
                  animatedZooms = TRUE,
                  includeZero = TRUE,
                  digitsAfterDecimal = 1) |> 
        dyUnzoom() # dando opção de desfazer o zoom
    } else {
    
    xts_dataset_gi_plot <- xts_dataset_gi() 
    xts_dataset_gi_plot$Horas[2:(nrow(xts_dataset_gi())-1)] <- 0
    xts_dataset_gi_plot[gi_tbl()$month,]$Horas <- gi_tbl()$Horas
      
    if(nrow(gi_tbl()) == 1) {
      
      ### Visualização 1
      dygraph(xts_dataset_gi_plot) |>
        dyRangeSelector() |> # seletor inferior de tempo
        dyBarChart() |> # tipo de gráfico
        dyCrosshair(direction = 'vertical') |> # linhas verticais pra acompanhar o mouse
        dyLimit(as.numeric(24), "24 horas", color = "red", labelLoc = "right") %>%
        dyEvent('2023-4-1', 'Solução Go-Live', labelLoc = c('bottom'), 
                strokePattern = 'solid', color = "grey") |> # indicador de evento
        dyAxis('y', label = 'Tempo (horas)', valueRange = c(0,max_dif_isolamento$Horas)) |> # label y
        # dyAxis('x', label = "", drawGrid = FALSE, valueRange = range(gi_tbl()$month) + c(-3,3)) |> # retirando as linhas verticais do gráfico
        dyOptions(useDataTimezone = TRUE, colors = '#08A18E', rightGap = 10, # colocando estilo mês/dia/ano quando passa o mouse
                  drawPoints = TRUE, 
                  pointSize = 3,
                  fillGraph  = TRUE,
                  animatedZooms = TRUE,
                  includeZero = TRUE,
                  digitsAfterDecimal = 1) |> 
        dyUnzoom() # dando opção de desfazer o zoom
    } else {
    
    ### Visualização 1
    dygraph(xts_dataset_gi_plot) |>
      dyRangeSelector() |> # seletor inferior de tempo
      dySeries("Horas", label = "Tempo (horas)") %>%
      dyCrosshair(direction = 'vertical') |> # linhas verticais pra acompanhar o mouse
      dyLimit(as.numeric(24), "24 horas", color = "red", labelLoc = "right") %>%
      dyEvent('2023-4-1', 'Solução Go-Live', labelLoc = c('bottom'), 
              strokePattern = 'solid', color = "grey") |> # indicador de evento
      dyAxis('y', label = 'Tempo (horas)', valueRange = c(0,max_dif_isolamento$Horas)) |> # label y
      # dyAxis('x', label = "", drawGrid = FALSE, valueRange = range(gi_tbl()$month) + c(-3,3)) |> # retirando as linhas verticais do gráfico
      dyOptions(useDataTimezone = TRUE, colors = '#08A18E', rightGap = 10, # colocando estilo mês/dia/ano quando passa o mouse
                drawPoints = TRUE, 
                pointSize = 3,
                fillGraph  = TRUE,
                animatedZooms = TRUE,
                includeZero = TRUE,
                digitsAfterDecimal = 1) |> 
      dyUnzoom() # dando opção de desfazer o zoom
    }
    }

  })

  output$plot1b <- renderPlotly({
    
    if(input$gi_button_sector == "Agregado") {
    
    xts_dataset_gi_ggplot <- xts_dataset_gi()[-c(1,nrow(xts_dataset_gi())),]
    xts_dataset_gi_ggplot[index(xts_dataset_gi()),]$Horas <- 0
    xts_dataset_gi_ggplot[gi_tbl()$month,]$Horas <- gi_tbl()$Horas
    
    dataset_gi_ggplot <- data.frame(month = index(xts_dataset_gi_ggplot),
                                    Horas = xts_dataset_gi_ggplot$Horas)

    p4 <- dataset_gi_ggplot %>%
      ggplot(aes(x = month, y = Horas)) +
      annotate("text", x = max(dataset_gi_ggplot$month, na.rm = T), y = 26, 
               label = "24 horas",
               color = "red",
               size = 3) + 
      geom_hline(yintercept = 24, linetype = "dashed", color = "red") +
      xlab("") +
      ylab("Tempo (horas)") +
      theme_minimal() +
      scale_y_continuous(expand = c(0, 0), limits = c(0, max_dif_isolamento$Horas)) +
      scale_x_date(breaks = "month", date_labels = "%b %Y") +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            axis.line = element_line(),
            plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches"))
    
    if(sum(dataset_gi_ggplot$Horas) > 0) {
      p4 <- p4 +
        geom_line(color = '#08A18E') +
        geom_point(aes(text = paste0('Tempo: ', round(Horas,1), ' horas')),
                   color = '#08A18E')
    }  
    
    
    } else {

      p4 <- gi_tbl_sector() %>%
        ggplot(aes(x = month, y = Horas, color = DESCRIPTION)) +
        annotate("text", x = max(gi_tbl_sector()$month, na.rm = T), y = 26, 
                 label = "24 horas",
                 color = "red",
                 size = 3) + 
        geom_hline(yintercept = 24, linetype = "dashed", color = "red") +
        xlab("") +
        ylab("Tempo (horas)") +
        theme_minimal() +
        scale_y_continuous(expand = c(0, 0), limits = c(0, max_dif_isolamento$Horas)) +
        scale_x_date(breaks = "month", date_labels = "%b %Y") +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
              axis.line = element_line(),
              plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches"),
              legend.position = "none")
      
      if(nrow(gi_tbl_prev()) > 0) {
      p4 <- p4 + geom_line() +
        geom_point(aes(text = paste0('Setor: ', DESCRIPTION, '<br>Tempo: ', round(Horas,1), ' horas')))
      }
    }
    
    ggplotly(p4, tooltip = 'text') 
  })
  
  
  output$plot1c <- renderPlotly({
    
    xts_dataset_gi_ggplot <- xts_dataset_gi()[-c(1,nrow(xts_dataset_gi())),]
    xts_dataset_gi_ggplot[index(xts_dataset_gi()),]$Horas <- 0
    xts_dataset_gi_ggplot[gi_tbl()$month,]$Horas <- gi_tbl()$Horas
    
    dataset_gi_ggplot <- data.frame(month = index(xts_dataset_gi_ggplot),
                                    Horas = xts_dataset_gi_ggplot$Horas)
                                    
    p3 <- dataset_gi_ggplot %>%
      group_by(month) %>% 
      summarise(Horas = mean(Horas)) %>% 
      ggplot(aes(x = month, y = Horas,
                 text = paste0('Tempo: ', round(Horas,1), ' horas'))) +
      geom_col(fill = '#08A18E', width = 20) +
      annotate("text", x = max(dataset_gi_ggplot$month, na.rm = T), y = 26, 
               label = "24 horas",
               color = "red",
               size = 3) + 
      geom_hline(yintercept = 24, linetype = "dashed", color = "red") +
      xlab("") +
      ylab("Tempo (horas)") +
      theme_minimal() +
      scale_y_continuous(expand = c(0, 0), limits = c(0, max_dif_isolamento$Horas)) +
      scale_x_date(breaks = "month", date_labels = "%b %Y", limits = range(dataset_gi_ggplot$month)+c(-10,10)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            axis.line = element_line(),
            plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches"))

    ggplotly(p3, tooltip = 'text')
    
  })
  
  ### PRD 2
  
  # Reactive values to store filters
  filter_mic <- reactiveValues(
    date_range = antimicrob_date_range,
    setor_mic = NULL,
    medicamento_mic = NULL
  )
  
  # Update reactive values when the button is pressed
  observeEvent(input$filter_btn_mic, {
    filter_mic$date_range <- input$internacao_data
    filter_mic$setor_mic <- input$antimicrobiano_setor
    filter_mic$medicamento_mic <- input$antimicrobiano_medicamento
  })
  
  DDD_tbl <- reactive({
    
      antimicrob_tbl_final1 <- antimicrob_tbl_final %>%
        dplyr::filter(ADM_START_DATE >= filter_mic$date_range[1] &
                      ADM_START_DATE <= filter_mic$date_range[2])
    
    if (is.null(filter_mic$setor_mic)) {
      antimicrob_tbl_final2 <- antimicrob_tbl_final1
    } else {
      antimicrob_tbl_final2 <- antimicrob_tbl_final1 %>%
        dplyr::filter(DESCRIPTION %in% filter_mic$setor_mic)
    }
      return(antimicrob_tbl_final2)
  })
  
  output$ddd_plot <- renderPlotly({
    
    # Opção A: Filtrar medicamentos
    
    if(input$ddd_plot_button == "Ver somente itens selecionados") {
    
    if (is.null(filter_mic$medicamento_mic)) {
      antimicrob_tbl_final3 <- DDD_tbl()
    } else {
      antimicrob_tbl_final3 <- DDD_tbl() %>%
        dplyr::filter(MEDICATION_NAME %in% filter_mic$medicamento_mic)
    }
    
    # Calcular DDD por medicamento
    antimicrob_tbl_final4 <- antimicrob_tbl_final3 %>%
      group_by(MEDICATION_NAME, MEDICATION_CLASS_CODE) %>%
      reframe(A_ratio_B = sum(DD)/unique(DDD_OMS),
              P = sum(ADMISSION_DURATION)) %>%
      ungroup() %>%
      mutate(DDD = A_ratio_B/P*1000) %>%
      # Filtrar para deixar nomes de medicamentos únicos (verificar como será o tratamento com os dados reais)
      group_by(MEDICATION_NAME) %>%
      mutate(n_med_name = 1:n()) %>%
      ungroup() %>%
      filter(n_med_name == 1)  
    
  plot_ddd <- antimicrob_tbl_final4 %>%
    arrange(desc(DDD)) %>%
    ggplot(aes(y = fct_reorder(MEDICATION_NAME, DDD), x = DDD,
               labels = "",
               text = paste0(MEDICATION_NAME,
                             '<br>DDD: ', round(DDD,2)))) +
    geom_col(fill = "#1F77B4") +
    theme_minimal() +
    ylab("") +
    xlab("g/1000 pacientes-dia") +
    labs(title = "DDD (Dose Diária Definida)") +
    theme(axis.line.y = element_line()) +
    scale_x_continuous(expand = c(0, 0))
  
  ggplotly(plot_ddd, tooltip = "text")
  } else { 
  #if(input$ddd_plot_button == "Destacar itens")
  # Opção B: Highlight nos medicamentos selecionados
  
    # Calcular DDD por medicamento
    antimicrob_tbl_final3b <- DDD_tbl() %>%
      group_by(MEDICATION_NAME, MEDICATION_CLASS_CODE) %>%
      reframe(A_ratio_B = sum(DD)/unique(DDD_OMS),
              P = sum(ADMISSION_DURATION)) %>%
      ungroup() %>%
      mutate(DDD = A_ratio_B/P*1000) %>%
      # Filtrar para deixar nomes de medicamentos únicos (verificar como será o tratamento com os dados reais)
      group_by(MEDICATION_NAME) %>%
      mutate(n_med_name = 1:n()) %>%
      ungroup() %>%
      filter(n_med_name == 1) %>%
      mutate(highlight_medication = ifelse(MEDICATION_NAME %in% filter_mic$medicamento_mic, T, F))
    
    plot_ddd2 <- antimicrob_tbl_final3b %>%
      arrange(desc(DDD)) %>%
      ggplot(aes(y = fct_reorder(MEDICATION_NAME, DDD), x = DDD,
                 fill = highlight_medication,
                 labels = "",
                 text = paste0(MEDICATION_NAME,
                               '<br>DDD: ', round(DDD,2)))) +
      geom_col() +
      scale_fill_manual(values = c("#1F77B4", "orange")) +
      theme_minimal() +
      ylab("") +
      xlab("g/1000 pacientes-dia") +
      labs(title = "DDD (Dose Diária Definida)") +
      theme(axis.line.y = element_line(),
            legend.position = "none") +
      scale_x_continuous(expand = c(0, 0))
    
    ggplotly(plot_ddd2, tooltip = "text")
    }
  })
  
  # Calcular DDD por medicamento e mês
  DDD_month_tbl <- reactive({ 
    
    # Filtro por data
    antimicrob_tbl_final1 <- antimicrob_tbl_final %>%
      dplyr::filter(ADM_START_DATE >= filter_mic$date_range[1] &
                      ADM_START_DATE <= filter_mic$date_range[2])
    
    # Filtro por setor
    if (is.null(filter_mic$setor_mic)) {
      antimicrob_tbl_final2 <- antimicrob_tbl_final1
    } else {
      antimicrob_tbl_final2 <- antimicrob_tbl_final1 %>%
        dplyr::filter(DESCRIPTION %in% filter_mic$setor_mic)
    }
    
    # Filtro por medicamento
    if (is.null(filter_mic$medicamento_mic)) {
      antimicrob_tbl_final3 <- antimicrob_tbl_final2
    } else {
      antimicrob_tbl_final3 <- antimicrob_tbl_final2 %>%
        dplyr::filter(MEDICATION_NAME %in% filter_mic$medicamento_mic)
    }

    antimicrob_tbl_final3 %>%
      group_by(MEDICATION_NAME, ADM_START_MONTH) %>%
      reframe(A_ratio_B = sum(DD)/unique(DDD_OMS),
              P = sum(ADMISSION_DURATION)) %>%
      ungroup() %>%
      mutate(DDD = A_ratio_B/P*1000) %>%
      group_by(MEDICATION_NAME, ADM_START_MONTH) %>%
      mutate(n_med_name = 1:n()) %>%
      ungroup() %>%
      filter(n_med_name == 1) %>%
      select(MEDICATION_NAME, ADM_START_MONTH, DDD) %>%
      tidyr::complete(MEDICATION_NAME,
                      ADM_START_MONTH=seq(min(ADM_START_MONTH), max(ADM_START_MONTH), by="month"),
                      fill = list(DDD = 0)) %>%
      left_join(antimicrob_tbl_codes, by = "MEDICATION_NAME")
  })
    
  
  output$ddd_plot_month <- renderPlotly({ 
    
  max_DDD <- max(DDD_month_tbl()$DDD)*1.1
  
  plot_month_ddd <- DDD_month_tbl() %>%
    ggplot(aes(x = ADM_START_MONTH,
               y = DDD,
               color = MEDICATION_NAME)) +
    geom_point(aes(text = paste0(str_to_title(format(ADM_START_MONTH, format = "%b, %Y")),
                                 '<br>',MEDICATION_NAME,
                                 '<br>DDD: ',round(DDD,2)
    ))) +
    geom_line() +
    theme_minimal() +
    theme(legend.position = "none") +
    xlab("") +
    ylab("g/1000 pacientes-dia") +
   # labs(title = "DDD (Dose Diária Definida)") +
    scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
    scale_y_continuous(limits = c(0,max_DDD), expand = c(0, 0)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          axis.line = element_line(),
          plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches"))
  
  ggplotly(plot_month_ddd, tooltip = "text")
  
  })
  
  output$ddd_month_dygraph <- renderDygraph({
    
    DDD_month_dates <- as.Date(sort(unique(DDD_month_tbl()$ADM_START_MONTH)))
    
    DDD_month_xts = DDD_month_tbl() %>%
      pivot_wider(names_from = MEDICATION_NAME, values_from = DDD, id_cols = ADM_START_MONTH) %>%
      arrange(ADM_START_MONTH) %>%
      select(-ADM_START_MONTH) %>%
      xts(order.by = DDD_month_dates)
    
    first_row_xts <- DDD_month_xts[1,]
    first_row_xts[1,] <- NA
    last_row_xts <- first_row_xts
    
    index(first_row_xts) <- min(DDD_month_dates) - 5
    index(last_row_xts) <- max(DDD_month_dates) + 5
    
    DDD_month_xts <- rbind(first_row_xts, DDD_month_xts, last_row_xts)
    
    num_series <- ncol(DDD_month_xts)
    colors <- brewer.pal(min(num_series, 8), "Dark2") # Paleta para até 8 categorias
    if (num_series > 8) {
      colors <- c(colors, rainbow(num_series - 8)) # Adiciona mais cores se necessário
    }
    
    # Criar o gráfico e adicionar dySeries dinamicamente
    dy <- dygraph(DDD_month_xts)
    
    for (i in seq_len(num_series)) {
      dy <- dy %>% dySeries(names(DDD_month_xts)[i], 
                            label = colnames(DDD_month_xts)[i], 
                            color = colors[i],
                            strokeWidth = 2)
    }
    dy |>
      dyRangeSelector() |> # seletor inferior de tempo
      #dyBarChart() |> # tipo de gráfico
      #  dySeries('value', label = 'Total de Solicitações') |> # label do mouse
      dyCrosshair(direction = 'vertical') |> # linhas verticais pra acompanhar o mouse
      dyAxis('y', label = 'g/1000 pacientes-dia') |> # label y
      dyAxis('x', label = "", drawGrid = TRUE, valueRange =  range(DDD_month_dates) + c(-30,30)) |> # retirando as linhas verticais do gráfico
      dyOptions(useDataTimezone = TRUE, rightGap = 10, # colocando estilo mês/dia/ano quando passa o mouse
                drawPoints = TRUE, pointSize = 4,
                fillGraph  = F) |> 
      dyLegend(show = "always", hideOnMouseOut = FALSE,
               labelsDiv = "ddd_month_dygraph_legendDivID",
               labelsSeparateLines = TRUE#,
               #width = "240px"
               ) |>
      dyUnzoom() %>% # dando opção de desfazer o zoom
      dyHighlight(highlightCircleSize = 5, 
                #  highlightSeriesBackgroundAlpha = 0.5,
                  highlightSeriesOpts = list(strokeWidth = 4),
                  hideOnMouseOut = FALSE)
    
    
  })
  
  # Render the appropriate plot based on the selected option
  output$ddd_month_plot_dynamic <- renderUI({
    if (input$ddd_plot_month_button == "Opção A") {
      plotlyOutput("ddd_plot_month")
    } else {
      fluidRow(
        tags$head(
          tags$style(HTML("
      #ddd_month_dygraph_legendDivID {
        background-color: white;
        opacity: 1;
        padding: 10px;
        border: 1px solid #ccc;
        border-radius: 5px;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
        overflow-y: auto;
        height: 100%; /* Ensure it spans the full height of the grid */
      }
    "))
        ),
        column(9, dygraphOutput("ddd_month_dygraph", width = "100%", height = "380px")), 
        column(3, div(id = "ddd_month_dygraph_legendDivID"))
      )
      
    }
  })
  
  output$ddd_datatable <- renderDataTable({
    
    DDD_month_tbl_wide = DDD_month_tbl() %>%
      mutate(MONTH_YEAR = str_to_title(format(ADM_START_MONTH, format = "%b %Y"))) %>%
      select(MEDICATION_NAME, MEDICATION_CLASS_CODE, MONTH_YEAR, DDD) %>%
      pivot_wider(names_from = MONTH_YEAR, values_from = DDD, id_cols = c(MEDICATION_NAME, MEDICATION_CLASS_CODE)) %>%
      rename(Antimicrobiano = MEDICATION_NAME,
             'Código ATC' = MEDICATION_CLASS_CODE) %>%
      mutate(across(where(is.numeric), ~ round(.x, digits = 1)))

    datatable(DDD_month_tbl_wide, rownames = FALSE,
              options = list(pageLength = 8,
                             dom = 'tpi',
                             language = list(
                               info = 'Mostrando registros _START_ - _END_ de _TOTAL_',
                               paginate = list(previous = 'Anterior', `next` = 'Próximo')
                             ) ))
  })
  
  # DOT
  
  DOT_tbl <- reactive({
    
    antimicrob_tbl_final1 <- antimicrob_tbl_final %>%
      dplyr::filter(ADM_START_DATE >= filter_mic$date_range[1] &
                      ADM_START_DATE <= filter_mic$date_range[2])
    
    if (is.null(filter_mic$setor_mic)) {
      antimicrob_tbl_final2 <- antimicrob_tbl_final1
    } else {
      antimicrob_tbl_final2 <- antimicrob_tbl_final1 %>%
        dplyr::filter(DESCRIPTION %in% filter_mic$setor_mic)
    }
    return(antimicrob_tbl_final2)
  })
  
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
      antimicrob_tbl_final4 <- antimicrob_tbl_final3 %>%
        # group_by(ADMISSION_ID) %>%
        # mutate(dias_uso_atm_total = sum(as.numeric(THERAPY_END_DATE - THERAPY_START_DATE))) %>%
        # ungroup() %>%
        mutate(dias_uso_atm = as.numeric(THERAPY_END_DATE - THERAPY_START_DATE) ) %>%
        group_by(MEDICATION_NAME, MEDICATION_CLASS_CODE) %>%
        reframe(dias_uso_atm_sum = sum(dias_uso_atm),
                P = sum(ADMISSION_DURATION)) %>%
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
        # group_by(ADMISSION_ID) %>%
        # mutate(dias_uso_atm_total = sum(as.numeric(THERAPY_END_DATE - THERAPY_START_DATE))) %>%
        # ungroup() %>%
        mutate(dias_uso_atm = as.numeric(THERAPY_END_DATE - THERAPY_START_DATE) ) %>%
        
        group_by(MEDICATION_NAME, MEDICATION_CLASS_CODE) %>%
        reframe(dias_uso_atm_sum = sum(dias_uso_atm),
                P = sum(ADMISSION_DURATION)) %>%
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
  
  # Calcular DOT por medicamento e mês
  DOT_month_tbl <- reactive({ 
    
    # Filtro por data
    atm_tbl_dot_filter_date <- antimicrob_tbl_final %>%
      dplyr::filter(ADM_START_DATE >= filter_mic$date_range[1] &
                      ADM_START_DATE <= filter_mic$date_range[2])
    
    # Filtro por setor
    if (is.null(filter_mic$setor_mic)) {
      atm_tbl_dot_filter_sector <- atm_tbl_dot_filter_date
    } else {
      atm_tbl_dot_filter_sector <- atm_tbl_dot_filter_date %>%
        dplyr::filter(DESCRIPTION %in% filter_mic$setor_mic)
    }
    
    # Filtro por medicamento
    if (is.null(filter_mic$medicamento_mic)) {
      atm_tbl_dot_filter_med <- atm_tbl_dot_filter_sector
    } else {
      atm_tbl_dot_filter_med <- atm_tbl_dot_filter_sector %>%
        dplyr::filter(MEDICATION_NAME %in% filter_mic$medicamento_mic)
    }
    
    atm_tbl_dot_filter_med %>%
      # group_by(ADMISSION_ID) %>%
      # mutate(dias_uso_atm_total = sum(as.numeric(THERAPY_END_DATE - THERAPY_START_DATE))) %>%
      # ungroup() %>%
      mutate(dias_uso_atm = as.numeric(THERAPY_END_DATE - THERAPY_START_DATE) ) %>%
      group_by(MEDICATION_NAME, ADM_START_MONTH) %>%
      reframe(dias_uso_atm_sum = sum(dias_uso_atm),
              P = sum(ADMISSION_DURATION)) %>%
      ungroup() %>%
      mutate(DOT = 1000*dias_uso_atm_sum/P) %>%
      # Filtrar para deixar nomes de medicamentos únicos (verificar como será o tratamento com os dados reais)
      group_by(MEDICATION_NAME, ADM_START_MONTH) %>%
      mutate(n_med_name = 1:n()) %>%
      ungroup() %>%
      filter(n_med_name == 1) %>%
      select(MEDICATION_NAME, ADM_START_MONTH, DOT) %>%
      tidyr::complete(MEDICATION_NAME,
                      ADM_START_MONTH=seq(min(ADM_START_MONTH), max(ADM_START_MONTH), by="month"),
                      fill = list(DOT = 0)) %>%
      left_join(antimicrob_tbl_codes, by = "MEDICATION_NAME")
  })
  
  output$dot_plot_month <- renderPlotly({ 
    
    max_DOT <- max(DOT_month_tbl()$DOT)*1.1
    
    plot_month_dot <- DOT_month_tbl() %>%
      ggplot(aes(x = ADM_START_MONTH,
                 y = DOT,
                 color = MEDICATION_NAME)) +
      geom_point(aes(text = paste0(str_to_title(format(ADM_START_MONTH, format = "%b, %Y")),
                                   '<br>',MEDICATION_NAME,
                                   '<br>DOT: ',round(DOT,2)
      ))) +
      geom_line() +
      theme_minimal() +
      theme(legend.position = "none") +
      xlab("") +
      ylab("g/1000 pacientes-dia") +
      # labs(title = "DOT (Dose Diária Definida)") +
      scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
      scale_y_continuous(limits = c(0,max_DOT), expand = c(0, 0)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            axis.line = element_line(),
            plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches"))
    
    ggplotly(plot_month_dot, tooltip = "text")
  })
 
  
  output$dot_month_dygraph <- renderDygraph({
    
    DOT_month_dates <- as.Date(sort(unique(DOT_month_tbl()$ADM_START_MONTH)))
    
    DOT_month_xts = DOT_month_tbl() %>%
      pivot_wider(names_from = MEDICATION_NAME, values_from = DOT, id_cols = ADM_START_MONTH) %>%
      arrange(ADM_START_MONTH) %>%
      select(-ADM_START_MONTH) %>%
      xts(order.by = DOT_month_dates)
    
    first_row_xts <- DOT_month_xts[1,]
    first_row_xts[1,] <- NA
    last_row_xts <- first_row_xts
    
    index(first_row_xts) <- min(DOT_month_dates) - 5
    index(last_row_xts) <- max(DOT_month_dates) + 5
    
    DOT_month_xts <- rbind(first_row_xts, DOT_month_xts, last_row_xts)
    
    num_series <- ncol(DOT_month_xts)
    colors <- brewer.pal(min(num_series, 8), "Dark2") # Paleta para até 8 categorias
    if (num_series > 8) {
      colors <- c(colors, rainbow(num_series - 8)) # Adiciona mais cores se necessário
    }
    
    # Criar o gráfico e adicionar dySeries dinamicamente
    dy <- dygraph(DOT_month_xts)
    
    for (i in seq_len(num_series)) {
      dy <- dy %>% dySeries(names(DOT_month_xts)[i], 
                            label = colnames(DOT_month_xts)[i], 
                            color = colors[i],
                            strokeWidth = 2)
    }
    dy |>
      dyRangeSelector() |> # seletor inferior de tempo
      #dyBarChart() |> # tipo de gráfico
      #  dySeries('value', label = 'Total de Solicitações') |> # label do mouse
      dyCrosshair(direction = 'vertical') |> # linhas verticais pra acompanhar o mouse
      dyAxis('y', label = 'dia/1000 pacientes-dia') |> # label y
      dyAxis('x', label = "", drawGrid = TRUE) |> # retirando as linhas verticais do gráfico
      dyOptions(useDataTimezone = TRUE, rightGap = 10, # colocando estilo mês/dia/ano quando passa o mouse
                drawPoints = TRUE, pointSize = 4,
                fillGraph  = F) |> 
      dyLegend(show = "always", hideOnMouseOut = FALSE,
               labelsDiv = "dot_month_dygraph_legendDivID",
               labelsSeparateLines = TRUE#,
               #width = "240px"
      ) |>
      dyUnzoom() %>% # dando opção de desfazer o zoom
      dyHighlight(highlightCircleSize = 5, 
                  #  highlightSeriesBackgroundAlpha = 0.5,
                  highlightSeriesOpts = list(strokeWidth = 4),
                  hideOnMouseOut = FALSE)
    
    
  })
  
  # Render the appropriate plot based on the selected option
  output$dot_month_plot_dynamic <- renderUI({
    if (input$dot_plot_month_button == "Opção A") {
      plotlyOutput("dot_plot_month")
    } else {
      fluidRow(
        tags$head(
          tags$style(HTML("
      #dot_month_dygraph_legendDivID {
        background-color: white;
        opacity: 1;
        padding: 10px;
        border: 1px solid #ccc;
        border-radius: 5px;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
        overflow-y: auto;
        height: 100%; /* Ensure it spans the full height of the grid */
      }
    "))
        ),
        column(9, dygraphOutput("dot_month_dygraph", width = "100%", height = "380px")), 
        column(3, div(id = "dot_month_dygraph_legendDivID"))
      )
      
    }
  })
  
  output$dot_datatable <- renderDataTable({
    
    DOT_month_tbl_wide = DOT_month_tbl() %>%
      mutate(MONTH_YEAR = str_to_title(format(ADM_START_MONTH, format = "%b %Y"))) %>%
      select(MEDICATION_NAME, MEDICATION_CLASS_CODE, MONTH_YEAR, DOT) %>%
      pivot_wider(names_from = MONTH_YEAR, values_from = DOT, id_cols = c(MEDICATION_NAME, MEDICATION_CLASS_CODE)) %>%
      rename(Antimicrobiano = MEDICATION_NAME,
             'Código ATC' = MEDICATION_CLASS_CODE) %>%
      mutate(across(where(is.numeric), ~ round(.x, digits = 1)))
    
    datatable(DOT_month_tbl_wide, rownames = FALSE,
              options = list(pageLength = 8,
                             dom = 'tpi',
                             language = list(
                               info = 'Mostrando registros _START_ - _END_ de _TOTAL_',
                               paginate = list(previous = 'Anterior', `next` = 'Próximo')
                             ) ))
  })
  
  ### LOT Length of Treatment
  LOT_tbl <- reactive({
    
    antimicrob_tbl_final %>%
      dplyr::filter(ADM_START_DATE >= filter_mic$date_range[1] &
                      ADM_START_DATE <= filter_mic$date_range[2])
    
  })

  output$lot_plot <- renderPlotly({
    
    # Opção A: Filtrar medicamentos
    
    if(input$lot_plot_button == "Ver somente itens selecionados") {
      
      if (is.null(filter_mic$setor_mic)) {
        atb_lot_tbl_filter_sector <- LOT_tbl()
      } else {
        atb_lot_tbl_filter_sector <- LOT_tbl() %>%
          dplyr::filter(DESCRIPTION %in% filter_mic$setor_mic)
      }
      
      # Calcular LOT por setor hospitalar
      atb_lot_tbl <- atb_lot_tbl_filter_sector %>%
        group_by(ADMISSION_ID) %>%
        mutate(THERAPY_DURATION = as.numeric(max(THERAPY_END_DATE, na.rm = T) - min(THERAPY_START_DATE, na.rm = T))) %>%
        ungroup() %>%
        group_by(DESCRIPTION) %>%
        reframe(LOT = sum(THERAPY_DURATION)/sum(ADMISSION_DURATION)*1000) %>%
        ungroup()
      
      plot_lot <- atb_lot_tbl %>%
        arrange(desc(LOT)) %>%
        ggplot(aes(y = fct_reorder(DESCRIPTION, LOT), x = LOT,
                   labels = "",
                   text = paste0(DESCRIPTION,
                                 '<br>LOT: ', round(LOT,2)))) +
        geom_col(fill = "#1F77B4") +
        theme_minimal() +
        ylab("") +
        xlab("dias/1000 pacientes-dia") +
        labs(title = "LOT (Length of Treatment)") +
        theme(axis.line.y = element_line()) +
        scale_x_continuous(expand = c(0, 0))
      
      ggplotly(plot_lot, tooltip = "text")
    } else { 
      #if(input$lot_plot_button == "Destacar itens")
      # Opção B: Highlight nos medicamentos selecionados
      
      # Calcular LOT por setor hospitalar
      atb_lot_tblb <- LOT_tbl() %>%
        group_by(ADMISSION_ID) %>%
        mutate(THERAPY_DURATION = as.numeric(max(THERAPY_END_DATE, na.rm = T) - min(THERAPY_START_DATE, na.rm = T))) %>%
        ungroup() %>%
        group_by(DESCRIPTION) %>%
        reframe(LOT = sum(THERAPY_DURATION)/sum(ADMISSION_DURATION)*1000) %>%
        ungroup() %>%
        mutate(highlight_medication = ifelse(DESCRIPTION %in% filter_mic$setor_mic, T, F))
      
      plot_lot2 <- atb_lot_tblb %>%
        arrange(desc(LOT)) %>%
        ggplot(aes(y = fct_reorder(DESCRIPTION, LOT), x = LOT,
                   fill = highlight_medication,
                   labels = "",
                   text = paste0(DESCRIPTION,
                                 '<br>LOT: ', round(LOT,2)))) +
        geom_col() +
        scale_fill_manual(values = c("#1F77B4", "orange")) +
        theme_minimal() +
        ylab("") +
        xlab("dias/1000 pacientes-dia") +
        labs(title = "LOT (Length of Treatment)") +
        theme(axis.line.y = element_line(),
              legend.position = "none") +
        scale_x_continuous(expand = c(0, 0))
      
      ggplotly(plot_lot2, tooltip = "text")
    }
  })
  
  # Calcular LOT por mês
  LOT_month_tbl <- reactive({ 
    
    # Filtro por data
    atm_tbl_lot_filter_date <- antimicrob_tbl_final %>%
      dplyr::filter(ADM_START_DATE >= filter_mic$date_range[1] &
                      ADM_START_DATE <= filter_mic$date_range[2])
    
    # Filtro por setor
    if (is.null(filter_mic$setor_mic)) {
      atm_tbl_lot_filter_sector <- atm_tbl_lot_filter_date
    } else {
      atm_tbl_lot_filter_sector <- atm_tbl_lot_filter_date %>%
        dplyr::filter(DESCRIPTION %in% filter_mic$setor_mic)
    }
    
    atm_tbl_lot_filter_sector %>%
      group_by(ADMISSION_ID) %>%
      mutate(THERAPY_DURATION = as.numeric(max(THERAPY_END_DATE, na.rm = T) - min(THERAPY_START_DATE, na.rm = T))) %>%
      ungroup() %>%
      group_by(DESCRIPTION, ADM_START_MONTH) %>%
      reframe(LOT = sum(THERAPY_DURATION)/sum(ADMISSION_DURATION)*1000) %>%
      ungroup() %>%
      select(DESCRIPTION, ADM_START_MONTH, LOT) %>%
      tidyr::complete(DESCRIPTION,
                      ADM_START_MONTH = seq(min(ADM_START_MONTH), max(ADM_START_MONTH), by="month"),
                      fill = list(LOT = 0))
  })
  
  output$lot_plot_month <- renderPlotly({ 
    
    max_LOT <- max(LOT_month_tbl()$LOT)*1.1
    
    plot_month_lot <- LOT_month_tbl() %>%
      ggplot(aes(x = ADM_START_MONTH,
                 y = LOT,
                 color = DESCRIPTION)) +
      geom_point(aes(text = paste0(str_to_title(format(ADM_START_MONTH, format = "%b, %Y")),
                                   '<br>',DESCRIPTION,
                                   '<br>LOT: ',round(LOT,2)
      ))) +
      geom_line() +
      theme_minimal() +
      theme(legend.position = "none") +
      xlab("") +
      ylab("dias/1000 pacientes-dia") +
      scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
      scale_y_continuous(limits = c(0,max_LOT), expand = c(0, 0)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            axis.line = element_line(),
            plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches"))
    
    ggplotly(plot_month_lot, tooltip = "text")
  })
  
  
  output$lot_month_dygraph <- renderDygraph({
    
    LOT_month_dates <- as.Date(sort(unique(LOT_month_tbl()$ADM_START_MONTH)))
    
    LOT_month_xts = LOT_month_tbl() %>%
      pivot_wider(names_from = DESCRIPTION, values_from = LOT, id_cols = ADM_START_MONTH) %>%
      arrange(ADM_START_MONTH) %>%
      select(-ADM_START_MONTH) %>%
      xts(order.by = LOT_month_dates)
    
    first_row_xts <- LOT_month_xts[1,]
    first_row_xts[1,] <- NA
    last_row_xts <- first_row_xts
    
    index(first_row_xts) <- min(LOT_month_dates) - 5
    index(last_row_xts) <- max(LOT_month_dates) + 5
    
    LOT_month_xts <- rbind(first_row_xts, LOT_month_xts, last_row_xts)
    
    num_series <- ncol(LOT_month_xts)
    colors <- brewer.pal(min(num_series, 8), "Dark2") # Paleta para até 8 categorias
    if (num_series > 8) {
      colors <- c(colors, rainbow(num_series - 8)) # Adiciona mais cores se necessário
    }
    
    # Criar o gráfico e adicionar dySeries dinamicamente
    dy <- dygraph(LOT_month_xts)
    
    for (i in seq_len(num_series)) {
      dy <- dy %>% dySeries(names(LOT_month_xts)[i], 
                            label = colnames(LOT_month_xts)[i], 
                            color = colors[i],
                            strokeWidth = 2)
    }
    dy |>
      dyRangeSelector() |> # seletor inferior de tempo
      #dyBarChart() |> # tipo de gráfico
      #  dySeries('value', label = 'Total de Solicitações') |> # label do mouse
      dyCrosshair(direction = 'vertical') |> # linhas verticais pra acompanhar o mouse
      dyAxis('y', label = 'dias/1000 pacientes-dia') |> # label y
      dyAxis('x', label = "", drawGrid = TRUE) |> # retirando as linhas verticais do gráfico
      dyOptions(useDataTimezone = TRUE, rightGap = 10, # colocando estilo mês/dia/ano quando passa o mouse
                drawPoints = TRUE, pointSize = 4,
                fillGraph  = F) |> 
      dyLegend(show = "always", hideOnMouseOut = FALSE,
               labelsDiv = "lot_month_dygraph_legendDivID",
               labelsSeparateLines = TRUE#,
               #width = "240px"
      ) |>
      dyUnzoom() %>% # dando opção de desfazer o zoom
      dyHighlight(highlightCircleSize = 5, 
                  #  highlightSeriesBackgroundAlpha = 0.5,
                  highlightSeriesOpts = list(strokeWidth = 4),
                  hideOnMouseOut = FALSE)
  })
  
  # Render the appropriate plot based on the selected option
  output$lot_month_plot_dynamic <- renderUI({
    if (input$lot_plot_month_button == "Opção A") {
      plotlyOutput("lot_plot_month")
    } else {
      fluidRow(
        tags$head(
          tags$style(HTML("
      #lot_month_dygraph_legendDivID {
        background-color: white;
        opacity: 1;
        padding: 10px;
        border: 1px solid #ccc;
        border-radius: 5px;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
        overflow-y: auto;
        height: 100%; /* Ensure it spans the full height of the grid */
      }
    "))
        ),
        column(9, dygraphOutput("lot_month_dygraph", width = "100%", height = "380px")), 
        column(3, div(id = "lot_month_dygraph_legendDivID"))
      )
      
    }
  })
  
  output$lot_datatable <- renderDataTable({
    
    LOT_month_tbl_wide = LOT_month_tbl() %>%
      mutate(MONTH_YEAR = str_to_title(format(ADM_START_MONTH, format = "%b %Y"))) %>%
      select(DESCRIPTION, MONTH_YEAR, LOT) %>%
      pivot_wider(names_from = MONTH_YEAR, values_from = LOT, id_cols = c(DESCRIPTION)) %>%
      rename('Setor Hospitalar' = DESCRIPTION) %>%
      mutate(across(where(is.numeric), ~ round(.x, digits = 1)))
    
    datatable(LOT_month_tbl_wide, rownames = FALSE,
              options = list(pageLength = 8,
                             dom = 'tpi',
                             language = list(
                               info = 'Mostrando registros _START_ - _END_ de _TOTAL_',
                               paginate = list(previous = 'Anterior', `next` = 'Próximo')
                             ) ))
  })
  
# PRD 3
  # Reactive values to store filters
  filter_aceitabilidade <- reactiveValues(
    aceitabilidade_date = aceitabilidade_date_range,
    aceitabilidade_setor = NULL,
    aceitabilidade_medico_id = NULL,
    aceitabilidade_user_id = NULL,
    aceitabilidade_adherence = "all",
    aceitabilidade_type = "sugestao"
  )
  
  # Update reactive values when the button is pressed
  observeEvent(input$filter_btn_acpt, {
    filter_aceitabilidade$aceitabilidade_date <- input$aceitabilidade_date
    filter_aceitabilidade$aceitabilidade_setor <- input$aceitabilidade_setor
    filter_aceitabilidade$aceitabilidade_medico_id <- input$aceitabilidade_medico_id
    filter_aceitabilidade$aceitabilidade_user_id <- input$aceitabilidade_user_id
    filter_aceitabilidade$aceitabilidade_adherence <- input$aceitabilidade_adherence
    filter_aceitabilidade$aceitabilidade_type <- input$aceitabilidade_type
  })
  
  aceitabilidade_tbl_filtered_prev <- reactive({
    
    # Filtro por data
    acpt_tbl_filter_date <- aceitabilidade_tbl %>%
      dplyr::filter(ALERT_DATE >= filter_aceitabilidade$aceitabilidade_date[1] &
                      ALERT_DATE <= filter_aceitabilidade$aceitabilidade_date[2])
    
    # Filtro por setor
    if (is.null(filter_aceitabilidade$aceitabilidade_setor)) {
      acpt_tbl_filter_sector <- acpt_tbl_filter_date
    } else {
      acpt_tbl_filter_sector <- acpt_tbl_filter_date %>%
        dplyr::filter(DESCRIPTION %in% filter_aceitabilidade$aceitabilidade_setor)
    }
    
    # Filtro por médico prescritor
    if (is.null(filter_aceitabilidade$aceitabilidade_medico_id)) {
      acpt_tbl_filter_pract <- acpt_tbl_filter_sector
    } else {
      acpt_tbl_filter_pract <- acpt_tbl_filter_sector %>%
        dplyr::filter(PRACTITIONER_ID %in% filter_aceitabilidade$aceitabilidade_medico_id)
    }
    
    # Filtro por usuário/farmacêutico
    if (is.null(filter_aceitabilidade$aceitabilidade_user_id)) {
      acpt_tbl_filter_user <- acpt_tbl_filter_pract
    } else {
      acpt_tbl_filter_user <- acpt_tbl_filter_pract %>%
        dplyr::filter(USER_ID %in% filter_aceitabilidade$aceitabilidade_user_id)
    } 
    
    return(acpt_tbl_filter_user)
  })
  
  aceitabilidade_tbl_filtered_max_week <- reactive({  
    
    aceitabilidade_tbl_filtered_prev() %>%
      count(ALERT_WEEK) %>%
      summarise(max_value = max(n, na.rm = T))
  })

  aceitabilidade_tbl_filtered_max_month <- reactive({  
    
    aceitabilidade_tbl_filtered_prev() %>%
      count(ALERT_MONTH) %>%
      summarise(max_value = max(n, na.rm = T))
  })
  
  tm_aceitabilidade_tbl_filtered_max_week <- reactive({
    
    # Valor máximo do tempo médio de resposta para os filtros selecionados, exceto ALERT_ADHERENCE
    aceitabilidade_tbl_filtered_prev() %>%
      mutate(tempo_resposta = round(as.numeric(difftime(ALERT_ADHERENCE_DATE, ALERT_DATE, units  = "hours")),2)) %>%
      group_by(ALERT_WEEK, ALERT_ADHERENCE) %>%
      reframe(tempo_resposta_medio = mean(tempo_resposta, na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(ALERT_WEEK) %>%
      reframe(tm_sum = sum(tempo_resposta_medio, na.rm = T)) %>%
      ungroup() %>%
      reframe(max_value_t_acpt_mean = max(tm_sum, na.rm = TRUE))
    
  })
  
  tm_aceitabilidade_tbl_filtered_max_month <- reactive({
    
    # Valor máximo do tempo médio de resposta para os filtros selecionados, exceto ALERT_ADHERENCE
    aceitabilidade_tbl_filtered_prev() %>%
      mutate(tempo_resposta = round(as.numeric(difftime(ALERT_ADHERENCE_DATE, ALERT_DATE, units  = "hours")),2)) %>%
      group_by(ALERT_MONTH, ALERT_ADHERENCE) %>%
      reframe(tempo_resposta_medio = mean(tempo_resposta, na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(ALERT_MONTH) %>%
      reframe(tm_sum = sum(tempo_resposta_medio, na.rm = T)) %>%
      ungroup() %>%
      reframe(max_value_t_acpt_mean = max(tm_sum, na.rm = TRUE))
    
  })
  
  aceitabilidade_tbl_filtered <- reactive({
    
    # Filtro por resposta de aceite ou não à intervenção
     if (filter_aceitabilidade$aceitabilidade_adherence == "all") {
       acpt_tbl_filter_adherence <- aceitabilidade_tbl_filtered_prev()
    } else {
      acpt_tbl_filter_adherence <- aceitabilidade_tbl_filtered_prev() %>%
        dplyr::filter(ALERT_ADHERENCE == filter_aceitabilidade$aceitabilidade_adherence)
    }
    
    return(acpt_tbl_filter_adherence)
  })
  
  aceitabilidade_implem_tbl_filtered_prev <- reactive({
    
    # Filtro por data
    acpt_tbl_filter_date_i <- aceitabilidade_tbl %>%
     filter(ALERT_ADHERENCE) %>%
     dplyr::filter(ALERT_IMPLEMENTATION_DATE >= filter_aceitabilidade$aceitabilidade_date[1] &
                    ALERT_IMPLEMENTATION_DATE <= filter_aceitabilidade$aceitabilidade_date[2])
    
    # Filtro por setor
    if (is.null(filter_aceitabilidade$aceitabilidade_setor)) {
      acpt_tbl_filter_sector_i <- acpt_tbl_filter_date_i
    } else {
      acpt_tbl_filter_sector_i <- acpt_tbl_filter_date_i %>%
        dplyr::filter(DESCRIPTION %in% filter_aceitabilidade$aceitabilidade_setor)
    }
    
    # Filtro por médico prescritor
    if (is.null(filter_aceitabilidade$aceitabilidade_medico_id)) {
      acpt_tbl_filter_pract_i <- acpt_tbl_filter_sector_i
    } else {
      acpt_tbl_filter_pract_i <- acpt_tbl_filter_sector_i %>%
        dplyr::filter(PRACTITIONER_ID %in% filter_aceitabilidade$aceitabilidade_medico_id)
    }
    
    # Filtro por usuário/farmacêutico
    if (is.null(filter_aceitabilidade$aceitabilidade_user_id)) {
      acpt_tbl_filter_user_i <- acpt_tbl_filter_pract_i
    } else {
      acpt_tbl_filter_user_i <- acpt_tbl_filter_pract_i %>%
        dplyr::filter(USER_ID %in% filter_aceitabilidade$aceitabilidade_user_id)
    } 
  })
  
  aceitabilidade_implem_tbl_filtered_max_week <- reactive({  
    
    aceitabilidade_implem_tbl_filtered_prev() %>%
      count(IMPLEMENTATION_WEEK) %>%
      summarise(max_value = max(n, na.rm = T))
  })
  
  aceitabilidade_implem_tbl_filtered_max_month <- reactive({  
    
    aceitabilidade_implem_tbl_filtered_prev() %>%
      count(IMPLEMENTATION_MONTH) %>%
      summarise(max_value = max(n, na.rm = T))
  })
  
  
  tm_aceitabilidade_implem_tbl_filtered_max_week <- reactive({
    
    if(nrow(aceitabilidade_implem_tbl_filtered_prev())==0) {
      data.frame(max_value_t_acpt_mean = 0)
    } else {
    # Valor máximo do tempo médio de resposta para os filtros selecionados, exceto ALERT_ADHERENCE
    aceitabilidade_implem_tbl_filtered_prev() %>%
      mutate(tempo_resposta = round(as.numeric(difftime(ALERT_ADHERENCE_DATE, ALERT_DATE, units  = "hours")),2)) %>%
      group_by(ALERT_WEEK, ALERT_ADHERENCE) %>%
      reframe(tempo_resposta_medio = mean(tempo_resposta, na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(ALERT_WEEK) %>%
      reframe(tm_sum = sum(tempo_resposta_medio, na.rm = T)) %>%
      ungroup() %>%
      reframe(max_value_t_acpt_mean = max(tm_sum, na.rm = TRUE))
    }
  })
  
  tm_aceitabilidade_implem_tbl_filtered_max_month <- reactive({
    
    if(nrow(aceitabilidade_implem_tbl_filtered_prev())==0) {
      data.frame(max_value_t_acpt_mean = 0)
    } else {
    # Valor máximo do tempo médio de resposta para os filtros selecionados, exceto ALERT_ADHERENCE
    aceitabilidade_implem_tbl_filtered_prev() %>%
      mutate(tempo_resposta = round(as.numeric(difftime(ALERT_ADHERENCE_DATE, ALERT_DATE, units  = "hours")),2)) %>%
      group_by(ALERT_MONTH, ALERT_ADHERENCE) %>%
      reframe(tempo_resposta_medio = mean(tempo_resposta, na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(ALERT_MONTH) %>%
      reframe(tm_sum = sum(tempo_resposta_medio, na.rm = T)) %>%
      ungroup() %>%
      reframe(max_value_t_acpt_mean = max(tm_sum, na.rm = TRUE))
    }
  })
  
  aceitabilidade_implem_tbl_filtered <- reactive({      
    # Filtro por resposta de aceite ou não à intervenção
    if (filter_aceitabilidade$aceitabilidade_adherence == "all") {
      acpt_tbl_filter_adherence_i <- aceitabilidade_implem_tbl_filtered_prev()
    } else {
      acpt_tbl_filter_adherence_i <- aceitabilidade_implem_tbl_filtered_prev() %>%
        dplyr::filter(IMPLEMENTATION_ADHERENCE == filter_aceitabilidade$aceitabilidade_adherence)
    }
    
    return(acpt_tbl_filter_adherence_i)
    
  })

  output$aceitabilidade_sugest_pie <- renderPlotly({
    
    if(nrow(aceitabilidade_tbl_filtered()) == 0) { 
      
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
        
    aceitabilidade_count_tbl <- aceitabilidade_tbl_filtered() %>%
      count(ALERT_ADHERENCE) %>%
      mutate(p = n/sum(n)) %>%
      mutate(ALERT_ADHERENCE = factor(ALERT_ADHERENCE, levels = c(T, F),
                                      labels = c("Aceitas", "Rejeitadas"),
                                      ordered = TRUE))
  
    n_acceptance_value <- aceitabilidade_count_tbl[aceitabilidade_count_tbl$ALERT_ADHERENCE == "Aceitas", "p"]
    
    if(nrow(n_acceptance_value)==0) {n_acceptance_value <- 0}
    
    p_aceite <- aceitabilidade_count_tbl %>% filter(ALERT_ADHERENCE == "Aceitas") %>% select(p)
    
    pie_color = c('#AA1D30','#1f77b4')
    
    if(nrow(p_aceite) > 0) {
      if(p_aceite$p == 1) {
      pie_color = '#1f77b4' }
    }
    
    plot_ly(data = aceitabilidade_count_tbl,
            type = "pie",
            hole = 0.7,
            rotation = 90,
            labels = ~ALERT_ADHERENCE,
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
            text = paste(round(n_acceptance_value *100,1), "%<br>das intervenções<br>aceitas"),
            showarrow = FALSE,
            font = list(size = 20, weight = "bold")
          )
        )
      )
      }
  })
  
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
  
  output$aceitabilidade_pie <- renderUI({
    if (filter_aceitabilidade$aceitabilidade_type == "sugestao") {
      plotlyOutput("aceitabilidade_sugest_pie")
    } else {
      plotlyOutput("aceitabilidade_implemen_pie")
    }
  })
  
  # Aceitabilidade ao longo do tempo (contagem)

  output$plot_aceitabilidade_n <- renderPlotly({

    y_lab = "Sugestões enviadas"
    plot_title_acpt = "Contagem de Adesão às Intervenções Sugeridas"
    
    if(input$plot_acpt_time_setting == "Mensal") {
      
      max_y_month <- ifelse(aceitabilidade_tbl_filtered_max_month()$max_value < 10, 10, aceitabilidade_tbl_filtered_max_month()$max_value)
      
      # x axis range for months
      if(as.numeric(diff(as.Date(filter_aceitabilidade$aceitabilidade_date))) < 150) {
        range_x_month <- c(as.Date(filter_aceitabilidade$aceitabilidade_date)[1], as.Date(filter_aceitabilidade$aceitabilidade_date)[1]+150)
      } else {
        range_x_month <- as.Date(filter_aceitabilidade$aceitabilidade_date)
      }
      
      range_x_month[1] <- floor_date(range_x_month[1], unit = "month")
      range_x_month[2] <- ceiling_date(range_x_month[2], unit = "month")
      
      if(nrow(aceitabilidade_tbl_filtered()) == 0) {
        
        aceitabilidade_count_tbl_month <- data.frame(ALERT_MONTH = as.Date(filter_aceitabilidade$aceitabilidade_date),
                                                     ALERT_ADHERENCE = NA,
                                                     n = 0)
      } else {
        aceitabilidade_count_tbl_month <- aceitabilidade_tbl_filtered() %>%
          count(ALERT_MONTH, ALERT_ADHERENCE) %>%
          mutate(ALERT_ADHERENCE = factor(ALERT_ADHERENCE,
                                          levels = c(T,F),
                                          labels = c("Aceitas","Rejeitadas"))) %>%
          tidyr::complete(ALERT_ADHERENCE,
                          ALERT_MONTH=seq(min(ALERT_MONTH), max(ALERT_MONTH), by="month"),
                          fill = list(n = 0))
      }
      
      label_color_tbl_acpt <- data.frame(label_name = c("Aceitas", "Rejeitadas"),
                                         color = c('#1f77b4','#AA1D30')) %>%
        filter(label_name %in% unique(aceitabilidade_count_tbl_month$ALERT_ADHERENCE))
      
      aceitabilidade_count_tbl_month <- aceitabilidade_count_tbl_month %>%
        mutate(ALERT_MONTH_PT = format(ALERT_MONTH, "%B %Y") %>%
                 tools::toTitleCase())
      
      # Create the plotly plot
      acpt_plotly <- plot_ly(
        data = aceitabilidade_count_tbl_month,
        x = ~ALERT_MONTH,
        y = ~n,
        type = "bar",
        color = ~ALERT_ADHERENCE,
        colors = setNames(label_color_tbl_acpt$color, label_color_tbl_acpt$label_name),
        hovertemplate = ~paste0(str_to_title(format(ALERT_MONTH, format = "%B %Y")), "<br>", ALERT_ADHERENCE,": ", n,"<extra></extra>")
      ) %>%
        layout(
          legend = list(x = 100, y = 0.5),
          barmode = 'stack',
          title = list(text = plot_title_acpt, x = 0.5), # Center the title
          xaxis = list(
            title = "",
            tickangle = 45,
            type = "date",
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = "M1",  # One month intervals
            tickformat = "%B %Y",  # Full month name and year
            range = range_x_month + c(-15,-1)
          ),
          yaxis = list(
            title = y_lab,
            range = c(0, max_y_month)
          ),
          margin = list(t = 50, r = 20, b = 120, l = 20), # Adjust plot margins
          legend = list(title = list(text = ""), orientation = "h")
        )  %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
      ### ELSE
    } else {
      
      # Configurações para gráfico por semana
      
      max_y_week <- ifelse(aceitabilidade_tbl_filtered_max_week()$max_value < 10, 10,aceitabilidade_tbl_filtered_max_week()$max_value)
      
      # x axis range for weeks
      if(as.numeric(diff(as.Date(filter_aceitabilidade$aceitabilidade_date))) < 60) {
        range_x_week <- c(as.Date(filter_aceitabilidade$aceitabilidade_date)[1], as.Date(filter_aceitabilidade$aceitabilidade_date)[1]+60)
      } else {
        range_x_week <- as.Date(filter_aceitabilidade$aceitabilidade_date)
      }
      range_x_week[1] <- floor_date(range_x_week[1], unit = "week")
      range_x_week[2] <- ceiling_date(range_x_week[2], unit = "week")
      
      x_axis_range <- as.numeric(diff(range(range_x_week)))
      x_tick_label_breaks <- case_when(x_axis_range <= 60 ~ 1,
                                       x_axis_range > 60 & x_axis_range <= 300 ~ 2,
                                       x_axis_range > 300 & x_axis_range <= 540 ~ 3,
                                       x_axis_range > 540 ~ 4)
      
      if(nrow(aceitabilidade_tbl_filtered()) == 0) {
        
        aceitabilidade_count_tbl_week <- data.frame(ALERT_WEEK = as.Date(filter_aceitabilidade$aceitabilidade_date),
                                                    ALERT_ADHERENCE = NA,
                                                    n = 0)
      } else {
        aceitabilidade_count_tbl_week<- aceitabilidade_tbl_filtered() %>%
          count(ALERT_WEEK, ALERT_ADHERENCE) %>%
          mutate(ALERT_ADHERENCE = factor(ALERT_ADHERENCE,
                                          levels = c(T,F),
                                          labels = c("Aceitas","Rejeitadas"))) %>%
          tidyr::complete(ALERT_ADHERENCE,
                          ALERT_WEEK=seq(min(ALERT_WEEK), max(ALERT_WEEK), by="week"),
                          fill = list(n = 0))
      }
      
      label_color_tbl_acpt <- data.frame(label_name = c("Aceitas", "Rejeitadas"),
                                         color = c('#1f77b4','#AA1D30')) %>%
        filter(label_name %in% unique(aceitabilidade_count_tbl_week$ALERT_ADHERENCE))
      
      aceitabilidade_count_tbl_week <- aceitabilidade_count_tbl_week %>%
        mutate(ALERT_WEEK_PT = format(ALERT_WEEK, "%d-%m-%y") %>%
                 tools::toTitleCase())
      
      # Create the plotly plot
      acpt_plotly <- plot_ly(
        data = aceitabilidade_count_tbl_week,
        x = ~ALERT_WEEK,
        y = ~n,
        type = "bar",
        color = ~ALERT_ADHERENCE,
        colors = setNames(label_color_tbl_acpt$color, label_color_tbl_acpt$label_name),
        hovertemplate = ~paste0("Semana: ", format(ALERT_WEEK_PT, format = "%d-%m-%y"), "<br>", ALERT_ADHERENCE,": ", n,"<extra></extra>")
      ) %>%
        layout(
          legend = list(x = 100, y = 0.5),
          barmode = 'stack',
          title = list(text = plot_title_acpt, x = 0.5), # Center the title
          xaxis = list(
            title = "",
            tickangle = 45,
            type = "date",
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = x_tick_label_breaks * 7 * 24 * 60 * 60 * 1000,  # semanas em ms
            tickformat = "%d %b %Y",  # Formato dos rótulos
            range = range_x_week + c(-5,-1)
          ),
          yaxis = list(
            title = y_lab,
            range = c(0, max_y_week)
          ),
          margin = list(t = 50, r = 20, b = 120, l = 20), # Adjust plot margins
          legend = list(title = list(text = ""), orientation = "h")
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
    } # else semana
    
    acpt_plotly
    
  })

  # Implementação ao longo do tempo Contagem  
  output$plot_aceitabilidade_implem_n <- renderPlotly({
    
    y_lab_i = "Sugestões aceitas"
    plot_title_acpt_i = "Contagem de Implementação das Intervenções Sugeridas"
    
    if(input$plot_acpt_time_setting == "Mensal") {
      
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
          xaxis = list(
            title = "",
            tickangle = 45,
            type = "date",
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = "M1",  # One month intervals
            tickformat = "%B %Y",  # Full month name and year
            range = range_x_month_i + c(-15,-1)
          ),
          yaxis = list(
            title = y_lab_i,
            range = c(0, max_y_month_i)
          ),
          margin = list(t = 50, r = 20, b = 120, l = 20), # Adjust plot margins
          legend = list(title = list(text = ""), orientation = "h")
        )  %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
      ### ELSE
    } else {
      
      # Configurações para gráfico por semana
      
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
          xaxis = list(
            title = "",
            tickangle = 45,
            type = "date",
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = x_tick_label_breaks_i * 7 * 24 * 60 * 60 * 1000,  # semanas em ms
            tickformat = "%d %b %Y",  # Formato dos rótulos
            range = range_x_week + c(-5,-1)
          ),
          yaxis = list(
            title = y_lab_i,
            range = c(0, max_y_week)
          ),
          margin = list(t = 50, r = 20, b = 120, l = 20), # Adjust plot margins
          legend = list(title = list(text = ""), orientation = "h")
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
    } # else semana
    
    acpt_plotly_i
    
  })
  
  output$plot_aceitabilidade_n_dynamic <- renderUI({
    if (filter_aceitabilidade$aceitabilidade_type == "sugestao") {
      plotlyOutput("plot_aceitabilidade_n")
    } else {
      plotlyOutput("plot_aceitabilidade_implem_n")
    }
  })
  
  # Aceitabilidade ao longo do tempo (Tempo médio de resposta)
  
  output$plot_aceitabilidade_tm <- renderPlotly({
    
    y_lab = "Horas"
    plot_title_acpt = "Intervalo Médio de Resposta às Intervenções Sugeridas"
    
    if(input$plot_acpt_tm_time_setting == "Mensal") {
      
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
        hovertemplate = ~paste0(str_to_title(format(ALERT_MONTH, format = "%B %Y")), "<br>", ALERT_ADHERENCE,": ", tempo_resposta_medio,"<extra></extra>")
      ) %>%
        layout(
          legend = list(x = 100, y = 0.5),
          barmode = 'stack',
          title = list(text = plot_title_acpt, x = 0.5), # Center the title
          xaxis = list(
            title = "",
            tickangle = 45,
            type = "date",
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = "M1",  # One month intervals
            tickformat = "%B %Y",  # Full month name and year
            range = range_x_month_tm + c(-15,-1)
          ),
          yaxis = list(
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
        hovertemplate = ~paste0("Semana: ", format(ALERT_WEEK_PT, format = "%d-%m-%y"), "<br>", ALERT_ADHERENCE,": ", tempo_resposta_medio,"<extra></extra>")
      ) %>%
        layout(
          legend = list(x = 100, y = 0.5),
          barmode = 'stack',
          title = list(text = plot_title_acpt, x = 0.5), # Center the title
          xaxis = list(
            title = "",
            tickangle = 45,
            type = "date",
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = x_tick_label_breaks_tm * 7 * 24 * 60 * 60 * 1000,  # semanas em ms
            tickformat = "%d %b %Y",  # Formato dos rótulos
            range = range_x_week_tm + c(-5,-1)
          ),
          yaxis = list(
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
  
  # Implementação ao longo do tempo (Tempo médio de resposta)
  
  output$plot_aceitabilidade_implem_tm <- renderPlotly({
    
    y_lab = "Horas"
    plot_title_acpt = "Intervalo Médio de Resposta às Intervenções Sugeridas"
    
    if(input$plot_acpt_tm_time_setting == "Mensal") {
      
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
        hovertemplate = ~paste0(str_to_title(format(IMPLEMENTATION_MONTH, format = "%B %Y")), "<br>", IMPLEMENTATION_ADHERENCE,": ", tempo_resposta_medio,"<extra></extra>")
      ) %>%
        layout(
          legend = list(x = 100, y = 0.5),
          barmode = 'stack',
          title = list(text = plot_title_acpt, x = 0.5), # Center the title
          xaxis = list(
            title = "",
            tickangle = 45,
            type = "date",
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = "M1",  # One month intervals
            tickformat = "%B %Y",  # Full month name and year
            range = range_x_month_tm_i + c(-15,-1)
          ),
          yaxis = list(
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
        hovertemplate = ~paste0("Semana: ", format(IMPLEMENTATION_WEEK_PT, format = "%d-%m-%y"), "<br>", IMPLEMENTATION_ADHERENCE,": ", tempo_resposta_medio,"<extra></extra>")
      ) %>%
        layout(
          legend = list(x = 100, y = 0.5),
          barmode = 'stack',
          title = list(text = plot_title_acpt, x = 0.5), # Center the title
          xaxis = list(
            title = "",
            tickangle = 45,
            type = "date",
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = x_tick_label_breaks_tm_i * 7 * 24 * 60 * 60 * 1000,  # semanas em ms
            tickformat = "%d %b %Y",  # Formato dos rótulos
            range = range_x_week_tm_i + c(-5,-1)
          ),
          yaxis = list(
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
  
  output$plot_aceitabilidade_tm_dynamic <- renderUI({
    if (filter_aceitabilidade$aceitabilidade_type == "sugestao") {
      plotlyOutput("plot_aceitabilidade_tm")
    } else {
      plotlyOutput("plot_aceitabilidade_implem_tm")
    }
  })
  
  ### PRD 4
  
  # Reactive values to store filters
  filter_saving_int <- reactiveValues(
    saving_int_date = aceitabilidade_date_range,
    saving_int_setor = NULL,
    saving_int_medicamento = NULL,
    saving_int_medico_id = NULL
  )
  
  # Update reactive values when the button is pressed
  observeEvent(input$filter_btn_saving_int, {
    filter_saving_int$saving_int_date <- input$saving_int_date
    filter_saving_int$saving_int_setor <- input$saving_int_setor
    filter_saving_int$saving_int_medicamento <- input$saving_int_medicamento
    filter_saving_int$saving_int_medico_id <- input$saving_int_medico_id
  })
  
  saving_int_tbl_prev <- reactive({
    
    # Filtro por data
    saving_int_tbl_filter_date <- aceitabilidade_tbl %>%
      filter(ALERT_ADHERENCE) %>%
      dplyr::filter(ALERT_DATE >= filter_saving_int$saving_int_date[1] &
                      ALERT_DATE <= filter_saving_int$saving_int_date[2])
    
    # Filtro por médico prescritor
    if (is.null(filter_saving_int$saving_int_medicamento)) {
      saving_int_tbl_filter_med <- saving_int_tbl_filter_date
    } else {
      saving_int_tbl_filter_med <- saving_int_tbl_filter_date %>%
        dplyr::filter(MEDICATION_NAME %in% filter_saving_int$saving_int_medicamento)
    }
    
    # Filtro por médico prescritor
    if (is.null(filter_saving_int$saving_int_medico_id)) {
      saving_int_tbl_filter_pract <- saving_int_tbl_filter_med
    } else {
      saving_int_tbl_filter_pract <- saving_int_tbl_filter_med %>%
        dplyr::filter(PRACTITIONER_ID %in% filter_saving_int$saving_int_medico_id)
    }
    
  })  

  saving_int_tbl <- reactive({  
  # Filtro por setor
  if (is.null(filter_saving_int$saving_int_setor)) {
    saving_int_tbl_filter_sector <- saving_int_tbl_prev()
  } else {
    saving_int_tbl_filter_sector <- saving_int_tbl_prev() %>%
      dplyr::filter(DESCRIPTION %in% filter_saving_int$saving_int_setor)
  }
  })
  ###
  
  output$tableresult <- renderTable({
    saving_tm_comparison_tbl = saving_int_tbl() %>%
      mutate(IMPLEMENTATION_ADHERENCE = factor(IMPLEMENTATION_ADHERENCE,
                                               levels = c(T,F),
                                               labels = c("Alerta convertido","Alerta não aceito"))) %>%
      group_by(IMPLEMENTATION_ADHERENCE, .drop = FALSE) %>%
      reframe(MEAN_THERAPY_DURATION = round(mean(THERAPY_DURATION, na.rm = TRUE),1)) %>%
      ungroup() %>%
      arrange(IMPLEMENTATION_ADHERENCE)
    
    
  })
  output$inputtest <- renderText({filter_saving_int$saving_int_date })
  
  output$saving_total_value <- renderText({
    saving_int_tbl() %>%
      filter(IMPLEMENTATION_ADHERENCE) %>%
      reframe(TOTAL_COST = format_currency_br(sum(SAVING_COST))) %>%
      unlist()
  })
  
  output$saving_per_conversion_value <- renderText({
    pc_value <- saving_int_tbl() %>%
      filter(IMPLEMENTATION_ADHERENCE) %>%
      reframe(TOTAL_COST = format_currency_br(sum(SAVING_COST)/n())) %>%
      unlist()
    ifelse(is.na(pc_value),format_currency_br(0),pc_value)
  })

  output$saving_ratio <- renderText({
    
    if(nrow(saving_int_tbl())==0) {
      "0.0%"
    } else {
    acpt_ratio_value <- saving_int_tbl() %>%
      reframe(ratio = round(sum(IMPLEMENTATION_ADHERENCE)/n()*100,1)) %>%
      unlist()
    
    paste0(acpt_ratio_value, "%")
    }
  })  
  # Taxa de Implementação ao longo do tempo (Tempo médio de resposta)
  
  output$plot_saving_int_ratio <- renderPlotly({
    
    si_y_lab_ratio = "Implementação (%)"
    
    if(input$plot_saving_int_time_setting == "Mensal") {
      
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
          xaxis = list(
            title = "",
            tickangle = 45,
            type = "date",
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = "M1",  # One month intervals
            tickformat = "%b %Y",  # Full month name and year
            range = si_range_x_month_ratio + c(-15,-1)
          ),
          yaxis = list(
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
          xaxis = list(
            title = "",
            tickangle = 45,
            type = "date",
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = si_x_tick_label_breaks_ratio * 7 * 24 * 60 * 60 * 1000,  # semanas em ms
            tickformat = "%d %b %Y",  # Formato dos rótulos
            range = si_range_x_week_ratio + c(-5,-1)
          ),
          yaxis = list(
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
  
  output$plot_saving_int_total <- renderPlotly({
    
    if(nrow(saving_int_tbl())== 0) {
      saving_comparison_tbl <- data.frame(IMPLEMENTATION_ADHERENCE = factor(c("Alerta convertido","Alerta não aceito")),
                                             TOTAL_COST = c(0,0)
      )  %>%
        arrange(IMPLEMENTATION_ADHERENCE)
      
      y_upper_limit_si <- 100
      sc_title = ""
      
    } else {
      
    saving_comparison_tbl = saving_int_tbl() %>%
      mutate(IMPLEMENTATION_ADHERENCE = factor(IMPLEMENTATION_ADHERENCE,
                                               levels = c(T,F),
                                               labels = c("Economia Efetivada","Economia Potencial"))) %>%
      group_by(IMPLEMENTATION_ADHERENCE, .drop = FALSE) %>%
      reframe(TOTAL_COST = sum(SAVING_COST)) %>%
      arrange(IMPLEMENTATION_ADHERENCE)
    
    y_upper_limit_si <- ifelse(max(saving_comparison_tbl$TOTAL_COST, na.rm = T) < 100, 
                               100, 
                               max(saving_comparison_tbl$TOTAL_COST, na.rm = T))
    
    if(!(any(is.na(saving_comparison_tbl$TOTAL_COST)) | any(saving_comparison_tbl$TOTAL_COST == 0))){
      saving_cost_reduction = saving_comparison_tbl$TOTAL_COST[1] - saving_comparison_tbl$TOTAL_COST[2]

      sc_title = paste0("Diferença entre economia efetivada e potencial: ", format_currency_br(saving_cost_reduction))
    } else {
      sc_title = "" 
      }
    }

    # Create the plotly plot
    # acpt_plotly <- 
    plot_ly(
      data = saving_comparison_tbl,
      x = ~IMPLEMENTATION_ADHERENCE,
      y = ~TOTAL_COST,
      type = "bar",
      color = ~IMPLEMENTATION_ADHERENCE,
      colors = c("Economia Efetivada" = "#008080","Economia Potencial" = "#EFBF04"),
      hovertemplate = ~paste0(IMPLEMENTATION_ADHERENCE, ":<br>", format_currency_br(TOTAL_COST),"<extra></extra>"),
      showlegend= FALSE
    ) %>%
      layout(
        legend = 'none',
        barmode = 'stack',
        title = list(text =sc_title, x = 0.5), # Center the title
        xaxis = list(
          title = "",
          tickmode = "linear",
          ticklen = 10,              # Increase tick length to push labels further
          tickfont = list(size = 14) # Define espaçamento uniforme
        ),
        yaxis = list(
          title = list(text = "Valor Total (Reais)",
                       standoff = 20),          # Increase the distance (in pixels)
          tickformat = ",.2f",  # Format numbers as currency with 2 decimal places
          range = c(0,y_upper_limit_si*1.2)
        ),
        margin = list(t = 50, r = 20, b = 50, l = 80), # Adjust plot margins
        legend = list(title = list(text = ""), orientation = "h")
      )  %>%
      config(locale = "pt-BR", # Set the locale to Brazilian Portuguese 
             displayModeBar = FALSE) %>% 
      hide_colorbar() 
  })

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
        xaxis = list(
          title = "",
          tickmode = "linear",  # Define espaçamento uniforme
          ticklen = 10,              # Increase tick length to push labels further
          tickfont = list(size = 14) # Define espaçamento uniforme
        ),
        yaxis = list(
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

output$plot_si_cost_sector <- renderPlotly({
  
  # Opção A: Filtrar Setor
  if(input$si_sector_button == "Ver somente itens selecionados") {
    
    if (is.null(filter_saving_int$saving_int_setor)) {
      saving_int_tbl_filter_sector <- saving_int_tbl_prev()
    } else {
      saving_int_tbl_filter_sector <- saving_int_tbl_prev() %>%
        dplyr::filter(DESCRIPTION %in% filter_saving_int$saving_int_setor)
    }
    
    # Calcular custo total salvo por Setor
    saving_int_cost_tbl <- saving_int_tbl_filter_sector %>%
      mutate(SAVED_COST = COST - EFFECTIVE_COST) %>%
      group_by(DESCRIPTION) %>%
      reframe(SAVED_COST_TOTAL = sum(SAVED_COST, na.rm = TRUE),
              COST_TOTAL = sum(COST, na.rm = T),
              EFFECTIVE_COST_TOTAL = sum(EFFECTIVE_COST, na.rm = T)) %>%
      ungroup()
    
    if(nrow(saving_int_cost_tbl)== 0) {
      saving_int_cost_tbl <- data.frame(DESCRIPTION = sort(unique(aceitabilidade_tbl$DESCRIPTION)),
                                        SAVED_COST_TOTAL = 0,
                                        COST_TOTAL = 0,
                                        EFFECTIVE_COST_TOTAL = 0)
    }

    x_lim_upp_cost_a <- max(saving_int_cost_tbl$SAVED_COST_TOTAL, na.rm = T)
    x_lim_upp_cost_a <- ifelse(x_lim_upp_cost_a < 100, 100, x_lim_upp_cost_b)

    plot_si_cost_a <- 
      saving_int_cost_tbl %>%
      arrange(desc(SAVED_COST_TOTAL)) %>%
      ggplot(aes(y = fct_reorder(DESCRIPTION, SAVED_COST_TOTAL), x = SAVED_COST_TOTAL,
                 labels = "",
                 text = paste0(DESCRIPTION,
                               '<br>Custo sem implementação de intervenções: ', format_currency_br(COST_TOTAL),
                               '<br>Custo efetivo: ', format_currency_br(EFFECTIVE_COST_TOTAL),
                               '<br>Redução do custo: ', format_currency_br(SAVED_COST_TOTAL)
                 ))) +
      geom_col(fill = "#1F968BFF") +
      theme_minimal() +
      ylab("") +
      xlab("Redução de custo (R$)") +
      labs(title = "Redução do Custo Total de Antibióticos por Setor") +
      theme(axis.line.y = element_line()) +
      scale_x_continuous(limits = c(0,x_lim_upp_cost_a),
                         expand = c(0, 0),
                         labels = f <- function(x) format_currency_no_prefix_br(x))
    
    ggplotly(plot_si_cost_a, tooltip = "text")
  } else { 
    # Opção B: Highlight nos setores selecionados

    # Calcular custo total salvo por Setor
    saving_int_cost_tbl <- saving_int_tbl_prev() %>%
      mutate(SAVED_COST = COST - EFFECTIVE_COST) %>%
      group_by(DESCRIPTION) %>%
      reframe(SAVED_COST_TOTAL = sum(SAVED_COST, na.rm = TRUE),
              COST_TOTAL = sum(COST, na.rm = T),
              EFFECTIVE_COST_TOTAL = sum(EFFECTIVE_COST, na.rm = T)) %>%
      ungroup() %>%
      mutate(highlight_medication = ifelse(DESCRIPTION %in% filter_saving_int$saving_int_setor, T, F)) %>%
      mutate(DESCRIPTION = factor(DESCRIPTION, levels = sort(unique(aceitabilidade_tbl$DESCRIPTION)))) %>%
      complete(DESCRIPTION, fill = list(SAVED_COST_TOTAL = 0, COST_TOTAL = 0, 
                                        EFFECTIVE_COST_TOTAL = 0,
                                        highlight_medication = FALSE))
    
  
    x_lim_upp_cost_b <- max(saving_int_cost_tbl$SAVED_COST_TOTAL, na.rm = T)
    x_lim_upp_cost_b <- ifelse(x_lim_upp_cost_b < 100, 100, x_lim_upp_cost_b)
    
    plot_si_cost_color <- c("#1F968BFF", "#404788FF")
    
    plot_si_cost_b <- saving_int_cost_tbl %>%
      arrange(desc(SAVED_COST_TOTAL)) %>%
      ggplot(aes(y = fct_reorder(DESCRIPTION, SAVED_COST_TOTAL), x = SAVED_COST_TOTAL,
                 fill = highlight_medication,
                 labels = "",
                 text = paste0(DESCRIPTION,
                               '<br>Custo sem implementação de intervenções: ', format_currency_br(COST_TOTAL),
                               '<br>Custo efetivo: ', format_currency_br(EFFECTIVE_COST_TOTAL),
                               '<br>Redução do custo: ', format_currency_br(SAVED_COST_TOTAL)
                               )
                 )) +
      geom_col() +
      scale_fill_manual(values = plot_si_cost_color) +
      theme_minimal() +
      ylab("") +
      xlab("Redução de custo (R$)") +
      labs(title = "Redução do Custo Total de Antibióticos por Setor") +
      theme(axis.line.y = element_line(),
            legend.position='none') +
      scale_x_continuous(limits = c(0,x_lim_upp_cost_b),
                          expand = c(0, 0),
                         labels = f <- function(x) format_currency_no_prefix_br(x))
    
    ggplotly(plot_si_cost_b, tooltip = "text")
  }
})

# PRD 4
# Reactive values to store filters
filter_atb_cost <- reactiveValues(
  atb_cost_date = antimicrob_date_range,
  atb_cost_setor = NULL,
  atb_cost_medico_id = NULL,
  atb_cost_tuss = NULL,
  atb_cost_microorganism = NULL,
  atb_medication = NULL
)

# Update reactive values when the button is pressed
observeEvent(input$filter_btn_atb_cost, {
  filter_atb_cost$atb_cost_date <- input$atb_cost_date
  filter_atb_cost$atb_cost_setor <- input$atb_cost_setor
  filter_atb_cost$atb_cost_medico_id <- input$atb_cost_medico_id
  filter_atb_cost$atb_cost_tuss <- input$atb_cost_tuss
  filter_atb_cost$atb_cost_microorganism <- input$atb_cost_microorganism
  filter_atb_cost$atb_medication <- input$atb_medication
})

atb_cost_tbl <- reactive({
  
  atb_cost_tbl_filter_date <- antimicrob_tbl_final %>%
    dplyr::filter(THERAPY_START_DATE >= filter_atb_cost$atb_cost_date[1] &
                    THERAPY_START_DATE <= filter_atb_cost$atb_cost_date[2])
  
  if (is.null(filter_atb_cost$atb_cost_setor)) {
    atb_cost_tbl_filter_sector <- atb_cost_tbl_filter_date
  } else {
    atb_cost_tbl_filter_sector <- atb_cost_tbl_filter_date %>%
      dplyr::filter(DESCRIPTION %in% filter_atb_cost$atb_cost_setor)
  }
  
  if (is.null(filter_atb_cost$atb_cost_medico_id)) {
    atb_cost_tbl_filter_practitioner <- atb_cost_tbl_filter_sector
  } else {
    atb_cost_tbl_filter_practitioner <- atb_cost_tbl_filter_sector %>%
      dplyr::filter(PRACTITIONER_ID %in% filter_atb_cost$atb_cost_medico_id)
  }  
  
  if (is.null(filter_atb_cost$atb_cost_tuss)) {
    atb_cost_tbl_filter_tuss <- atb_cost_tbl_filter_practitioner
  } else {
    atb_cost_tbl_filter_tuss <- atb_cost_tbl_filter_practitioner %>%
      dplyr::filter(tuss_id_completo %in% filter_atb_cost$atb_cost_tuss)
  }    
  
  if (is.null(filter_atb_cost$atb_cost_microorganism)) {
    atb_cost_tbl_filter_micro <- atb_cost_tbl_filter_tuss
  } else {
    atb_cost_tbl_filter_micro <- atb_cost_tbl_filter_tuss %>%
      dplyr::filter(MICROORGANISM_ID %in% filter_atb_cost$atb_cost_microorganism)
  }     
  
  
  if (is.null(filter_atb_cost$atb_medication)) {
    atb_cost_tbl_filter_medication <- atb_cost_tbl_filter_micro
  } else {
    atb_cost_tbl_filter_medication <- atb_cost_tbl_filter_micro %>%
      dplyr::filter(MICROORGANISM_ID %in% filter_atb_cost$atb_medication)
  }       
  
  atb_cost_tbl_filter_medication
})

output$atb_cost_total_value <- renderText({
  atb_cost_tbl() %>%
    reframe(TOTAL_COST = format_currency_br(sum(TOTAL_COST, na.rm = TRUE))) %>%
    unlist()
})

output$atb_mean_cost_value <- renderText({
  
  if(nrow(atb_cost_tbl())==0) {
    format_currency_br(0)
  } else {
    atb_cost_tbl() %>%
      reframe(mean_cost_treatment = format_currency_br(sum(TOTAL_COST, na.rm = TRUE)/nrow(.))) %>%
      unlist()
  }
}) 

output$plot_atb_cost <- renderPlotly({
  
  y_lab_atb_cost = "Custo Total (R$)"
  
  if(input$plot_atb_cost_time_setting == "Mensal") {

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
        xaxis = list(
          title = "",
          tickangle = 45,
          type = "date",
          tickmode = "linear",  # Define espaçamento uniforme
          dtick = "M1",  # One month intervals
          tickformat = "%B %Y",  # Full month name and year
          range = range_x_atb_cost_month + c(-15,-10)
        ),
        yaxis = list(
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
        xaxis = list(
          title = "",
          tickangle = 45,
          type = "date",
          tickmode = "linear",  # Define espaçamento uniforme
          dtick = ac_x_tick_label_breaks * 7 * 24 * 60 * 60 * 1000,  # semanas em ms
          tickformat = "%d %b %Y",  # Formato dos rótulos
          range = range_x_atb_cost_week + c(-5,-1)
        ),
        yaxis = list(
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

}

shinyApp(ui, server)