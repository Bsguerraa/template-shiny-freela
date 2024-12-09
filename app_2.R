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
library(viridis)

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
        ### COLOQUE AQUI OS FILTROS DA PAGINA 1 ##
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
           
           column(1),
           
           pickerInput(
             inputId = "isolamento_microorganism",
             label = "Microorganismo",
             choices = sort(unique(gestao_isolamento_tbl$MICROORGANISM_ID)),
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
           
           radioGroupButtons(
             inputId = "gi_button_time_setting",
             label = "Intervalo de tempo:",
             choices = c("Mensal", 
                         "Semanal"),
             individual = TRUE,
             size = "sm",
             selected = "Mensal",
             direction = "horizontal",
             status = "light"
           ),
           
           radioGroupButtons(
             inputId = "gi_button_sector",
             label = "Setores:",
             choices = c("Agregados", 
                         "Separados"),
             individual = TRUE,
             size = "sm",
             selected = "Agregados",
             direction = "horizontal",
             status = "light"
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
            selected = "Todas"
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
              
              layout_column_wrap(
                width = 1/3,
                height = 120,
                
                value_box(
                  title = "Economia Total do Giro de Leito de Isolamento",
                  value = textOutput("saving_giro_total_value"),
                  showcase = bsicons::bs_icon("cash-coin"),
                  theme = value_box_theme(bg = "white", fg = "#0B538E"),
                  class = "border"
                ),
                
                value_box(
                  title = "Taxa de Rotatividade de Leitos",
                  value = textOutput("saving_giro_tx_rotatividade_value"),
                  showcase = bsicons::bs_icon("file-medical"),
                  theme = value_box_theme(bg = "white", fg = "#0B538E"),
                  class = "border"
                ),
                
                value_box(
                  title = "Tempo Médio de Ocupação do Leito de Isolamento",
                  value = textOutput("saving_tm_ocupacao_leito_value"),
                  showcase = bsicons::bs_icon("hospital"),
                  theme = value_box_theme(bg = "white", fg = "#0B538E"),
                  class = "border"
                )
              ),
              
             navset_card_underline(
                title = "Eficiência BDMáx",
                
              nav_panel("Tempo entre Coleta e Resultado",
              
              card(
                height = 450,
                full_screen = TRUE,
                uiOutput("gi_dif_amostra_result_plot")
              )
              ),
              
              nav_panel("Tempo entre Resultado e Isolamento/Liberação",
                        card(
                          height = 450,
                          full_screen = TRUE,
                          uiOutput("gi_dif_res_amostra_result_plot")
                        )
                        
              )
              ),
              
              card(
                height = 450,
                max_height = 600,
                min_height = 350,
                full_screen = TRUE,
                card_header(
                  # Usando flexbox para alinhar o título e o ícone
                  div(
                    style = "display: flex; justify-content: space-between; align-items: center;",
                    span("Economia pelo Giro de Leito de Isolamento", style = "flex: 1; text-align: center;"),
                    tooltip(
                      bsicons::bs_icon("info-circle"),
                      "A Economia do Giro de Leito de Isolamento é estimada assumindo uma média de ocupação anterior de 5 dias, subtraída pelo tempo entre o resultado do exame e a liberação do leito, e multiplicada pelo custo diário de internação",
                      id = "tooltip",
                      placement = "top"
                    )
                  )
                ),
                
                uiOutput("saving_cost_ocupacao_leito")
              ),
               
             card(
               height = 450,
               max_height = 600,
               min_height = 350,
               full_screen = TRUE,
               card_header(
                 # Usando flexbox para alinhar o título e o ícone
                 div(
                   style = "display: flex; justify-content: space-between; align-items: center;",
                   span("Taxa de Rotatividade de Leitos", style = "flex: 1; text-align: center;"),
                   tooltip(
                     bsicons::bs_icon("info-circle"),
                     "A taxa é calculada pela razão entre a o nº de Pacientes Transferidos ou Liberados do Isolamento pelo Total de Leitos de Isolamento Ocupados, multiplicado por 100",
                     id = "tooltip",
                     placement = "top"
                   )
                 )
               ),
               
               uiOutput("tx_rotatividade_ocupacao_leito")
             ),
             
             card(
               height = 450,
               max_height = 600,
               min_height = 350,
               full_screen = TRUE,

               card_header(
                 # Usando flexbox para alinhar o título e o ícone
                 div(
                   style = "display: flex; justify-content: space-between; align-items: center;",
                   span("Tempo Médio de Ocupação do Leito de Isolamento", style = "flex: 1; text-align: center;"),
                   tooltip(
                     bsicons::bs_icon("info-circle"),
                     "O Tempo Médio é calculado pela razão entre o Total de Dias de Ocupação de Leitos de Isolamento pelo Número Total Saídas do Isolamento no período. Considera-se somente pacientes que iniciaram a internação e realizaram a saída do isolamento dentro do período selecionado nos filtros. A visualização deste gráfico é feita sempre pela data de entrada do paciente, mesmo que outra data de filtro seja selecionada.",
                     id = "tooltip",
                     placement = "top"
                   )
                 )
               ),
               
               uiOutput("tm_ocupacao_leito_plot")
             )
              ),
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
                    plotlyOutput("ddd_plot_month")
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
                    plotlyOutput("dot_plot_month")
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
                    
                    plotlyOutput("lot_plot_month")
                    
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
                card_header(
                  # Usando flexbox para alinhar o título e o ícone
                  div(
                    style = "display: flex; justify-content: space-between; align-items: center;",
                    span("Adesão às Intervenções Sugeridas", style = "flex: 1; text-align: center;"),
                    tooltip(
                      bsicons::bs_icon("info-circle"),
                      "Sugestão: considera as intervenções farmacêuticas que sugerem a conversão de antibióticos de via intravenosa para via oral, que podem ser aceitas ou rejeitadas pelo profissional. Implementação: dentro das intervenções sugeridas que foram aceitas, quantas de fato foram implementadas no tratamento do paciente",
                      id = "tooltip",
                      placement = "top"
                    )
                  )
                ),
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
                 uiOutput("plot_aceitabilidade_n_dynamic"),
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
              ),
              
              card(
                height = 450,
                full_screen = TRUE,
                layout_sidebar(
                  fillable = TRUE,
                  sidebar = sidebar(
                    width = 180,
                    open = "closed",
                    radioGroupButtons(
                      inputId = "atb_cost_plot_button",
                      label = "Escolha a visualização",
                      choices = c("Destacar itens", 
                                  "Ver somente itens selecionados"),
                      size = "sm",
                      direction = "vertical"
                    )
                  ),
                  plotlyOutput("atb_cost_sector_plot")
                )
              ), # card
              ),
    
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
    isolamento_medico_id = NULL,
    isolamento_microorganism = NULL,
    gi_button_sector = 'Agregados',
    gi_button_time_setting = 'Mensal'
  )
  
  # Update reactive values when the button is pressed
  observeEvent(input$filter_btn_gi, {
    filter_gi$gestao_isolamento_tipo_data <- input$gestao_isolamento_tipo_data
    filter_gi$gestao_isolamento_data <- input$gestao_isolamento_data
    filter_gi$isolamento_setor <- input$isolamento_setor
    filter_gi$isolamento_medico_id <- input$isolamento_medico_id
    filter_gi$isolamento_microorganism <- input$isolamento_microorganism
    filter_gi$gi_button_sector <- input$gi_button_sector
    filter_gi$gi_button_time_setting <- input$gi_button_time_setting
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
    updatePickerInput(session, "isolamento_microorganism", selected = "")
    updateRadioGroupButtons(session, "gi_button_sector", selected = 'Agregados')
    updateRadioGroupButtons(session, "gi_button_time_setting", selected = 'Mensal')
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
  
  # PRD 1
  
  gi_x_label_month <- reactive({
    switch(filter_gi$gestao_isolamento_tipo_data,
           "entrada" = "Mês de entrada do paciente",
           "coleta" = "Mês de coleta do exame",
           "resultado" = "Mês de resultado do exame",
           "release" = "Mês de transferência ou liberação")
  })
  
  gi_x_label_week <- reactive({
    switch(filter_gi$gestao_isolamento_tipo_data,
           "entrada" = "Semana de entrada do paciente",
           "coleta" = "Semana de coleta do exame",
           "resultado" = "Semana de resultado do exame",
           "release" = "Semana de transferência ou liberação")
  })
  
  x_range_gi_month <- reactive({ 
    if (filter_gi$gestao_isolamento_tipo_data == "entrada") {
      x_range_gi_month <- as.Date(floor_date(range(gi_tbl_prev_date()$ADMISSION_DATE), unit = "month"))
    }
    
    if (filter_gi$gestao_isolamento_tipo_data == "coleta") {
      x_range_gi_month <- as.Date(floor_date(range(gi_tbl_prev_date()$collection_date_time), unit = "month"))
    }
    
    if (filter_gi$gestao_isolamento_tipo_data == "resultado") {
      x_range_gi_month <- as.Date(floor_date(range(gi_tbl_prev_date()$result_status_date_time), unit = "month"))
    }
    
    if (filter_gi$gestao_isolamento_tipo_data == "release") {
      x_range_gi_month <- as.Date(floor_date(range(gi_tbl_prev_date()$RELEASE_DATE), unit = "month"))
    }
    x_range_gi_month
  })
  
  x_range_gi_week <- reactive({
    if (filter_gi$gestao_isolamento_tipo_data == "entrada") {
      x_range_gi_week <- as.Date(floor_date(range(gi_tbl_prev_date()$ADMISSION_DATE), unit = "week"))
    }
    
    if (filter_gi$gestao_isolamento_tipo_data == "coleta") {
      x_range_gi_week <- as.Date(floor_date(range(gi_tbl_prev_date()$collection_date_time), unit = "week"))
    }
    
    if (filter_gi$gestao_isolamento_tipo_data == "resultado") {
      x_range_gi_week <- as.Date(floor_date(range(gi_tbl_prev_date()$result_status_date_time), unit = "week"))
    }
    
    if (filter_gi$gestao_isolamento_tipo_data == "release") {
      x_range_gi_week <- as.Date(floor_date(range(gi_tbl_prev_date()$RELEASE_DATE), unit = "week"))
    }
    x_range_gi_week
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
    
      # Filtro por médico
    if (is.null(filter_gi$isolamento_medico_id)) {
      gestao_isolamento_tbl_filter_medico <- gestao_isolamento_tbl_filter_setor
    } else {
      gestao_isolamento_tbl_filter_medico <- gestao_isolamento_tbl_filter_setor %>%
        dplyr::filter(PRACTITIONER_ID %in% filter_gi$isolamento_medico_id)
    }
    
    # Filtro por microorganismo
    if (is.null(filter_gi$isolamento_microorganism)) {
      gestao_isolamento_tbl_filter_micro <- gestao_isolamento_tbl_filter_medico
    } else {
      gestao_isolamento_tbl_filter_micro <- gestao_isolamento_tbl_filter_medico %>%
        dplyr::filter(MICROORGANISM_ID %in% filter_gi$isolamento_microorganism)
    }

    # Calcular horas e mês de acordo com o tipo de data escolhida
    
    if (filter_gi$gestao_isolamento_tipo_data == "entrada") {
      gestao_isolamento_tbl_output <- gestao_isolamento_tbl_filter_micro %>%
        mutate(Horas = as.numeric(difftime(result_status_date_time, collection_date_time, units = "hours")),
               Horas_res = as.numeric(difftime(RELEASE_DATE, result_status_date_time, units = "hours")),
               month = as.Date(floor_date(ADMISSION_DATE, unit = "month")),
               week = as.Date(floor_date(ADMISSION_DATE, unit = "week")))
    }
    
    if (filter_gi$gestao_isolamento_tipo_data == "coleta") {
      gestao_isolamento_tbl_output <- gestao_isolamento_tbl_filter_micro %>%
        mutate(Horas = as.numeric(difftime(result_status_date_time, collection_date_time, units = "hours")),
               Horas_res = as.numeric(difftime(RELEASE_DATE, result_status_date_time, units = "hours")),
               month = as.Date(floor_date(collection_date_time, unit = "month")),
               week = as.Date(floor_date(collection_date_time, unit = "week")))
    }
    
    if (filter_gi$gestao_isolamento_tipo_data == "resultado") {
      gestao_isolamento_tbl_output <- gestao_isolamento_tbl_filter_micro %>%
        mutate(Horas = as.numeric(difftime(result_status_date_time, collection_date_time, units = "hours")),
               Horas_res = as.numeric(difftime(RELEASE_DATE, result_status_date_time, units = "hours")),
               month = as.Date(floor_date(result_status_date_time, unit = "month")),
               week = as.Date(floor_date(result_status_date_time, unit = "week")))
    }
    
    if (filter_gi$gestao_isolamento_tipo_data == "release") {
      gestao_isolamento_tbl_output <- gestao_isolamento_tbl_filter_micro %>%
        mutate(Horas = as.numeric(difftime(result_status_date_time, collection_date_time, units = "hours")),
               Horas_res = as.numeric(difftime(RELEASE_DATE, result_status_date_time, units = "hours")),
               month = as.Date(floor_date(RELEASE_DATE, unit = "month")),
               week = as.Date(floor_date(RELEASE_DATE, unit = "week")))
    }
    
    gestao_isolamento_tbl_output

  })
  
  gi_tbl_month <- reactive({
    
    if(nrow(gi_tbl_prev())==0) {
      data.frame(month = c(floor_date(filter_gi$gestao_isolamento_data[1], unit = 'month'), 
                           floor_date(filter_gi$gestao_isolamento_data[2], unit = 'month')),
                 Horas = NA,
                 Horas_res = NA)
    } else {
    
    gi_tbl_prev() %>% 
      mutate(Horas = as.numeric(difftime(result_status_date_time, collection_date_time, units = "hours")),
             Horas_res = as.numeric(difftime(RELEASE_DATE, result_status_date_time, units = "hours")),
             month = as.Date(floor_date(collection_date_time, unit = "month"))) |>
      group_by(month) |>
      summarise(Horas = mean(Horas, na.rm = TRUE),
                Horas_res = mean(Horas_res, na.rm = TRUE)) %>%
      select(month, Horas, Horas_res) %>%
      arrange(month)  %>%
        complete(month = seq(floor_date(filter_gi$gestao_isolamento_data[1], unit = 'month'), 
                             floor_date(filter_gi$gestao_isolamento_data[2], unit = 'month'), by = "month"),
                 fill = list(Horas = 0, Horas_res = 0))
    }
    })
  
  gi_tbl_week <- reactive({
    
    if(nrow(gi_tbl_prev())==0) {
      data.frame(week = c(floor_date(filter_gi$gestao_isolamento_data[1], unit = 'week'), 
                          floor_date(filter_gi$gestao_isolamento_data[2], unit = 'week')),
                 Horas = NA,
                 Horas_res = NA)
    } else {
      
      gi_tbl_prev() %>% 
        mutate(Horas = as.numeric(difftime(result_status_date_time, collection_date_time, units = "hours")),
               Horas_res = as.numeric(difftime(RELEASE_DATE, result_status_date_time, units = "hours")),
               week = as.Date(floor_date(collection_date_time, unit = "week"))) |>
        group_by(week) |>
        summarise(Horas = mean(Horas, na.rm = TRUE),
                  Horas_res = mean(Horas_res, na.rm = TRUE)) %>%
        select(week, Horas, Horas_res) %>%
        arrange(week)  %>%
        complete(week = seq(floor_date(filter_gi$gestao_isolamento_data[1], unit = 'week'), 
                            floor_date(filter_gi$gestao_isolamento_data[2], unit = 'week'), by = "week"),
                 fill = list(Horas = 0, Horas_res = 0))
    }
  })
  
  gi_tbl_sector <- reactive({
    
    if(nrow(gi_tbl_prev())==0) {
      data.frame(month = c(floor_date(filter_gi$gestao_isolamento_data[1], unit = 'month'), 
                           floor_date(filter_gi$gestao_isolamento_data[2], unit = 'month')),
                                         Horas = NA,
                                         Horas_res = NA,
                                         DESCRIPTION = NA)
    } else {

    gi_tbl_prev() %>%
        mutate(Horas = as.numeric(difftime(result_status_date_time, collection_date_time, units = "hours")),
               Horas_res = as.numeric(difftime(RELEASE_DATE, result_status_date_time, units = "hours")),
               month = as.Date(floor_date(collection_date_time, unit = "month"))) |>
        group_by(month, DESCRIPTION) |>
        reframe(Horas = mean(Horas, na.rm = TRUE),
                Horas_res = mean(Horas_res, na.rm = TRUE)) %>%
        select(month, DESCRIPTION, Horas, Horas_res) %>%
        arrange(DESCRIPTION, month) %>%
        mutate(DESCRIPTION = factor(DESCRIPTION)) %>%
        complete(DESCRIPTION,
                 month = seq(floor_date(filter_gi$gestao_isolamento_data[1], unit = 'month'), 
                             floor_date(filter_gi$gestao_isolamento_data[2], unit = 'month'), by = "month"),
                 fill = list(Horas = 0, Horas_res = 0))
    }
  })
  
  gi_tbl_sector_week <- reactive({
    
    if(nrow(gi_tbl_prev())==0) {
      data.frame(week = c(floor_date(filter_gi$gestao_isolamento_data[1], unit = 'week'), 
                          floor_date(filter_gi$gestao_isolamento_data[2], unit = 'week')),
                 Horas = NA,
                 Horas_res = NA,
                 DESCRIPTION = NA)
    } else {
      
      gi_tbl_prev() %>%
        mutate(Horas = as.numeric(difftime(result_status_date_time, collection_date_time, units = "hours")),
               Horas_res = as.numeric(difftime(RELEASE_DATE, result_status_date_time, units = "hours")),
               week = as.Date(floor_date(collection_date_time, unit = "week"))) |>
        group_by(week, DESCRIPTION) |>
        reframe(Horas = mean(Horas, na.rm = TRUE),
                Horas_res = mean(Horas_res, na.rm = TRUE)) %>%
        select(week, DESCRIPTION, Horas, Horas_res) %>%
        arrange(DESCRIPTION, week) %>%
        mutate(DESCRIPTION = factor(DESCRIPTION)) %>%
        complete(DESCRIPTION,
                 week = seq(floor_date(filter_gi$gestao_isolamento_data[1], unit = 'week'), 
                            floor_date(filter_gi$gestao_isolamento_data[2], unit = 'week'), by = "week"),
                 fill = list(Horas = 0, Horas_res = 0))
    }
  })
  
  # Diferença máxima de tempo na gestão de isolamento (para limite do eixo y) por mês
  max_dif_isolamento_month <- gestao_isolamento_tbl %>% 
    mutate(Horas = as.numeric(difftime(result_status_date_time, collection_date_time, units = "hours")),
           month = as.Date(floor_date(collection_date_time, unit = "month"))) |>
    group_by(month, DESCRIPTION) |>
    summarise(Horas = mean(Horas, na.rm = T)) %>%
    ungroup() %>%
    filter(Horas == max(Horas, na.rm = TRUE)) %>%
    select(Horas) %>%
    mutate(Horas = Horas*1.2)
  
  # Diferença máxima de tempo entre resultado e liberacao/isolamento na gestão de isolamento (para limite do eixo y) por mês
  max_dif_isolamento_month_res <- gestao_isolamento_tbl %>% 
    mutate(Horas_res = as.numeric(difftime(RELEASE_DATE, result_status_date_time, units = "hours")),
           month = as.Date(floor_date(collection_date_time, unit = "month"))) |>
    group_by(month, DESCRIPTION) |>
    summarise(Horas_res = mean(Horas_res, na.rm = T)) %>%
    ungroup() %>%
    filter(Horas_res == max(Horas_res, na.rm = TRUE)) %>%
    select(Horas_res) %>%
    mutate(Horas_res = Horas_res*1.2)  
  
  # Diferença máxima de tempo na gestão de isolamento (para limite do eixo y) por semana
  max_dif_isolamento_week <- gestao_isolamento_tbl %>% 
    mutate(Horas = as.numeric(difftime(result_status_date_time, collection_date_time, units = "hours")),
           week = as.Date(floor_date(collection_date_time, unit = "week"))) |>
    group_by(week, DESCRIPTION) |>
    summarise(Horas = mean(Horas, na.rm = T)) %>%
    ungroup() %>%
    filter(Horas == max(Horas, na.rm = TRUE)) %>%
    select(Horas) %>%
    mutate(Horas = Horas*1.2)
  
  # Diferença máxima de tempo entre resultado e liberacao/isolamento na gestão de isolamento (para limite do eixo y) por semana
  max_dif_isolamento_week_res <- gestao_isolamento_tbl %>% 
    mutate(Horas_res = as.numeric(difftime(RELEASE_DATE, result_status_date_time, units = "hours")),
           week = as.Date(floor_date(collection_date_time, unit = "week"))) |>
    group_by(week, DESCRIPTION) |>
    summarise(Horas_res = mean(Horas_res, na.rm = T)) %>%
    ungroup() %>%
    filter(Horas_res == max(Horas_res, na.rm = TRUE)) %>%
    select(Horas_res) %>%
    mutate(Horas_res = Horas_res*1.2)
  
  
  saving_giro_tbl <- reactive({ 
    
    if(nrow(gi_tbl_prev())== 0) {
      data.frame()
    } else {
      gi_tbl_prev() %>%
        filter(ISOLATED == "Sim") %>%
        mutate(occupation_hours = as.numeric(difftime(result_status_date_time, ADMISSION_DATE, units = "hours")),
               occupation_days = (occupation_hours %/% 24) + 1,
               saving_days_release = mean_isolation_time - occupation_days,
               saving_cost = saving_days_release * bed_cost_per_day)
    }
  })
  
  # Diferença máxima de tempo na gestão de isolamento (para limite do eixo y)
  max_dif_isolamento_sc <- reactive({ 
    
    if(nrow(saving_giro_tbl())>0) {  
      
      saving_giro_tbl() %>%
        group_by(month) %>%
        reframe(saving_cost = sum(saving_cost, na.rm = TRUE)) |>
        ungroup() %>%
        filter(saving_cost == max(saving_cost, na.rm = TRUE)) %>%
        select(saving_cost) %>%
        mutate(max_saving_cost = saving_cost*1.2)
      
    } else {
      
      data.frame(max_saving_cost = 100)
      
    }
  })
  
  output$saving_giro_total_value <- renderText({
    
    if(nrow(saving_giro_tbl())==0) {
      sgtvalue <- format_currency_br(0)
    } else {
      sgtvalue <- format_currency_br(sum(saving_giro_tbl()$saving_cost, na.rm = TRUE))
    }
    sgtvalue
  })
  
  # Taxa de Rotatividade
  saving_giro_tx_rot_tbl <- reactive({ 
    gi_tbl_prev() %>%
      filter(ISOLATED == "Sim") %>%
      mutate(RELEASE_DATE = as.Date(RELEASE_DATE)) %>%
      filter(RELEASE_DATE >= filter_gi$gestao_isolamento_data[1] &
               RELEASE_DATE <= filter_gi$gestao_isolamento_data[2]) %>%
      left_join(bed_availability_tbl, by = c("RELEASE_DATE" = "bed_date")) %>%
      reframe(tx_rotatividade = n()/mean(bed_n, na.rm = T))
  })
  
  output$saving_giro_tx_rotatividade_value <- renderText({
    
    if(nrow(gi_tbl_prev())==0) {
      sgtx_result <- "0"
    } else {
      tx_giro_value <- round(saving_giro_tx_rot_tbl()$tx_rotatividade,1)*100
      sgtx_result <- format(tx_giro_value, decimal.mark = ",", big.mark = ".", nsmall = 1)
    }
    sgtx_result
  })
  
  tm_ocupacao_leito_tbl <- reactive({ 
    gi_tbl_prev() %>%
      # Considera somente pacientes que internaram dentro do período selecionado
      filter(RELEASE_DATE >= filter_gi$gestao_isolamento_data[1]) %>%
      # Considera somente pacientes que receberam alta até o período selecionado
      filter(RELEASE_DATE <= filter_gi$gestao_isolamento_data[2]) %>%
      # Considera somente pacientes que estavam isolados, e foram liberados
      filter(ISOLATED == "Sim") %>%
      reframe(tm_ocupacao_leito = sum(bed_occupation_days, na.rm = T)/n())
  })
  
  output$saving_tm_ocupacao_leito_value <- renderText({
    
    if(nrow(gi_tbl_prev())==0) {
      sgtmvlue <- ""
    } else {
      tm_value <- round(tm_ocupacao_leito_tbl()$tm_ocupacao_leito,1)
      tm_value_format <- format(tm_value, decimal.mark = ",", big.mark = ".", nsmall = 1)
      sgtmvlue <- paste(tm_value_format, "dias")
    }
    sgtmvlue
  })
  
  output$gi_dif_amostra_result_plot_month <- renderPlotly({
    
    if(filter_gi$gi_button_sector == "Agregados") {

      display_hoverinfo = ifelse(all(is.na(gi_tbl_month()$Horas)), 'none','text')
      
      p4 <- gi_tbl_month() %>%
        plot_ly(
          x = ~month,
          y = ~Horas,
          marker = list(color = '#08A18E'),
          line = list(color = '#08A18E'),
          type = "scatter",
          mode = "lines+markers",
          text = ~paste0(str_to_title(format(month, format = "%B %Y")),'<br>Tempo: ', format(round(Horas, 1), decimal.mark = ","), ' horas'),
          hoverinfo = display_hoverinfo,
          showlegend = FALSE
        ) |>
        layout(
          shapes = list(
            list(
              type = "line",
              x0 = min(gi_tbl_month()$month, na.rm = TRUE),
              x1 = max(gi_tbl_month()$month, na.rm = TRUE),
              y0 = 24,
              y1 = 24,
              line = list(dash = "dash", color = "red")
            )
          ),
          xaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = gi_x_label_month(),
            type = "date",
            tickformat = "%b %Y",
            tickangle = 45,
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = "M1"  # One month intervals
          ),
          yaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "Tempo (horas)",
            range = c(0, max_dif_isolamento_month$Horas)
          ),
          annotations = list(
            list(
              x = max(gi_tbl_month()$month, na.rm = TRUE),
              y = 26,
              text = "24 horas",
              showarrow = FALSE,
              font = list(color = "red", size = 12),
              xanchor = "right"
            )
          ),
          margin = list(l = 50, r = 50, t = 50, b = 50)
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
    
    } else {
      
      color_pallete <- viridis::viridis(length(unique(gi_tbl_sector()$DESCRIPTION)))
      
      display_hoverinfo = ifelse(all(is.na(gi_tbl_sector()$Horas)), 'none','text')
      
      p4 <- gi_tbl_sector() %>%
        plot_ly(
          x = ~month,
          y = ~Horas,
          color = ~DESCRIPTION,
          colors = color_pallete,
          type = "scatter",
          mode = "lines+markers",
          text = ~paste0(DESCRIPTION, ': ', format(round(Horas, 1), decimal.mark = ","), ' horas'),
          hoverinfo = display_hoverinfo,
          showlegend = TRUE
        ) |>
        layout(
          shapes = list(
            list(
              type = "line",
              x0 = min(gi_tbl_sector()$month, na.rm = TRUE),
              x1 = max(gi_tbl_sector()$month, na.rm = TRUE),
              y0 = 24,
              y1 = 24,
              line = list(dash = "dash", color = "red")
            )
          ),
          xaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = gi_x_label_month(),
            type = "date",
            tickformat = "%b %Y",
            tickangle = 45,
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = "M1"  # One month intervals
          ),
          yaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "Tempo (horas)",
            range = c(0, max_dif_isolamento_month$Horas)
          )
          
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
      # Mostra legenda somente se existirem valores para os filtros selecionados
      if (!all(is.na(gi_tbl_sector()$Horas))) {
        p4 <- p4 %>%
          layout(
            annotations = list(
              list(
                x = max(gi_tbl_sector()$month, na.rm = TRUE),
                y = 26,
                text = "24 horas",
                showarrow = FALSE,
                font = list(color = "red", size = 12),
                xanchor = "right"
              )
            ),
            hovermode = "x unified",
            legend = list(
              traceorder = "grouped", # Agrupa itens na legenda
              font = list(family = "Open Sans", size = 12)
            ),
            margin = list(l = 50, r = 50, t = 50, b = 50)
          )
      }
      
    }
    
    return(p4)

  })
  
  output$gi_dif_amostra_result_plot_week <- renderPlotly({
    
    gi_dar_x_axis_range <- as.numeric(diff(range(gi_tbl_week()$week)))
    gi_dar_x_tick_label_breaks <- case_when(gi_dar_x_axis_range <= 60 ~ 1,
                                     gi_dar_x_axis_range > 60 & gi_dar_x_axis_range <= 300 ~ 2,
                                     gi_dar_x_axis_range > 300 & gi_dar_x_axis_range <= 540 ~ 3,
                                     gi_dar_x_axis_range > 540 ~ 4)
    
    if(filter_gi$gi_button_sector == "Agregados") {
      
      display_hoverinfo = ifelse(all(is.na(gi_tbl_week()$Horas)), 'none','text')
      
      p4 <- gi_tbl_week() %>%
        plot_ly(
          x = ~week,
          y = ~Horas,
          marker = list(color = '#08A18E'),
          line = list(color = '#08A18E'),
          type = "scatter",
          mode = "lines+markers",
          text = ~paste0(str_to_title(format(week, format = "%d %b %Y")),'<br>Tempo: ', format(round(Horas, 1), decimal.mark = ","), ' horas'),
          hoverinfo = display_hoverinfo,
          showlegend = FALSE
        ) |>
        layout(
          shapes = list(
            list(
              type = "line",
              x0 = min(gi_tbl_week()$week, na.rm = TRUE),
              x1 = max(gi_tbl_week()$week, na.rm = TRUE),
              y0 = 24,
              y1 = 24,
              line = list(dash = "dash", color = "red")
            )
          ),
          xaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = gi_x_label_week(),
            type = "date",
            tickformat = "%d %b %Y",
            tickangle = 45,
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = gi_dar_x_tick_label_breaks * 7 * 24 * 60 * 60 * 1000  # semanas em ms #distância entre ticks
          ),
          yaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "Tempo (horas)",
            range = c(0, max_dif_isolamento_week$Horas)
          ),
          annotations = list(
            list(
              x = max(gi_tbl_week()$week, na.rm = TRUE),
              y = 26,
              text = "24 horas",
              showarrow = FALSE,
              font = list(color = "red", size = 12),
              xanchor = "right"
            )
          ),
          margin = list(l = 50, r = 50, t = 50, b = 50)
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
    } else {
      
      color_pallete <- viridis::viridis(length(unique(gi_tbl_sector_week()$DESCRIPTION)))
      
      display_hoverinfo = ifelse(all(is.na(gi_tbl_sector_week()$Horas)), 'none','text')
      
      p4 <- gi_tbl_sector_week() %>%
        plot_ly(
          x = ~week,
          y = ~Horas,
          color = ~DESCRIPTION,
          colors = color_pallete,
          type = "scatter",
          mode = "lines+markers",
          text = ~paste0(DESCRIPTION, ': ', format(round(Horas, 1), decimal.mark = ","), ' horas (',format(week, format = "%d %B %Y"),')'),
          hoverinfo = display_hoverinfo,
          showlegend = TRUE
        ) |>
        layout(
          shapes = list(
            list(
              type = "line",
              x0 = min(gi_tbl_sector_week()$week, na.rm = TRUE),
              x1 = max(gi_tbl_sector_week()$week, na.rm = TRUE),
              y0 = 24,
              y1 = 24,
              line = list(dash = "dash", color = "red")
            )
          ),
          xaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = gi_x_label_week(),
            hoverformat = "Semana: %d %B %Y",
            type = "date",
            tickformat = "%d %b %Y",
            tickangle = 45,
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = gi_dar_x_tick_label_breaks * 7 * 24 * 60 * 60 * 1000  # semanas em ms #distância entre ticks
          ),
          yaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "Tempo (horas)",
            range = c(0, max_dif_isolamento_week$Horas)
          )
          
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
      # Mostra legenda somente se existirem valores para os filtros selecionados
      if (!all(is.na(gi_tbl_sector_week()$Horas))) {
        p4 <- p4 %>%
          layout(
            annotations = list(
              list(
                x = max(gi_tbl_sector_week()$week, na.rm = TRUE),
                y = 26,
                text = "24 horas",
                showarrow = FALSE,
                font = list(color = "red", size = 12),
                xanchor = "right"
              )
            ),
            hovermode = "x unified",
            legend = list(
              traceorder = "grouped", # Agrupa itens na legenda
              font = list(family = "Open Sans", size = 12)
            ),
            margin = list(l = 50, r = 50, t = 50, b = 50)
          )
      }
      
    }
    
    return(p4)
    
  })
  
  output$gi_dif_amostra_result_plot <- renderUI({
    
    if(filter_gi$gi_button_time_setting == 'Mensal') {
      plotlyOutput("gi_dif_amostra_result_plot_month")
    } else {
      plotlyOutput("gi_dif_amostra_result_plot_week")
    }
    
  })
  
  output$gi_dif_res_amostra_result_plot_month <- renderPlotly({
    
    if(filter_gi$gi_button_sector == "Agregados") {
      
      display_hoverinfo = ifelse(all(is.na(gi_tbl_month()$Horas_res)), 'none','text')
      
      p4 <- gi_tbl_month() %>%
        plot_ly(
          x = ~month,
          y = ~Horas_res,
          type = "scatter",
          mode = "lines+markers",
          text = ~paste0(str_to_title(format(month, format = "%B %Y")),'<br>Tempo: ', format(round(Horas_res, 1), decimal.mark = ","), ' horas'),
          hoverinfo = display_hoverinfo,
          showlegend = FALSE
        ) |>
        layout(
          shapes = list(
            list(
              type = "line",
              x0 = min(gi_tbl_month()$month, na.rm = TRUE),
              x1 = max(gi_tbl_month()$month, na.rm = TRUE),
              y0 = 24,
              y1 = 24,
              line = list(dash = "dash", color = "red")
            )
          ),
          xaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = gi_x_label_month(),
            type = "date",
            tickformat = "%b %Y",
            tickangle = 45,
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = "M1"  # One month intervals
          ),
          yaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "Tempo (horas)",
            range = c(0, max_dif_isolamento_month_res$Horas_res)
          ),
          annotations = list(
            list(
              x = max(gi_tbl_month()$month, na.rm = TRUE),
              y = 26,
              text = "24 horas",
              showarrow = FALSE,
              font = list(color = "red", size = 12),
              xanchor = "right"
            )
          ),
          margin = list(l = 50, r = 50, t = 50, b = 50)
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
    } else {
      
      color_pallete <- viridis::viridis(length(unique(gi_tbl_sector()$DESCRIPTION)))
      
      display_hoverinfo = ifelse(all(is.na(gi_tbl_sector()$Horas_res)), 'none','text')
      
      p4 <- gi_tbl_sector() %>%
        plot_ly(
          x = ~month,
          y = ~Horas_res,
          color = ~DESCRIPTION,
          colors = color_pallete,
          type = "scatter",
          mode = "lines+markers",
          text = ~paste0(DESCRIPTION, ': ', format(round(Horas_res, 1), decimal.mark = ","), ' horas'),
          hoverinfo = display_hoverinfo,
          showlegend = TRUE
        ) |>
        layout(
          shapes = list(
            list(
              type = "line",
              x0 = min(gi_tbl_sector()$month, na.rm = TRUE),
              x1 = max(gi_tbl_sector()$month, na.rm = TRUE),
              y0 = 24,
              y1 = 24,
              line = list(dash = "dash", color = "red")
            )
          ),
          xaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = gi_x_label_month(),
            type = "date",
            tickformat = "%b %Y",
            tickangle = 45,
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = "M1"  # One month intervals
          ),
          yaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "Tempo (horas)",
            range = c(0, max_dif_isolamento_month_res$Horas_res)
          )
          
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
      # Mostra legenda somente se existirem valores para os filtros selecionados
      if (!all(is.na(gi_tbl_sector()$Horas_res))) {
        p4 <- p4 %>%
          layout(
            annotations = list(
              list(
                x = max(gi_tbl_sector()$month, na.rm = TRUE),
                y = 26,
                text = "24 horas",
                showarrow = FALSE,
                font = list(color = "red", size = 12),
                xanchor = "right"
              )
            ),
            hovermode = "x unified",
            legend = list(
              traceorder = "grouped", # Agrupa itens na legenda
              font = list(family = "Open Sans", size = 12)
            ),
            margin = list(l = 50, r = 50, t = 50, b = 50)
          )
      }
      
    }
    
    return(p4)
    
  })
  
  output$gi_dif_res_amostra_result_plot_week <- renderPlotly({
    
    gi_dar_x_axis_range <- as.numeric(diff(range(gi_tbl_week()$week)))
    gi_dar_x_tick_label_breaks <- case_when(gi_dar_x_axis_range <= 60 ~ 1,
                                            gi_dar_x_axis_range > 60 & gi_dar_x_axis_range <= 300 ~ 2,
                                            gi_dar_x_axis_range > 300 & gi_dar_x_axis_range <= 540 ~ 3,
                                            gi_dar_x_axis_range > 540 ~ 4)
    
    if(filter_gi$gi_button_sector == "Agregados") {
      
      display_hoverinfo = ifelse(all(is.na(gi_tbl_week()$Horas_res)), 'none','text')
      
      p4 <- gi_tbl_week() %>%
        plot_ly(
          x = ~week,
          y = ~Horas_res,
          type = "scatter",
          mode = "lines+markers",
          text = ~paste0(str_to_title(format(week, format = "%d %b %Y")),'<br>Tempo: ', format(round(Horas_res, 1), decimal.mark = ","), ' horas'),
          hoverinfo = display_hoverinfo,
          showlegend = FALSE
        ) |>
        layout(
          shapes = list(
            list(
              type = "line",
              x0 = min(gi_tbl_week()$week, na.rm = TRUE),
              x1 = max(gi_tbl_week()$week, na.rm = TRUE),
              y0 = 24,
              y1 = 24,
              line = list(dash = "dash", color = "red")
            )
          ),
          xaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = gi_x_label_week(),
            type = "date",
            tickformat = "%d %b %Y",
            tickangle = 45,
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = gi_dar_x_tick_label_breaks * 7 * 24 * 60 * 60 * 1000  # semanas em ms #distância entre ticks
          ),
          yaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "Tempo (horas)",
            range = c(0, max_dif_isolamento_week_res$Horas_res)
          ),
          annotations = list(
            list(
              x = max(gi_tbl_week()$week, na.rm = TRUE),
              y = 26,
              text = "24 horas",
              showarrow = FALSE,
              font = list(color = "red", size = 12),
              xanchor = "right"
            )
          ),
          margin = list(l = 50, r = 50, t = 50, b = 50)
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
    } else {
      
      color_pallete <- viridis::viridis(length(unique(gi_tbl_sector_week()$DESCRIPTION)))
      
      display_hoverinfo = ifelse(all(is.na(gi_tbl_sector_week()$Horas_res)), 'none','text')
      
      p4 <- gi_tbl_sector_week() %>%
        plot_ly(
          x = ~week,
          y = ~Horas_res,
          color = ~DESCRIPTION,
          colors = color_pallete,
          type = "scatter",
          mode = "lines+markers",
          text = ~paste0(DESCRIPTION, ': ', format(round(Horas_res, 1), decimal.mark = ","), ' horas (',format(week, format = "%d %B %Y"),')'),
          hoverinfo = display_hoverinfo,
          showlegend = TRUE
        ) |>
        layout(
          shapes = list(
            list(
              type = "line",
              x0 = min(gi_tbl_sector_week()$week, na.rm = TRUE),
              x1 = max(gi_tbl_sector_week()$week, na.rm = TRUE),
              y0 = 24,
              y1 = 24,
              line = list(dash = "dash", color = "red")
            )
          ),
          xaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = gi_x_label_week(),
            hoverformat = "Semana: %d %B %Y",
            type = "date",
            tickformat = "%d %b %Y",
            tickangle = 45,
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = gi_dar_x_tick_label_breaks * 7 * 24 * 60 * 60 * 1000  # semanas em ms #distância entre ticks
          ),
          yaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "Tempo (horas)",
            range = c(0, max_dif_isolamento_week_res$Horas_res)
          )
          
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
      # Mostra legenda somente se existirem valores para os filtros selecionados
      if (!all(is.na(gi_tbl_sector_week()$Horas_res))) {
        p4 <- p4 %>%
          layout(
            annotations = list(
              list(
                x = max(gi_tbl_sector_week()$week, na.rm = TRUE),
                y = 26,
                text = "24 horas",
                showarrow = FALSE,
                font = list(color = "red", size = 12),
                xanchor = "right"
              )
            ),
            hovermode = "x unified",
            legend = list(
              traceorder = "grouped", # Agrupa itens na legenda
              font = list(family = "Open Sans", size = 12)
            ),
            margin = list(l = 50, r = 50, t = 50, b = 50)
          )
      }
      
    }
    
    return(p4)
    
  })
  
  output$gi_dif_res_amostra_result_plot <- renderUI({
    
    if(filter_gi$gi_button_time_setting == 'Mensal') {
      plotlyOutput("gi_dif_res_amostra_result_plot_month")
    } else {
      plotlyOutput("gi_dif_res_amostra_result_plot_week")
    }
    
  })
  
  # Saving Cost
  
  saving_giro_tbl_month <- reactive({
    
    if(nrow(saving_giro_tbl())==0) {
      data.frame(month = c(floor_date(filter_gi$gestao_isolamento_data[1], unit = 'month'), 
                           floor_date(filter_gi$gestao_isolamento_data[2], unit = 'month')),
                 saving_cost = NA)
    } else {
      
      saving_giro_tbl() %>% 
        group_by(month) |>
        summarise(saving_cost = sum(saving_cost, na.rm = T))  %>%
        ungroup() %>%
        complete(month = seq(floor_date(x_range_gi_month()[1], unit = 'month'), 
                             floor_date(x_range_gi_month()[2], unit = 'month'), by = "month"),
                 fill = list(saving_cost = 0))
    }
  })
  
  saving_giro_tbl_week <- reactive({
    
    if(nrow(saving_giro_tbl())==0) {
      data.frame(week = c(floor_date(filter_gi$gestao_isolamento_data[1], unit = 'week'), 
                          floor_date(filter_gi$gestao_isolamento_data[2], unit = 'week')),
                 saving_cost = NA)
    } else {
      
      saving_giro_tbl() %>% 
        group_by(week) |>
        summarise(saving_cost = sum(saving_cost, na.rm = T))  %>%
        ungroup() %>%
        complete(week = seq(floor_date(x_range_gi_week()[1], unit = 'week'), 
                            floor_date(x_range_gi_week()[2], unit = 'week'), by = "week"),
                 fill = list(saving_cost = 0))
    }
  })
  
  saving_giro_tbl_sector_month <- reactive({
    
    if(nrow(saving_giro_tbl())==0) {
      data.frame(month = c(floor_date(filter_gi$gestao_isolamento_data[1], unit = 'month'), 
                           floor_date(filter_gi$gestao_isolamento_data[2], unit = 'month')),
                 saving_cost = NA,
                 DESCRIPTION = NA)
    } else {
      
      saving_giro_tbl() %>% 
        group_by(month, DESCRIPTION) |>
        summarise(saving_cost = sum(saving_cost, na.rm = T))  %>%
        ungroup() %>%
        mutate(DESCRIPTION = factor(DESCRIPTION)) %>%
        complete(DESCRIPTION,
                 month = seq(floor_date(x_range_gi_month()[1], unit = 'month'), 
                                      floor_date(x_range_gi_month()[2], unit = 'month'), by = "month"),
                 fill = list(saving_cost = 0))
    }
  })
  
  saving_giro_tbl_sector_week <- reactive({
    
    if(nrow(saving_giro_tbl())==0) {
      data.frame(week = c(floor_date(filter_gi$gestao_isolamento_data[1], unit = 'week'), 
                          floor_date(filter_gi$gestao_isolamento_data[2], unit = 'week')),
                 saving_cost = NA,
                 DESCRIPTION = NA)
    } else {
      
      saving_giro_tbl() %>% 
        group_by(week, DESCRIPTION) |>
        summarise(saving_cost = sum(saving_cost, na.rm = T))  %>%
        ungroup() %>%
        mutate(DESCRIPTION = factor(DESCRIPTION)) %>%
        complete(DESCRIPTION,
                 week = seq(floor_date(x_range_gi_week()[1], unit = 'week'), 
                            floor_date(x_range_gi_week()[2], unit = 'week'), by = "week"),
                 fill = list(saving_cost = 0))
    }
  })
  
  output$saving_cost_ocupacao_leito_month <- renderPlotly({
    
    if(filter_gi$gi_button_sector == "Agregados") {

      if(all(is.na(saving_giro_tbl_month()$saving_cost))) {
        max_y_scm = 100
      } else {
        max_sc_value <- max(saving_giro_tbl_month()$saving_cost, na.rm = T)
        max_y_scm <- ifelse(max_sc_value < 100, 100, max_sc_value*1.1)
      }      
            
      if(all(is.na(saving_giro_tbl_month()$saving_cost))) {
        pgisc <- saving_giro_tbl_month() %>%
          plot_ly(
            x = ~month,
            y = ~saving_cost,
            hoverinfo = 'none',
            showlegend = FALSE
          )
        
      } else {
        pgisc <- saving_giro_tbl_month() %>%
          plot_ly(
            x = ~month,
            y = ~saving_cost,
            marker = list(color = '#008080'),
            line = list(color = '#008080'),
            type = "scatter",
            mode = "lines+markers",
            text = ~paste0(str_to_title(format(month, format = "%B %Y")),
                           '<br>Economia: ', format_currency_br(saving_cost)),
            hoverinfo = 'text',
            showlegend = FALSE
          )
      }
        
      pgisc <- pgisc %>%
        layout(
          xaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = gi_x_label_month(),
            type = "date",
            tickformat = "%b %Y",
            tickangle = 45,
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = "M1",  # One month intervals
            range = x_range_gi_month() + c(-15, 15)
          ),
          yaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "Valor economizado (Reais)",
            range = c(0, max_y_scm),
            tickformat = ",.2f"  # Format numbers as currency with 2 decimal places
          ),
          margin = list(l = 50, r = 50, t = 50, b = 50)
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
    } else {
      
      color_pallete_sc <- viridis::viridis(length(unique(saving_giro_tbl_sector_month()$DESCRIPTION)))
      
      display_hoverinfo_sc = ifelse(all(is.na(saving_giro_tbl_sector_month()$saving_cost)), 'none','text')
      
      if(all(is.na(saving_giro_tbl_sector_month()$saving_cost))) {
        max_y_scm = 100
      } else {
        max_sc_value <- max(saving_giro_tbl_sector_month()$saving_cost, na.rm = T)
        max_y_scm <- ifelse(max_sc_value < 100, 100, max_sc_value*1.1)
      } 
      
      pgisc <- saving_giro_tbl_sector_month() %>%
        plot_ly(
          x = ~month,
          y = ~saving_cost,
          color = ~DESCRIPTION,
          colors = color_pallete_sc,
          type = "scatter",
          mode = "lines+markers",
          text = ~paste0(DESCRIPTION, ': ', format_currency_br(saving_cost)),
          hoverinfo = display_hoverinfo_sc,
          showlegend = TRUE
        ) |>
        layout(
          xaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = gi_x_label_month(),
            type = "date",
            tickformat = "%b %Y",
            tickangle = 45,
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = "M1",  # One month intervals
            range = x_range_gi_month() + c(-15, 15)
          ),
          yaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "Valor economizado (Reais)",
            range = c(0, max_y_scm),
            tickformat = ",.2f"  # Format numbers as currency with 2 decimal places
          )
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
      # Mostra legenda somente se existirem valores para os filtros selecionados
      if (!all(is.na(saving_giro_tbl_sector_month()$saving_cost))) {
        pgisc <- pgisc %>%
          layout(
            hovermode = "x unified",
            legend = list(
              traceorder = "grouped", # Agrupa itens na legenda
              font = list(family = "Open Sans", size = 12)
            ),
            margin = list(l = 50, r = 50, t = 50, b = 50)
          )
      }
      
    }
    
    return(pgisc)
    
  })
  
  output$saving_cost_ocupacao_leito_week <- renderPlotly({
    
    gisc_x_axis_range <- as.numeric(diff(range(gi_tbl_week()$week)))
    gisc_x_tick_label_breaks <- case_when(gisc_x_axis_range <= 60 ~ 1,
                                          gisc_x_axis_range > 60 & gisc_x_axis_range <= 300 ~ 2,
                                          gisc_x_axis_range > 300 & gisc_x_axis_range <= 540 ~ 3,
                                          gisc_x_axis_range > 540 ~ 4)
    
    if(filter_gi$gi_button_sector == "Agregados") {
      
      if(all(is.na(saving_giro_tbl_week()$saving_cost))) {
        max_y_scm = 100
      } else {
        max_sc_value <- max(saving_giro_tbl_week()$saving_cost, na.rm = T)
        max_y_scm <- ifelse(max_sc_value < 100, 100, max_sc_value*1.1)
      }
      
      if(all(is.na(saving_giro_tbl_week()$saving_cost))) {
        pgisc <- saving_giro_tbl_week() %>%
          plot_ly(
            x = ~week,
            y = ~saving_cost,
            hoverinfo = 'none',
            showlegend = FALSE
          )
        
      } else {
        pgisc <- saving_giro_tbl_week() %>%
          plot_ly(
            x = ~week,
            y = ~saving_cost,
            marker = list(color = '#008080'),
            line = list(color = '#008080'),
            type = "scatter",
            mode = "lines+markers",
            text = ~paste0(str_to_title(format(week, format = "%d %B %Y")),
                           '<br>Economia: ', format_currency_br(saving_cost)),
            hoverinfo = 'text',
            showlegend = FALSE
          )
      }
      
      pgisc <- pgisc |>
        layout(
          xaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = gi_x_label_week(),
            type = "date",
            tickformat = "%b %Y",
            tickangle = 45,
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = gisc_x_tick_label_breaks * 7 * 24 * 60 * 60 * 1000,  # semanas em ms #distância entre ticks
            range = x_range_gi_week() + c(-15, 15)
          ),
          yaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "Valor economizado (Reais)",
            range = c(0, max_y_scm),
            tickformat = ",.2f"  # Format numbers as currency with 2 decimal places
          ),
          margin = list(l = 50, r = 50, t = 50, b = 50)
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
    } else {
      
      if(all(is.na(saving_giro_tbl_sector_week()$saving_cost))) {
        max_y_scm = 100
      } else {
        max_sc_value <- max(saving_giro_tbl_sector_week()$saving_cost, na.rm = T)
        max_y_scm <- ifelse(max_sc_value < 100, 100, max_sc_value*1.1)
      }
      
      color_pallete_sc <- viridis::viridis(length(unique(saving_giro_tbl_sector_week()$DESCRIPTION)))
      
      display_hoverinfo_sc = ifelse(all(is.na(saving_giro_tbl_sector_week()$saving_cost)), 'none','text')
      
      
      if(all(is.na(saving_giro_tbl_sector_week()$saving_cost))) {
        pgisc <- saving_giro_tbl_sector_week() %>%
          plot_ly(
            x = ~week,
            y = ~saving_cost,
            hoverinfo = 'none',
            showlegend = FALSE
          )
        
      } else {
        pgisc <- saving_giro_tbl_sector_week() %>%
          plot_ly(
            x = ~week,
            y = ~saving_cost,
            color = ~DESCRIPTION,
            colors = color_pallete_sc,
            type = "scatter",
            mode = "lines+markers",
            text = ~paste0(DESCRIPTION, ': ', format_currency_br(saving_cost),' (',format(week, format = "%d %B %Y"),')'),
            hoverinfo = display_hoverinfo_sc,
            showlegend = TRUE
          )
      }
      
      pgisc <- pgisc |>
        layout(
          xaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = gi_x_label_week(),
            type = "date",
            tickformat = "%d %b %Y",
            tickangle = 45,
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = gisc_x_tick_label_breaks * 7 * 24 * 60 * 60 * 1000,  # semanas em ms #distância entre ticks
            range = x_range_gi_week() + c(-15, 15)
          ),
          yaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "Valor economizado (Reais)",
            range = c(0, max_y_scm),
            tickformat = ",.2f"  # Format numbers as currency with 2 decimal places
          )
          
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
      # Mostra legenda somente se existirem valores para os filtros selecionados
      if (!all(is.na(saving_giro_tbl_sector_week()$saving_cost))) {
        pgisc <- pgisc %>%
          layout(
            hovermode = "x unified",
            legend = list(
              traceorder = "grouped", # Agrupa itens na legenda
              font = list(family = "Open Sans", size = 12)
            ),
            margin = list(l = 50, r = 50, t = 50, b = 50)
          )
      }
      
    }
    
    return(pgisc)
    
  })
  
  output$saving_cost_ocupacao_leito <- renderUI({
    
    if(filter_gi$gi_button_time_setting == 'Mensal') {
      plotlyOutput("saving_cost_ocupacao_leito_month")
    } else {
      plotlyOutput("saving_cost_ocupacao_leito_week")
    }
    
  })
  
  # Taxa de Rotatividade
  saving_giro_tx_rot_month_tbl <- reactive({
    
    if(nrow(gi_tbl_prev())==0) {
      data.frame(month = c(floor_date(filter_gi$gestao_isolamento_data[1], unit = 'month'), 
                           floor_date(filter_gi$gestao_isolamento_data[2], unit = 'month')),
                 tx_rotatividade = NA)
    } else {
      
      gi_tbl_prev() %>%
        filter(ISOLATED == "Sim") %>%
        mutate(RELEASE_DATE = as.Date(RELEASE_DATE)) %>%
        filter(RELEASE_DATE >= filter_gi$gestao_isolamento_data[1] &
                 RELEASE_DATE <= filter_gi$gestao_isolamento_data[2]) %>%
        left_join(bed_availability_tbl, by = c("RELEASE_DATE" = "bed_date")) %>%
        group_by(month) %>%
        reframe(tx_rotatividade = n()/mean(bed_n, na.rm = T)*100) %>%
        ungroup() %>%
        complete(month = seq(floor_date(x_range_gi_month()[1], unit = 'month'), 
                             floor_date(x_range_gi_month()[2], unit = 'month'), by = "month"),
                 fill = list(tx_rotatividade = 0))
    }
  })
  
  saving_giro_tx_rot_week_tbl <- reactive({
    
    if(nrow(gi_tbl_prev())==0) {
      data.frame(week = c(floor_date(filter_gi$gestao_isolamento_data[1], unit = 'week'), 
                          floor_date(filter_gi$gestao_isolamento_data[2], unit = 'week')),
                 tx_rotatividade = NA)
    } else {
      
      gi_tbl_prev() %>%
        filter(ISOLATED == "Sim") %>%
        mutate(RELEASE_DATE = as.Date(RELEASE_DATE)) %>%
        filter(RELEASE_DATE >= filter_gi$gestao_isolamento_data[1] &
                 RELEASE_DATE <= filter_gi$gestao_isolamento_data[2]) %>%
        left_join(bed_availability_tbl, by = c("RELEASE_DATE" = "bed_date")) %>%
        group_by(week) %>%
        reframe(tx_rotatividade = n()/mean(bed_n, na.rm = T)*100) %>%
        ungroup() %>%
        complete(week = seq(floor_date(x_range_gi_week()[1], unit = 'week'), 
                            floor_date(x_range_gi_week()[2], unit = 'week'), by = "week"),
                 fill = list(tx_rotatividade = 0))
    }
  })
  
  # By Sector
  
  saving_giro_tx_rot_sector_month_tbl <- reactive({
    
    if(nrow(gi_tbl_prev())==0) {
      data.frame(month = c(floor_date(filter_gi$gestao_isolamento_data[1], unit = 'month'), 
                           floor_date(filter_gi$gestao_isolamento_data[2], unit = 'month')),
                 tx_rotatividade = NA,
                 DESCRIPTION = NA)
    } else {
      
      gi_tbl_prev() %>%
        filter(ISOLATED == "Sim") %>%
        mutate(RELEASE_DATE = as.Date(RELEASE_DATE)) %>%
        filter(RELEASE_DATE >= filter_gi$gestao_isolamento_data[1] &
                 RELEASE_DATE <= filter_gi$gestao_isolamento_data[2]) %>%
        left_join(bed_availability_tbl, by = c("RELEASE_DATE" = "bed_date")) %>%
        group_by(month, DESCRIPTION) %>%
        reframe(tx_rotatividade = n()/mean(bed_n, na.rm = T)*100) %>%
        ungroup() %>%
        mutate(DESCRIPTION = factor(DESCRIPTION)) %>%
        complete(DESCRIPTION,
                 month = seq(floor_date(x_range_gi_month()[1], unit = 'month'), 
                             floor_date(x_range_gi_month()[2], unit = 'month'), by = "month"),
                 fill = list(tx_rotatividade = 0))
    }
  })
  
  saving_giro_tx_rot_sector_week_tbl <- reactive({
    
    if(nrow(gi_tbl_prev())==0) {
      data.frame(week = c(floor_date(filter_gi$gestao_isolamento_data[1], unit = 'week'), 
                          floor_date(filter_gi$gestao_isolamento_data[2], unit = 'week')),
                 tx_rotatividade = NA,
                 DESCRIPTION = NA)
    } else {
      
      gi_tbl_prev() %>%
        filter(ISOLATED == "Sim") %>%
        mutate(RELEASE_DATE = as.Date(RELEASE_DATE)) %>%
        filter(RELEASE_DATE >= filter_gi$gestao_isolamento_data[1] &
                 RELEASE_DATE <= filter_gi$gestao_isolamento_data[2]) %>%
        left_join(bed_availability_tbl, by = c("RELEASE_DATE" = "bed_date")) %>%
        group_by(week, DESCRIPTION) %>%
        reframe(tx_rotatividade = n()/mean(bed_n, na.rm = T)*100) %>%
        ungroup() %>%
        mutate(DESCRIPTION = factor(DESCRIPTION)) %>%
        complete(DESCRIPTION,
                 week = seq(floor_date(x_range_gi_week()[1], unit = 'week'), 
                            floor_date(x_range_gi_week()[2], unit = 'week'), by = "week"),
                 fill = list(tx_rotatividade = 0))
    }
  })
  
  #####
  
  output$tx_rotatividade_month_plot <- renderPlotly({
    
    if(filter_gi$gi_button_sector == "Agregados") {
      
      if(all(is.na(saving_giro_tx_rot_month_tbl()$tx_rotatividade))) {
        max_y_trm = 50
      } else {
        max_trm_value <- max(saving_giro_tx_rot_month_tbl()$tx_rotatividade, na.rm = T)
        max_y_trm <- ifelse(max_trm_value < 50, 50, max_trm_value*1.1)
      }
      
      if(all(is.na(saving_giro_tx_rot_month_tbl()$tx_rotatividade))) {
        ptr <- saving_giro_tx_rot_month_tbl() %>%
          plot_ly(
            x = ~month,
            y = ~tx_rotatividade,
            hoverinfo = 'none',
            showlegend = FALSE
          )
        
      } else {
        ptr <- saving_giro_tx_rot_month_tbl() %>%
          plot_ly(
            x = ~month,
            y = ~tx_rotatividade,
            marker = list(color = '#FFAF42'),
            line = list(color = '#FFAF42'),
            type = "scatter",
            mode = "lines+markers",
            text = ~paste0(str_to_title(format(month, format = "%B %Y")),
                           '<br>Taxa: ', format(round(tx_rotatividade,1), decimal.mark = ",")),
            hoverinfo = 'text',
            showlegend = FALSE
          )
      }
        
      ptr <- ptr %>%
        layout(
          xaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = gi_x_label_month(),
            type = "date",
            tickformat = "%b %Y",
            tickangle = 45,
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = "M1",  # One month intervals
            range = x_range_gi_month() + c(-15, 15)
          ),
          yaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "Taxa de Rotatividade",
            range = c(0, max_y_trm)
          ),
          margin = list(l = 50, r = 50, t = 50, b = 50)
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
    } else {
      
      color_pallete_trm <- viridis::viridis(length(unique(saving_giro_tx_rot_sector_month_tbl()$DESCRIPTION)))
      
      display_hoverinfo_trm = ifelse(all(is.na(saving_giro_tx_rot_sector_month_tbl()$tx_rotatividade)), 'none','text')
      
      if(all(is.na(saving_giro_tx_rot_sector_month_tbl()$tx_rotatividade))) {
        max_y_trm = 20
      } else {
        max_trm_value <- max(saving_giro_tx_rot_sector_month_tbl()$tx_rotatividade, na.rm = T)
        max_y_trm <- ifelse(max_trm_value < 20, 20, max_trm_value*1.1)
      }
      
      ptr <- saving_giro_tx_rot_sector_month_tbl() %>%
        plot_ly(
          x = ~month,
          y = ~tx_rotatividade,
          color = ~DESCRIPTION,
          colors = color_pallete_trm,
          type = "scatter",
          mode = "lines+markers",
          text = ~paste0(DESCRIPTION,': ', format(round(tx_rotatividade,1), decimal.mark = ",")),
          hoverinfo = display_hoverinfo_trm,
          showlegend = TRUE
        ) |>
        layout(
          xaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = gi_x_label_month(),
            type = "date",
            tickformat = "%b %Y",
            tickangle = 45,
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = "M1",  # One month intervals
            range = x_range_gi_month() + c(-15, 15)
          ),
          yaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "Taxa de Rotatividade",
            range = c(0, max_y_trm)
          )
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
      # Mostra legenda somente se existirem valores para os filtros selecionados
      if (!all(is.na(saving_giro_tx_rot_sector_month_tbl()$tx_rotatividade))) {
        ptr <- ptr %>%
          layout(
            hovermode = "x unified",
            legend = list(
              traceorder = "grouped", # Agrupa itens na legenda
              font = list(family = "Open Sans", size = 12)
            ),
            margin = list(l = 50, r = 50, t = 50, b = 50)
          )
      }
      
    }
    
    return(ptr)
    
  })
  
  output$tx_rotatividade_week_plot <- renderPlotly({
    
    trm_x_axis_range <- as.numeric(diff(range(gi_tbl_week()$week)))
    trm_x_tick_label_breaks <- case_when(trm_x_axis_range <= 60 ~ 1,
                                         trm_x_axis_range > 60 & trm_x_axis_range <= 300 ~ 2,
                                         trm_x_axis_range > 300 & trm_x_axis_range <= 540 ~ 3,
                                         trm_x_axis_range > 540 ~ 4)
    
    if(filter_gi$gi_button_sector == "Agregados") {
      
      if(all(is.na(saving_giro_tx_rot_week_tbl()$tx_rotatividade))) {
        max_y_trm = 10
      } else {
        max_trm_value <- max(saving_giro_tx_rot_week_tbl()$tx_rotatividade, na.rm = T)
        max_y_trm <- ifelse(max_trm_value < 10, 10, max_trm_value*1.1)
      }
      
      if(all(is.na(saving_giro_tx_rot_week_tbl()$tx_rotatividade))) {
        ptrm <- saving_giro_tx_rot_week_tbl() %>%
          plot_ly(
            x = ~week,
            y = ~tx_rotatividade,
            hoverinfo = 'none',
            showlegend = FALSE
          )
        
      } else {
        ptrm <- saving_giro_tx_rot_week_tbl() %>%
          plot_ly(
            x = ~week,
            y = ~tx_rotatividade,
            marker = list(color = '#FFAF42'),
            line = list(color = '#FFAF42'),
            type = "scatter",
            mode = "lines+markers",
            text = ~paste0(str_to_title(format(week, format = "%d %B %Y")),
                           '<br>Taxa: ', format(round(tx_rotatividade,1), decimal.mark = ",")),
            hoverinfo = 'text',
            showlegend = FALSE
          )
      }
      
      ptrm <- ptrm |>
        layout(
          xaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = gi_x_label_week(),
            type = "date",
            tickformat = "%d %b %Y",
            tickangle = 45,
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = trm_x_tick_label_breaks * 7 * 24 * 60 * 60 * 1000,  # semanas em ms #distância entre ticks
            range = x_range_gi_week() + c(-15, 15)
          ),
          yaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "Taxa de Rotatividade",
            range = c(0, max_y_trm)
          ),
          margin = list(l = 50, r = 50, t = 50, b = 50)
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
    } else {
      
      if(all(is.na(saving_giro_tbl_sector_week()$tx_rotatividade))) {
        max_y_trm = 15
      } else {
        max_trm_value <- max(saving_giro_tbl_sector_week()$tx_rotatividade, na.rm = T)
        max_y_trm <- ifelse(max_trm_value < 15, 15, max_trm_value*1.1)
      }
      
      color_pallete_trm <- viridis::viridis(length(unique(saving_giro_tx_rot_sector_week_tbl()$DESCRIPTION)))
      
      if(all(is.na(saving_giro_tx_rot_sector_week_tbl()$tx_rotatividade))) {
        ptrm <- saving_giro_tx_rot_sector_week_tbl() %>%
          plot_ly(
            x = ~week,
            y = ~tx_rotatividade,
            hoverinfo = 'none',
            showlegend = FALSE
          )
        
      } else {
        ptrm <- saving_giro_tx_rot_sector_week_tbl() %>%
          plot_ly(
            x = ~week,
            y = ~tx_rotatividade,
            color = ~DESCRIPTION,
            colors = color_pallete_trm,
            type = "scatter",
            mode = "lines+markers",
            text = ~paste0(DESCRIPTION, ': ', tx_rotatividade,' (',format(week, format = "%d %B %Y"),')'),
            hoverinfo = 'text',
            showlegend = TRUE
          )
      }
      
      ptrm <- ptrm |>
        layout(
          xaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = gi_x_label_week(),
            type = "date",
            tickformat = "%d %b %Y",
            tickangle = 45,
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = trm_x_tick_label_breaks * 7 * 24 * 60 * 60 * 1000,  # semanas em ms #distância entre ticks
            range = x_range_gi_week() + c(-15, 15)
          ),
          yaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "Taxa de Rotatividade",
            range = c(0, max_y_trm)
          )
          
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
      # Mostra legenda somente se existirem valores para os filtros selecionados
      if (!all(is.na(saving_giro_tbl_sector_week()$tx_rotatividade))) {
        ptrm <- ptrm %>%
          layout(
            hovermode = "x unified",
            legend = list(
              traceorder = "grouped", # Agrupa itens na legenda
              font = list(family = "Open Sans", size = 12)
            ),
            margin = list(l = 50, r = 50, t = 50, b = 50)
          )
      }
      
    }
    
    return(ptrm)
    
  })
  
  output$tx_rotatividade_ocupacao_leito <- renderUI({
    
    if(filter_gi$gi_button_time_setting == 'Mensal') {
      plotlyOutput("tx_rotatividade_month_plot")
    } else {
      plotlyOutput("tx_rotatividade_week_plot")
    }
    
  })
  
  ## Tempo Médio de Ocupação do Leito
  
  # Tempo Médio de Ocupação do Leito
  
  gitm_month_tbl <- reactive({
    
    if(nrow(gi_tbl_prev())==0) {
      data.frame(month = c(floor_date(filter_gi$gestao_isolamento_data[1], unit = 'month'), 
                           floor_date(filter_gi$gestao_isolamento_data[2], unit = 'month')),
                 tm_ocupacao_leito = NA)
    } else {
      
      gi_tbl_prev() %>%
        # Considera somente pacientes que internaram dentro do período selecionado
        filter(ADMISSION_DATE >= filter_gi$gestao_isolamento_data[1]) %>%
        # Considera somente pacientes que receberam alta até o período selecionado
        filter(RELEASE_DATE <= filter_gi$gestao_isolamento_data[2]) %>%
        # Considera somente pacientes que estavam isolados, e foram liberados
        filter(ISOLATED == "Sim") %>%
        # COnsidera o mês de entrada do paciente
        mutate(month = as.Date(floor_date(ADMISSION_DATE, unit = 'month'))) %>%
        group_by(month) %>%
        reframe(tm_ocupacao_leito = round(sum(bed_occupation_days, na.rm = T)/n(),2)) %>%
        ungroup() %>%
        complete(month = seq(floor_date(x_range_gi_month()[1], unit = 'month'), 
                             floor_date(x_range_gi_month()[2], unit = 'month'), by = "month"),
                 fill = list(tm_ocupacao_leito = 0))
    }
  })
  
  gitm_week_tbl <- reactive({
    
    if(nrow(gi_tbl_prev())==0) {
      data.frame(week = c(floor_date(filter_gi$gestao_isolamento_data[1], unit = 'week'), 
                          floor_date(filter_gi$gestao_isolamento_data[2], unit = 'week')),
                 tm_ocupacao_leito = NA)
    } else {
      
      gi_tbl_prev() %>%
        # Considera somente pacientes que internaram dentro do período selecionado
        filter(ADMISSION_DATE >= filter_gi$gestao_isolamento_data[1]) %>%
        # Considera somente pacientes que receberam alta até o período selecionado
        filter(RELEASE_DATE <= filter_gi$gestao_isolamento_data[2]) %>%
        # Considera somente pacientes que estavam isolados, e foram liberados
        filter(ISOLATED == "Sim") %>%
        # COnsidera o mês de entrada do paciente
        mutate(week = as.Date(floor_date(ADMISSION_DATE, unit = 'week'))) %>%
        group_by(week) %>%
        reframe(tm_ocupacao_leito = round(sum(bed_occupation_days, na.rm = T)/n(),2)) %>%
        ungroup() %>%
        complete(week = seq(floor_date(x_range_gi_week()[1], unit = 'week'), 
                            floor_date(x_range_gi_week()[2], unit = 'week'), by = "week"),
                 fill = list(tm_ocupacao_leito = 0))
    }
  })
  
  # By Sector
  
  gitm_sector_month_tbl <- reactive({
    
    if(nrow(gi_tbl_prev())==0) {
      data.frame(month = c(floor_date(filter_gi$gestao_isolamento_data[1], unit = 'month'), 
                           floor_date(filter_gi$gestao_isolamento_data[2], unit = 'month')),
                 tm_ocupacao_leito = NA,
                 DESCRIPTION = NA)
    } else {
      
      gi_tbl_prev() %>%
        # Considera somente pacientes que internaram dentro do período selecionado
        filter(ADMISSION_DATE >= filter_gi$gestao_isolamento_data[1]) %>%
        # Considera somente pacientes que receberam alta até o período selecionado
        filter(RELEASE_DATE <= filter_gi$gestao_isolamento_data[2]) %>%
        # Considera somente pacientes que estavam isolados, e foram liberados
        filter(ISOLATED == "Sim") %>%
        # COnsidera o mês de entrada do paciente
        mutate(month = as.Date(floor_date(ADMISSION_DATE, unit = 'month'))) %>%
        group_by(month, DESCRIPTION) %>%
        reframe(tm_ocupacao_leito = round(sum(bed_occupation_days, na.rm = T)/n(),2)) %>%
        ungroup() %>%
        mutate(DESCRIPTION = factor(DESCRIPTION)) %>%
        complete(DESCRIPTION,
                 month = seq(floor_date(x_range_gi_month()[1], unit = 'month'), 
                             floor_date(x_range_gi_month()[2], unit = 'month'), by = "month"),
                 fill = list(tm_ocupacao_leito = 0))
    }
  })
  
  gitm_sector_week_tbl <- reactive({
    
    if(nrow(gi_tbl_prev())==0) {
      data.frame(week = c(floor_date(filter_gi$gestao_isolamento_data[1], unit = 'week'), 
                          floor_date(filter_gi$gestao_isolamento_data[2], unit = 'week')),
                 tm_ocupacao_leito = NA,
                 DESCRIPTION = NA)
    } else {
      
      gi_tbl_prev() %>%
        # Considera somente pacientes que internaram dentro do período selecionado
        filter(ADMISSION_DATE >= filter_gi$gestao_isolamento_data[1]) %>%
        # Considera somente pacientes que receberam alta até o período selecionado
        filter(RELEASE_DATE <= filter_gi$gestao_isolamento_data[2]) %>%
        # Considera somente pacientes que estavam isolados, e foram liberados
        filter(ISOLATED == "Sim") %>%
        # COnsidera o mês de entrada do paciente
        mutate(week = as.Date(floor_date(ADMISSION_DATE, unit = 'week'))) %>%
        group_by(week, DESCRIPTION) %>%
        reframe(tm_ocupacao_leito = round(sum(bed_occupation_days, na.rm = T)/n(),2)) %>%
        ungroup() %>%
        mutate(DESCRIPTION = factor(DESCRIPTION)) %>%
        complete(DESCRIPTION,
                 week = seq(floor_date(x_range_gi_week()[1], unit = 'week'), 
                            floor_date(x_range_gi_week()[2], unit = 'week'), by = "week"),
                 fill = list(tm_ocupacao_leito = 0))
    }
  })
  
  
  
  ####
  
  output$tm_ocupacao_leito_month_plot <- renderPlotly({
    
    if(filter_gi$gi_button_sector == "Agregados") {
      
      if(all(is.na(gitm_month_tbl()$tm_ocupacao_leito))) {
        max_y_tm = 1
      } else {
        max_tm_value <- max(gitm_month_tbl()$tm_ocupacao_leito, na.rm = T)
        max_y_tm <- ifelse(max_tm_value < 1, 1, max_tm_value*1.1)
      }
      
      if(all(is.na(gitm_month_tbl()$tm_ocupacao_leito))) {
        ptm <- gitm_month_tbl() %>%
          plot_ly(
            x = ~month,
            y = ~tm_ocupacao_leito,
            hoverinfo = 'none',
            showlegend = FALSE
          )
        
      } else {
        ptm <- gitm_month_tbl() %>%
          plot_ly(
            x = ~month,
            y = ~tm_ocupacao_leito,
            marker = list(color = '#0667B5'),
            line = list(color = '#0667B5'),
            type = "scatter",
            mode = "lines+markers",
            text = ~paste0(str_to_title(format(month, format = "%B %Y")),
                           '<br>Tempo Médio: ', format(round(tm_ocupacao_leito,1), decimal.mark = ",")),
            hoverinfo = 'text',
            showlegend = FALSE
          )
      }
      
      ptm <- ptm %>%
        layout(
          xaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "Mês de Entrada do Paciente",
            type = "date",
            tickformat = "%b %Y",
            tickangle = 45,
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = "M1",  # One month intervals
            range = x_range_gi_month() + c(-15, 15)
          ),
          yaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "Média de dias de ocupação do leito",
            range = c(0, max_y_tm)
          ),
          margin = list(l = 50, r = 50, t = 50, b = 50)
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
    } else {
      
      color_pallete_trm <- viridis::viridis(length(unique(gitm_sector_month_tbl()$DESCRIPTION)))
      
      display_hoverinfo_tm = ifelse(all(is.na(gitm_sector_month_tbl()$tm_ocupacao_leito)), 'none','text')
      
      if(all(is.na(gitm_sector_month_tbl()$tm_ocupacao_leito))) {
        max_y_tm = 1
      } else {
        max_tm_value <- max(gitm_sector_month_tbl()$tm_ocupacao_leito, na.rm = T)
        max_y_tm <- ifelse(max_tm_value < 1, 1, max_tm_value*1.1)
      }
      
      ptm <- gitm_sector_month_tbl() %>%
        plot_ly(
          x = ~month,
          y = ~tm_ocupacao_leito,
          color = ~DESCRIPTION,
          colors = color_pallete_trm,
          type = "scatter",
          mode = "lines+markers",
          text = ~paste0(DESCRIPTION,': ', format(round(tm_ocupacao_leito,1), decimal.mark = ","), ' dias'),
          hoverinfo = display_hoverinfo_tm,
          showlegend = TRUE
        ) |>
        layout(
          xaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "Mês de Entrada do Paciente",
            type = "date",
            tickformat = "%b %Y",
            tickangle = 45,
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = "M1",  # One month intervals
            range = x_range_gi_month() + c(-15, 15)
          ),
          yaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "Média de dias de ocupação do leito",
            range = c(0, max_y_tm)
          )
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
      # Mostra legenda somente se existirem valores para os filtros selecionados
      if (!all(is.na(gitm_sector_month_tbl()$tm_ocupacao_leito))) {
        ptm <- ptm %>%
          layout(
            hovermode = "x unified",
            legend = list(
              traceorder = "grouped", # Agrupa itens na legenda
              font = list(family = "Open Sans", size = 12)
            ),
            margin = list(l = 50, r = 50, t = 50, b = 50)
          )
      }
      
    }
    
    return(ptm)
    
  })
  
  output$tm_ocupacao_leito_week_plot <- renderPlotly({
    
    tmw_x_axis_range <- as.numeric(diff(range(gitm_week_tbl()$week)))
    tmw_x_tick_label_breaks <- case_when(tmw_x_axis_range <= 60 ~ 1,
                                         tmw_x_axis_range > 60 & tmw_x_axis_range <= 300 ~ 2,
                                         tmw_x_axis_range > 300 & tmw_x_axis_range <= 540 ~ 3,
                                         tmw_x_axis_range > 540 ~ 4)
    
    if(filter_gi$gi_button_sector == "Agregados") {
      
      if(all(is.na(gitm_week_tbl()$tm_ocupacao_leito))) {
        max_y_tm = 1
      } else {
        max_tm_value <- max(gitm_week_tbl()$tm_ocupacao_leito, na.rm = T)
        max_y_tm <- ifelse(max_tm_value < 1, 1, max_tm_value*1.1)
      }
      
      if(all(is.na(gitm_week_tbl()$tm_ocupacao_leito))) {
        ptmm <- gitm_week_tbl() %>%
          plot_ly(
            x = ~week,
            y = ~tm_ocupacao_leito,
            hoverinfo = 'none',
            showlegend = FALSE
          )
        
      } else {
        ptmm <- gitm_week_tbl() %>%
          plot_ly(
            x = ~week,
            y = ~tm_ocupacao_leito,
            marker = list(color = '#0667B5'),
            line = list(color = '#0667B5'),
            type = "scatter",
            mode = "lines+markers",
            text = ~paste0(str_to_title(format(week, format = "%d %B %Y")),
                           '<br>Tempo Médio: ', format(round(tm_ocupacao_leito,1), decimal.mark = ","), ' dias'),
            hoverinfo = 'text',
            showlegend = FALSE
          )
      }
      
      ptmm <- ptmm |>
        layout(
          xaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = gi_x_label_week(),
            type = "date",
            tickformat = "%d %b %Y",
            tickangle = 45,
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = tmw_x_tick_label_breaks * 7 * 24 * 60 * 60 * 1000,  # semanas em ms #distância entre ticks
            range = x_range_gi_week() + c(-15, 15)
          ),
          yaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "Média de dias de ocupação do leito",
            range = c(0, max_y_tm)
          ),
          margin = list(l = 50, r = 50, t = 50, b = 50)
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
    } else {
      
      if(all(is.na(gitm_sector_week_tbl()$tm_ocupacao_leito))) {
        max_y_tm = 1
      } else {
        max_tm_value <- max(gitm_sector_week_tbl()$tm_ocupacao_leito, na.rm = T)
        max_y_tm <- ifelse(max_tm_value < 1, 1, max_tm_value*1.1)
      }
      
      color_pallete_trm <- viridis::viridis(length(unique(gitm_sector_week_tbl()$DESCRIPTION)))
      
      if(all(is.na(gitm_sector_week_tbl()$tm_ocupacao_leito))) {
        ptmm <- gitm_sector_week_tbl() %>%
          plot_ly(
            x = ~week,
            y = ~tm_ocupacao_leito,
            hoverinfo = 'none',
            showlegend = FALSE
          )
        
      } else {
        ptmm <- gitm_sector_week_tbl() %>%
          plot_ly(
            x = ~week,
            y = ~tm_ocupacao_leito,
            color = ~DESCRIPTION,
            colors = color_pallete_trm,
            type = "scatter",
            mode = "lines+markers",
            text = ~paste0(DESCRIPTION, ': ', tm_ocupacao_leito,' dias (',format(week, format = "%d %B %Y"),')'),
            hoverinfo = 'text',
            showlegend = TRUE
          )
      }
      
      ptmm <- ptmm |>
        layout(
          xaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = gi_x_label_week(),
            type = "date",
            tickformat = "%d %b %Y",
            tickangle = 45,
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = tmw_x_tick_label_breaks * 7 * 24 * 60 * 60 * 1000,  # semanas em ms #distância entre ticks
            range = x_range_gi_week() + c(-15, 15)
          ),
          yaxis = list(
            showline= T, linewidth=1, linecolor='black',
            title = "Média de dias de ocupação do leito",
            range = c(0, max_y_tm)
          )
          
        ) %>%
        config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
      
      # Mostra legenda somente se existirem valores para os filtros selecionados
      if (!all(is.na(gitm_sector_week_tbl()$tm_ocupacao_leito))) {
        ptmm <- ptmm %>%
          layout(
            hovermode = "x unified",
            legend = list(
              traceorder = "grouped", # Agrupa itens na legenda
              font = list(family = "Open Sans", size = 12)
            ),
            margin = list(l = 50, r = 50, t = 50, b = 50)
          )
      }
      
    }
    
    return(ptmm)
    
  })
  
  output$tm_ocupacao_leito_plot <- renderUI({
    
    if(filter_gi$gi_button_time_setting == 'Mensal') {
      plotlyOutput("tm_ocupacao_leito_month_plot")
    } else {
      plotlyOutput("tm_ocupacao_leito_week_plot")
    }
    
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
  
  
  antimicrob_date_range <- reactive({
    as.Date(floor_date(filter_mic$date_range, unit = 'month'))
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
                      ADM_START_MONTH=seq(min(ADM_START_MONTH), 
                                          max(ADM_START_MONTH), by="month"),
                      fill = list(DDD = 0)) %>%
      left_join(antimicrob_tbl_codes, by = "MEDICATION_NAME")
  })
  
  output$ddd_plot_month <- renderPlotly({
    
    if(nrow(DDD_month_tbl())==0) {
      DDD_month_tbl_ <- data.frame(ADM_START_MONTH = as.Date("2024-01-01"),
                                   DDD = NA,
                                   MEDICATION_NAME = NA)
    } else {
      DDD_month_tbl_ <- DDD_month_tbl()
    }
    
    # Se desejar, definir a paleta de cores aqui
    #color_pallete_ddd <- rainbow(length(unique(DDD_month_tbl_$MEDICATION_NAME)))
    
    display_hoverinfo_ddd = ifelse(all(is.na(DDD_month_tbl_$DDD)), 'none','text')
    
    if(all(is.na(DDD_month_tbl_$DDD))) {
      max_y_ddd = 10
    } else {
      max_ddd_value <- max(DDD_month_tbl_$DDD, na.rm = T)
      max_y_ddd <- ifelse(max_ddd_value < 10, 10, max_ddd_value*1.1)
    }
    
    pdddm <- DDD_month_tbl_ %>%
      plot_ly(
        x = ~ADM_START_MONTH,
        y = ~DDD,
        color = ~MEDICATION_NAME,
        #colors = color_pallete_ddd, # Se desejar, definir a paleta de cores aqui
        type = "scatter",
        mode = "lines+markers",
        text = ~paste0(MEDICATION_NAME,': ', format(round(DDD,2), decimal.mark = ",")),
        hoverinfo = display_hoverinfo_ddd,
        showlegend = TRUE
      ) |>
      layout(
        xaxis = list(
          showline= T, linewidth=1, linecolor='black',
          title = "Mês de Entrada do Paciente",
          type = "date",
          tickformat = "%b %Y",
          tickangle = 45,
          tickmode = "linear",  # Define espaçamento uniforme
          dtick = "M1",  # One month intervals
          range = antimicrob_date_range() + c(-15, 15)
        ),
        yaxis = list(
          showline= T, linewidth=1, linecolor='black',
          title = "g/1000 pacientes-dia",
          range = c(0, max_y_ddd)
        )
      ) %>%
      config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
    
    # Mostra legenda somente se existirem valores para os filtros selecionados
    if (!all(is.na(DDD_month_tbl_$DDD))) {
      pdddm <- pdddm %>%
        layout(
          hovermode = "x unified",
          legend = list(
            traceorder = "grouped", # Agrupa itens na legenda
            font = list(family = "Open Sans", size = 12)
          ),
          margin = list(l = 50, r = 50, t = 50, b = 50)
        )
    }
    
    return(pdddm)
    
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
    
    if(nrow(DOT_month_tbl())==0) {
      DOT_month_tbl_ <- data.frame(ADM_START_MONTH = as.Date("2024-01-01"),
                                   DOT = NA,
                                   MEDICATION_NAME = NA)
    } else {
      DOT_month_tbl_ <- DOT_month_tbl()
    }
    
    # Se desejar, definir a paleta de cores aqui
    #color_pallete_dot <- rainbow(length(unique(DOT_month_tbl_$MEDICATION_NAME)))
    
    display_hoverinfo_dot = ifelse(all(is.na(DOT_month_tbl_$DOT)), 'none','text')
    
    if(all(is.na(DOT_month_tbl_$DOT))) {
      max_y_dot = 10
    } else {
      max_dot_value <- max(DOT_month_tbl_$DOT, na.rm = T)
      max_y_dot <- ifelse(max_dot_value < 10, 10, max_dot_value*1.1)
    }
    
    pdotm <- DOT_month_tbl_ %>%
      plot_ly(
        x = ~ADM_START_MONTH,
        y = ~DOT,
        color = ~MEDICATION_NAME,
        #colors = color_pallete_dot, # Se desejar, definir a paleta de cores aqui
        type = "scatter",
        mode = "lines+markers",
        text = ~paste0(MEDICATION_NAME,': ', format(round(DOT,1), decimal.mark = ",")),
        hoverinfo = display_hoverinfo_dot,
        showlegend = TRUE
      ) |>
      layout(
        xaxis = list(
          showline= T, linewidth=1, linecolor='black',
          title = "Mês de Entrada do Paciente",
          type = "date",
          tickformat = "%b %Y",
          tickangle = 45,
          tickmode = "linear",  # Define espaçamento uniforme
          dtick = "M1",  # One month intervals
          range = antimicrob_date_range() + c(-15, 15)
        ),
        yaxis = list(
          showline= T, linewidth=1, linecolor='black',
          title = "dias/1000 pacientes-dia",
          range = c(0, max_y_dot)
        )
      ) %>%
      config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
    
    # Mostra legenda somente se existirem valores para os filtros selecionados
    if (!all(is.na(DOT_month_tbl_$DOT))) {
      pdotm <- pdotm %>%
        layout(
          hovermode = "x unified",
          legend = list(
            traceorder = "grouped", # Agrupa itens na legenda
            font = list(family = "Open Sans", size = 12)
          ),
          margin = list(l = 50, r = 50, t = 50, b = 50)
        )
    }
    
    return(pdotm)
    
  })
  # output$dot_plot_month <- renderPlotly({ 
  #   
  #   max_DOT <- max(DOT_month_tbl()$DOT)*1.1
  #   
  #   plot_month_dot <- DOT_month_tbl() %>%
  #     ggplot(aes(x = ADM_START_MONTH,
  #                y = DOT,
  #                color = MEDICATION_NAME)) +
  #     geom_point(aes(text = paste0(str_to_title(format(ADM_START_MONTH, format = "%b, %Y")),
  #                                  '<br>',MEDICATION_NAME,
  #                                  '<br>DOT: ',round(DOT,2)
  #     ))) +
  #     geom_line() +
  #     theme_minimal() +
  #     theme(legend.position = "none") +
  #     xlab("") +
  #     ylab("g/1000 pacientes-dia") +
  #     # labs(title = "DOT (Dose Diária Definida)") +
  #     scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
  #     scale_y_continuous(limits = c(0,max_DOT), expand = c(0, 0)) +
  #     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
  #           axis.line = element_line(),
  #           plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches"))
  #   
  #   ggplotly(plot_month_dot, tooltip = "text")
  # })
  
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
    
    if(nrow(LOT_month_tbl())==0) {
      LOT_month_tbl_ <- data.frame(ADM_START_MONTH = as.Date("2024-01-01"),
                                   LOT = NA,
                                   DESCRIPTION = NA)
    } else {
      LOT_month_tbl_ <- LOT_month_tbl()
    }
    
    # Se desejar, definir a paleta de cores aqui
    #color_pallete_lot <- rainbow(length(unique(LOT_month_tbl_$DESCRIPTION)))
    
    display_hoverinfo_lot = ifelse(all(is.na(LOT_month_tbl_$LOT)), 'none','text')
    
    if(all(is.na(LOT_month_tbl_$LOT))) {
      max_y_lot = 10
    } else {
      max_lot_value <- max(LOT_month_tbl_$LOT, na.rm = T)
      max_y_lot <- ifelse(max_lot_value < 10, 10, max_lot_value*1.1)
    }
    
    plotm <- LOT_month_tbl_ %>%
      plot_ly(
        x = ~ADM_START_MONTH,
        y = ~LOT,
        color = ~DESCRIPTION,
        #colors = color_pallete_lot, # Se desejar, definir a paleta de cores aqui
        type = "scatter",
        mode = "lines+markers",
        text = ~paste0(DESCRIPTION,': ', format(round(LOT,1), decimal.mark = ",")),
        hoverinfo = display_hoverinfo_lot,
        showlegend = TRUE
      ) |>
      layout(
        xaxis = list(
          showline= T, linewidth=1, linecolor='black',
          title = "Mês de Entrada do Paciente",
          type = "date",
          tickformat = "%b %Y",
          tickangle = 45,
          tickmode = "linear",  # Define espaçamento uniforme
          dtick = "M1",  # One month intervals
          range = antimicrob_date_range() + c(-15, 15)
        ),
        yaxis = list(
          showline= T, linewidth=1, linecolor='black',
          title = "dias/1000 pacientes-dia",
          range = c(0, max_y_lot)
        )
      ) %>%
      config(locale = "pt-BR") # Set the locale to Brazilian Portuguese
    
    # Mostra legenda somente se existirem valores para os filtros selecionados
    if (!all(is.na(LOT_month_tbl_$LOT))) {
      plotm <- plotm %>%
        layout(
          hovermode = "x unified",
          legend = list(
            traceorder = "grouped", # Agrupa itens na legenda
            font = list(family = "Open Sans", size = 12)
          ),
          margin = list(l = 50, r = 50, t = 50, b = 50)
        )
    }
    
    return(plotm)
    
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
          xaxis = list(showline= T, linewidth=1, linecolor='black',showline= T, linewidth=1, linecolor='black',
            title = "",
            tickangle = 45,
            type = "date",
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = "M1",  # One month intervals
            tickformat = "%b %Y",  # Full month name and year
            range = range_x_month + c(-15,-1)
          ),
          yaxis = list(showline= T, linewidth=1, linecolor='black',
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
          xaxis = list(showline= T, linewidth=1, linecolor='black',
            title = "",
            tickangle = 45,
            type = "date",
            tickmode = "linear",  # Define espaçamento uniforme
            dtick = x_tick_label_breaks * 7 * 24 * 60 * 60 * 1000,  # semanas em ms
            tickformat = "%d %b %Y",  # Formato dos rótulos
            range = range_x_week + c(-5,-1)
          ),
          yaxis = list(showline= T, linewidth=1, linecolor='black',
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
        hovertemplate = ~paste0("Semana: ", format(ALERT_WEEK_PT, format = "%d-%m-%y"), "<br>", ALERT_ADHERENCE,": ", tempo_resposta_medio,"<extra></extra>")
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
        hovertemplate = ~paste0("Semana: ", format(IMPLEMENTATION_WEEK_PT, format = "%d-%m-%y"), "<br>", IMPLEMENTATION_ADHERENCE,": ", tempo_resposta_medio,"<extra></extra>")
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
      "0,0%"
    } else {
    acpt_ratio_value <- saving_int_tbl() %>%
      reframe(ratio = round(sum(IMPLEMENTATION_ADHERENCE)/n()*100,1)) %>%
      unlist()
    
    paste0(format(acpt_ratio_value, decimal.mark = ","), "%") 
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
        xaxis = list(showline= T, linewidth=1, linecolor='black',
          title = "",
          tickmode = "linear",
          ticklen = 10,              # Increase tick length to push labels further
          tickfont = list(size = 14) # Define espaçamento uniforme
        ),
        yaxis = list(showline= T, linewidth=1, linecolor='black',
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
        xaxis = list(showline= T, linewidth=1, linecolor='black',
          title = "",
          tickmode = "linear",  # Define espaçamento uniforme
          ticklen = 10,              # Increase tick length to push labels further
          tickfont = list(size = 14) # Define espaçamento uniforme
        ),
        yaxis = list(showline= T, linewidth=1, linecolor='black',
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
    x_lim_upp_cost_a <- ifelse(x_lim_upp_cost_a < 100, 100, x_lim_upp_cost_a)

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

atb_cost_tbl_prev <- reactive({
  
  atb_cost_tbl_filter_date <- antimicrob_tbl_final %>%
    dplyr::filter(THERAPY_START_DATE >= filter_atb_cost$atb_cost_date[1] &
                    THERAPY_START_DATE <= filter_atb_cost$atb_cost_date[2])
  
  if (is.null(filter_atb_cost$atb_cost_medico_id)) {
    atb_cost_tbl_filter_practitioner <- atb_cost_tbl_filter_date
  } else {
    atb_cost_tbl_filter_practitioner <- atb_cost_tbl_filter_date %>%
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

atb_cost_tbl <- reactive({ 
  if (is.null(filter_atb_cost$atb_cost_setor)) {
    atb_cost_tbl_filter_sector <- atb_cost_tbl_prev()
  } else {
    atb_cost_tbl_filter_sector <- atb_cost_tbl_prev() %>%
      dplyr::filter(DESCRIPTION %in% filter_atb_cost$atb_cost_setor)
  }
    atb_cost_tbl_filter_sector
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

output$atb_cost_sector_plot <- renderPlotly({
  
  # Opção A: Filtrar medicamentos
  
  if(input$atb_cost_plot_button == "Ver somente itens selecionados") {
    
    plot_atb_cost <- atb_cost_tbl() %>%
      group_by(DESCRIPTION) %>%
      reframe(TOTAL_COST = sum(TOTAL_COST, na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(desc(TOTAL_COST)) %>%
      ggplot(aes(y = fct_reorder(DESCRIPTION, TOTAL_COST), x = TOTAL_COST,
                 labels = "",
                 text = paste0(DESCRIPTION,
                               '<br>Custo total: ', format_currency_br(TOTAL_COST)))) +
      geom_col(fill = "#1F77B4") +
      theme_minimal() +
      ylab("") +
      xlab("Custo total (R$)") +
      theme(axis.line.y = element_line()) +
      scale_x_continuous(expand = c(0, 0),
                         labels = label_number(
                           accuracy = 0.01,
                           big.mark = ".", 
                           decimal.mark = ","
                         ))
    
    ggplotly(plot_atb_cost, tooltip = "text")
  } else { 
    # Opção B: Highlight nos medicamentos selecionados
    plot_atb_cost2 =  atb_cost_tbl_prev() %>%
      group_by(DESCRIPTION) %>%
      reframe(TOTAL_COST = sum(TOTAL_COST, na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(desc(TOTAL_COST)) %>%
      mutate(highlight_sector = ifelse(DESCRIPTION %in% filter_atb_cost$atb_cost_setor, T, F)) %>%
      ggplot(aes(y = fct_reorder(DESCRIPTION, TOTAL_COST), x = TOTAL_COST,
                 fill = highlight_sector,
                 labels = "",
                 text = paste0(DESCRIPTION,
                               '<br>Custo total: ', format_currency_br(TOTAL_COST)))) +
      geom_col() +
      scale_fill_manual(values = c("#1F77B4", "orange")) +
      theme_minimal() +
      ylab("") +
      xlab("Custo total (R$)") +
      theme(axis.line.y = element_line(),
            legend.position = 'none') +
      scale_x_continuous(expand = c(0, 0),
                         labels = label_number(
                           accuracy = 0.01,
                           big.mark = ".", 
                           decimal.mark = ","
                         ))
    
    ggplotly(plot_atb_cost2, tooltip = "text")
  }
})

}

shinyApp(ui, server)