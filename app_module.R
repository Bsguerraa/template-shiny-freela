# Carregar todas as bibliotecas
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
source("R/read_all_modules.R")
Sys.setlocale('LC_ALL', 'pt_BR.UTF-8')

# LENDO A PUB_KEY - Deixe essa parte em branco
# PUBKEY <- "-----BEGIN PUBLIC KEY-----
# -----END PUBLIC KEY-----"
# #pub_key <- read_pubkey(PUBKEY)

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
        ### FILTROS DA PAGINA 1 ##
        
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
        ### FILTROS DA PAGINA 2 ##
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
        ### FILTROS DA PAGINA 3 ##
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
        ### FILTROS DA PAGINA 4 ##
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
        ### FILTROS DA PAGINA 5 ##
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
    ),
    
    nav_panel("Gestão de Isolamento",
              layout_column_wrap(
                width = 1/3,
                height = 120,
                
                value_box(
                  title = "Economia Total do Giro de Leito de Isolamento",
                  value = saving_giro_total_value_ui("total_value"),
                  showcase = bsicons::bs_icon("cash-coin"),
                  theme = value_box_theme(bg = "white", fg = "#0B538E"),
                  class = "border"
                ),
                
                value_box(
                  title = "Taxa de Rotatividade de Leitos",
                  value = saving_giro_tx_rotatividade_ui("tx_rotatividade"),
                  showcase = bsicons::bs_icon("file-medical"),
                  theme = value_box_theme(bg = "white", fg = "#0B538E"),
                  class = "border"
                ),
                
                value_box(
                  title = "Tempo Médio de Ocupação do Leito de Isolamento",
                  value = saving_tm_ocupacao_leito_ui("ocupacao_leito"),
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
                              ui_ddd_plot("DDD_plot")
                            )
                          ), # card
                      
                          card(
                            height = 450,
                            max_height = 600,
                            min_height = 350,
                            full_screen = TRUE,
                            card_header("DDD (Dose Diária Definida) ao longo do tempo",
                                        style = "text-align: center;"),
                            ui_ddd_plot_month("DDD_plot_month")
                          ), # card                  
                          
                          card(
                            height = 450,
                            max_height = 600,
                            min_height = 350,
                            full_screen = TRUE,
                            ui_ddd_table("DDD_tbl")
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
                              ui_dot_plot("DOT_plot")
                            )
                          ), # card
                        
                          card(
                            height = 450,
                            max_height = 600,
                            min_height = 350,
                            full_screen = TRUE,
                            card_header("DOT (Days of Therapy) ao longo do tempo",
                                        style = "text-align: center;"),
                            ui_dot_plot_month("DOT_plot_month")
                          ), # card                  
                          
                          card(
                            height = 450,
                            max_height = 600,
                            min_height = 350,
                            full_screen = TRUE,
                            ui_dot_table("DOT_tbl")
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
                              ui_lot_plot("LOT_plot")
                            )
                          ), # card
                          
                          card(
                            height = 450,
                            max_height = 600,
                            min_height = 350,
                            full_screen = TRUE,
                            card_header("LOT (Length of Treatment) ao longo do tempo",
                                        style = "text-align: center;"),
                            
                            ui_lot_plot_month("LOT_plot_month")
                            
                          ), # card                  
                          
                          card(
                            height = 450,
                            max_height = 600,
                            min_height = 350,
                            full_screen = TRUE,
                            ui_lot_table("LOT_tbl")
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
    ),
    
    nav_panel("Saving de Intervenções Farmacêuticas",
              
              layout_column_wrap(
                width = 1/3,
                height = 120,
                
                value_box(
                  title = "Custo Evitado Total",
                  value = saving_total_value_ui("sitv"),
                  showcase = bsicons::bs_icon("cash-coin"),
                  theme = value_box_theme(bg = "white", fg = "#0B538E"),
                  class = "border"
                ),
                
                value_box(
                  title = "Custo Médio Evitado por Conversão",
                  value = saving_per_conversion_value_ui("spcv"),
                  showcase = bsicons::bs_icon("currency-dollar"),
                  theme = value_box_theme(bg = "white", fg = "#0B538E"),
                  class = "border"
                ),
                
                value_box(
                  title = "Taxa de Conversão Efetivada das Sugestões IV-VO",
                  value = saving_ratio_ui("srvalue"),
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
                     plot_saving_int_total_ui("psit")
                ),
                
                card(full_screen = TRUE, 
                     card_header("Redução no Tempo Médio de Uso Intravenoso (IV) Antes da Conversão",
                                 style = "text-align: center;"), 
                     card_body(class = "p-0", 
                               plot_saving_int_tm_ui("psitm")
                               )
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
                  plot_saving_int_ratio_ui("siratio")
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
                  plot_si_cost_sector_ui("psics")
                )
              )
    ),
    
    nav_panel("Custos por antibióticos",
              
              layout_column_wrap(
                width = 1/2,
                height = 120,
                
                value_box(
                  title = "Custo Total com Antibióticos",
                  value = atb_cost_total_value_ui("actv"),
                  showcase = bsicons::bs_icon("prescription2"),
                  theme = value_box_theme(bg = "white", fg = "#0B538E"),
                  class = "border"
                ),
                
                value_box(
                  title = "Custo Médio por Tratamento",
                  value = atb_mean_cost_value_ui("acmv"),
                  showcase = bsicons::bs_icon("clipboard2-pulse"),
                  theme = value_box_theme(bg = "white", fg = "#0B538E"),
                  class = "border"
                )
              ),

              plot_atb_cost_ui("pac"),
              
              atb_cost_sector_plot_ui("pasc")
    ),
    
    # Definir tema e fontes
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

  #######################################################
  ##### PAGINA 1: 'Gestão de Isolamento de Leito'   #####
  #######################################################
  
  ##### FILTROS PAG_1 #####
  
  ## Atualização de Botões (FILTRAR) ##
  
  # Valores reativos para armazenar filtros
  filter_gi <- reactiveValues(
    gestao_isolamento_tipo_data = "coleta",
    gestao_isolamento_data = gestao_isolamento_range,
    isolamento_setor = NULL,
    isolamento_medico_id = NULL,
    isolamento_microorganism = NULL,
    gi_button_sector = 'Agregados',
    gi_button_time_setting = 'Mensal'
  )
  
  # Atualizar os valores reativos quando o botão 'filtrar' for pressionado
  observeEvent(input$filter_btn_gi, {
    filter_gi$gestao_isolamento_tipo_data <- input$gestao_isolamento_tipo_data
    filter_gi$gestao_isolamento_data <- input$gestao_isolamento_data
    filter_gi$isolamento_setor <- input$isolamento_setor
    filter_gi$isolamento_medico_id <- input$isolamento_medico_id
    filter_gi$isolamento_microorganism <- input$isolamento_microorganism
    filter_gi$gi_button_sector <- input$gi_button_sector
    filter_gi$gi_button_time_setting <- input$gi_button_time_setting
  })
  
  ## Atualização de Botões (RESET) ##
  
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
  
  ## Tabelas e Objetos Reativos da Página 1
  
  # Define o rótulo do eixo X dos gráficos mensais de forma reativa
  gi_x_label_month <- reactive({
    switch(filter_gi$gestao_isolamento_tipo_data,
           "entrada" = "Mês de entrada do paciente",
           "coleta" = "Mês de coleta do exame",
           "resultado" = "Mês de resultado do exame",
           "release" = "Mês de transferência ou liberação")
  })
  
  # Define o rótulo do eixo X dos gráficos semanais de forma reativa
  gi_x_label_week <- reactive({
    switch(filter_gi$gestao_isolamento_tipo_data,
           "entrada" = "Semana de entrada do paciente",
           "coleta" = "Semana de coleta do exame",
           "resultado" = "Semana de resultado do exame",
           "release" = "Semana de transferência ou liberação")
  })
  
  # Define o limite superior e inferior do eixo x dos gráficos mensais com base no valor do filtro de data, e tipo de data escolhida para filtro/visualização
  # As datas são arrendondas para o primeiro dia do mês, para manter consistência na visualização do eixos.
  
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
  
  # Define o limite superior e inferior do eixo x dos gráficos semanais com base no valor do filtro de data, e tipo de data escolhida para filtro/visualização
  # As datas são arrendondas para o primeiro dia da semana, para manter consistência na visualização do eixos.
  
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
  
  # Tabela reativa utilizada para os gráficos, filtrando somente pela data, de acordo com o tipo de data escolhida para filtro/visualização
  
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
  
  # Tabela reativa utilizada para os gráficos, filtrando pelos outros filtros da UI
  
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
  
  # Tabela reativa com tempo médio entre amostra e coleta (Horas), ou entre resultado e liberação (Hores_res), agregados por mês
  
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
  
  # Tabela reativa com tempo médio entre amostra e coleta (Horas), ou entre resultado e liberação (Hores_res), agregados por semana
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
  
  # Tabela reativa com tempo médio entre amostra e coleta (Horas), ou entre resultado e liberação (Hores_res), agregados por mês e setor hospitalar (DESCRIPTION)
  
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
  
  # Tabela reativa com tempo médio entre amostra e coleta (Horas), ou entre resultado e liberação (Hores_res), agregados por semana e setor hospitalar (DESCRIPTION)
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
  
  # Tabela reativa para estimativa do valor economizado (saving) pelo giro de leito
  # Calcula o tempo de ocupação em horas (occupation_hours), então conta o número de dias de ocupação do leito (occupation_days),
  # O número de dias economizados (saving_days_release) é dado pelo numero de dias de ocupação menos um valor padrão 
  # de tempo médio de isolamento (mean_isolation_time, default de 5 dias).
  # Por fim, o valor economizado (saving_cost) é dado pela multiplicação do custo de diária do leito pelo número de dias economizados.
  
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
  
  # Módulos para caixas de informação (info_value)
  
  # Economia Total do Giro de Leito de Isolamento
  saving_giro_total_value_server("total_value", saving_giro_tbl)
  
  # Taxa de Rotatividade de Leitos
  saving_giro_tx_rotatividade_server("tx_rotatividade", gi_tbl_prev, bed_availability_tbl, filter_gi)
  
  # Tempo Médio de Ocupação do Leito de Isolamento
  saving_tm_ocupacao_leito_server("ocupacao_leito", gi_tbl_prev, filter_gi)
  
  # Módulo para gráficos de Tempo entre Coleta e Resultado
  
  ## Gráfico por mês:
  server_gi_dif_amostra_result_plot_month(id = "gidifm", gi_tbl_month, gi_tbl_sector, max_dif_isolamento_month, filter_gi, gi_x_label_month)
  
  ## Gráfico por semana:
  server_gi_dif_amostra_result_plot_week(id = "gidifw", gi_tbl_week, gi_tbl_sector_week, max_dif_isolamento_week, filter_gi, gi_x_label_week)
  
  ## Renderizar UI dependendo da escolha (Mensal ou Semanal)
  
  output$gi_dif_amostra_result_plot <- renderUI({
    
    if(filter_gi$gi_button_time_setting == 'Mensal') {
      ui_gi_dif_amostra_result_plot_month("gidifm")
    } else {
      ui_gi_dif_amostra_result_plot_week("gidifw")
    }
    
  })
  
  # Módulo para gráficos de Tempo entre Resultado e Liberação do Isolamento
  
  ## Gráfico por mês:
  server_gi_dif_res_plot_month(id = "plot_res_month", filter_gi, gi_tbl_month, gi_tbl_sector, gi_x_label_month, max_dif_isolamento_month_res)
  
  ## Gráfico por semana:
  server_gi_dif_res_plot_week(id = "plot_res_week", filter_gi, gi_tbl_week, gi_tbl_sector_week, gi_x_label_week, max_dif_isolamento_week_res)
  
  ## Renderizar UI dependendo da escolha (Mensal ou Semanal)
  output$gi_dif_res_amostra_result_plot <- renderUI({
    
    if(filter_gi$gi_button_time_setting == 'Mensal') {
      ui_gi_dif_res_plot_month("plot_res_month")
    } else {
      ui_gi_dif_res_plot_week("plot_res_week")
    }
    
  })
  
  # Tabela reativa para estimativa do valor economizado (saving) pelo giro de leito, agregados por mês
  
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
  
  # Tabela reativa para estimativa do valor economizado (saving) pelo giro de leito, agregados por semana
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
  
  # Tabela reativa para estimativa do valor economizado (saving) pelo giro de leito, agregados por mês e setor hospitalar (DESCRIPTION)
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
  
  # Tabela reativa para estimativa do valor economizado (saving) pelo giro de leito, agregados por semana e setor hospitalar (DESCRIPTION)
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
  
  # Módulo para gráficos de Economia pelo Giro de Leito de Isolamento
  
  ## Gráfico por mês:
  server_saving_cost_ocupacao_leito_month(id = "scol_month", filter_gi, saving_giro_tbl_month, saving_giro_tbl_sector_month, 
                                          gi_x_label_month, x_range_gi_month, format_currency_br)
  
  ## Gráfico por semana:
  server_saving_cost_ocupacao_leito_week(id = "scol_week", filter_gi, gi_tbl_week, saving_giro_tbl_week, saving_giro_tbl_sector_week, 
                                         gi_x_label_week, x_range_gi_week, format_currency_br)
  
  ## Renderizar UI dependendo da escolha (Mensal ou Semanal)
  
  output$saving_cost_ocupacao_leito <- renderUI({
    
    if(filter_gi$gi_button_time_setting == 'Mensal') {
      ui_saving_cost_ocupacao_leito_month("scol_month")
    } else {
      ui_saving_cost_ocupacao_leito_week("scol_week")
    }
    
  })

  # Taxa de Rotatividade
  # A taxa de rotatividade é calculada pelo número de leitos liberados, dividido pela média de leitos disponíveis durante o período analisado.
  # O número de leito liberados considera quantos pacientes estavam em isolamento (ISOLATED == "Sim"), e filtra quantas liberações ocorreram
  # dentro do intervalo de tempo especificado no filtro (filter_gi$gestao_isolamento_data), de acordo com a data em RELEASE_DATE
  # O número de leitos disponíveis é informado pela tabela `bed_availability_tbl`
  
  # Taxa de rotatividade, calculada por mês
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
  
  # Taxa de rotatividade, calculada por semana
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
  
  # Taxa de rotatividade, calculada por mês e setor hospitalar (DESCRIPTION)
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
  
  # Taxa de rotatividade, calculada por semana e setor hospitalar (DESCRIPTION)
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
  
  # Módulo para gráficos de Taxa de Rotatividade de Leitos
  
  ## Gráfico por mês:
  server_tx_rotatividade_month_plot(id = "trol_plot_month", filter_gi, saving_giro_tx_rot_month_tbl, saving_giro_tx_rot_sector_month_tbl, 
                                    gi_x_label_month, x_range_gi_month)
  
  ## Gráfico por semana:
  server_tx_rotatividade_week_plot(id = "trol_plot_week", filter_gi, gi_tbl_week, saving_giro_tx_rot_week_tbl, saving_giro_tx_rot_sector_week_tbl, 
                                   gi_x_label_week, x_range_gi_week)
  
  ## Renderizar UI dependendo da escolha (Mensal ou Semanal)
  output$tx_rotatividade_ocupacao_leito <- renderUI({
    
    if(filter_gi$gi_button_time_setting == 'Mensal') {
      ui_tx_rotatividade_month_plot("trol_plot_month")
    } else {
      ui_tx_rotatividade_week_plot("trol_plot_week")
    }
    
  })
  
  ## Tempo Médio de Ocupação do Leito, calculado por mês
  # São considerados somente internações de pacientes que estavam em isolamento (ISOLATED == "Sim"), cuja data de entrada (ADMISSION_DATE) e
  # data de liberação (RELEASE_DATE) estejam dentro do intervalo de tempo especificado no filtro.
  
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
  
  ## Tempo Médio de Ocupação do Leito, calculado por semana
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
  
  ## Tempo Médio de Ocupação do Leito, calculado por mês e setor hospitalar (DESCRIPTION)
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
  
  ## Tempo Médio de Ocupação do Leito, calculado por semana e setor hospitalar (DESCRIPTION)
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
  
  # Módulo para gráficos de Tempo Médio de Ocupação do Leito
  
  ## Gráfico por mês:
  server_tm_ocupacao_leito_month_plot(id = "gitm_plot_month", filter_gi, gitm_month_tbl, gitm_sector_month_tbl, x_range_gi_month)
  
  ## Gráfico por semana:
  server_tm_ocupacao_leito_week_plot(id = "gitm_plot_week", filter_gi, gitm_week_tbl, gitm_sector_week_tbl, x_range_gi_week)
  
  ## Renderizar UI dependendo da escolha (Mensal ou Semanal)
  output$tm_ocupacao_leito_plot <- renderUI({
    
    if(filter_gi$gi_button_time_setting == 'Mensal') {
      ui_tm_ocupacao_leito_month_plot("gitm_plot_month")
    } else {
      ui_tm_ocupacao_leito_week_plot("gitm_plot_week")
    }
    
  })
  
  #######################################################
  ##### PAGINA 2: 'Consumo de antimicrobianos'      #####
  #######################################################
  
  ##### FILTROS PAG_2 #####
  
  # Configura botões de filtro como reativos, para serem utilizados dentro dos módulos
  
  filter_mic_ddd <- reactiveValues(
    ddd_plot_button = "Destacar itens"
  )
  
  filter_mic_dot <- reactiveValues(
    dot_plot_button = "Destacar itens"
  )
  
  filter_mic_lot <- reactiveValues(
    lot_plot_button = "Destacar itens"
  )
  
  # Update reactive values
  observeEvent(input$ddd_plot_button, {
    filter_mic_ddd$ddd_plot_button <- input$ddd_plot_button
  })
  
  observeEvent(input$dot_plot_button, {
    filter_mic_dot$dot_plot_button <- input$dot_plot_button
  })
  
  observeEvent(input$lot_plot_button, {
    filter_mic_lot$lot_plot_button <- input$lot_plot_button
  })
  
  ## Atualização de Botões (FILTRAR) ##
  
  # Valores reativos para armazenar filtros
  filter_mic <- reactiveValues(
    date_range = antimicrob_date_range,
    setor_mic = NULL,
    medicamento_mic = NULL
  )
  
  # Atualizar os valores reativos quando o botão 'filtrar' for pressionado
  observeEvent(input$filter_btn_mic, {
    filter_mic$date_range <- input$internacao_data
    filter_mic$setor_mic <- input$antimicrobiano_setor
    filter_mic$medicamento_mic <- input$antimicrobiano_medicamento
  })
  
  ## Atualização de Botões (RESET) ##
  observeEvent(input$reset_btn_mic, {
    
    updateDateRangeInput(session, "internacao_data",
                         start = antimicrob_date_range[1],
                         end = antimicrob_date_range[2],
                         min = antimicrob_date_range[1], 
                         max = antimicrob_date_range[2])
    
    updatePickerInput(session, "antimicrobiano_setor", selected = "")
    
    updatePickerInput(session, "antimicrobiano_medicamento", selected = "")
    
  })
  
  ## Tabelas e Objetos Reativos da Página 2
  
  # Define intervalo de dados como valor reativo, arredondados para o primeiro dia do mês, para ser utilizado no módulo de gráficos
  antimicrob_date_range_floor <- reactive({
    as.Date(floor_date(filter_mic$date_range, unit = 'month'))
  })
  
  # Tabela de DDD, filtrada por data de entrada do paciente e setor hospitalar
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
  
  # Tabela de DDD, calculados por medicamento e mês
  
  DDD_month_tbl <- reactive({
    antimicrob_tbl_final1 <- antimicrob_tbl_final %>%
      dplyr::filter(
        ADM_START_DATE >= filter_mic$date_range[1] &
          ADM_START_DATE <= filter_mic$date_range[2]
      )
    
    antimicrob_tbl_final2 <- if (is.null(filter_mic$setor_mic)) {
      antimicrob_tbl_final1
    } else {
      antimicrob_tbl_final1 %>%
        dplyr::filter(DESCRIPTION %in% filter_mic$setor_mic)
    }
    
    antimicrob_tbl_final3 <- if (is.null(filter_mic$medicamento_mic)) {
      antimicrob_tbl_final2
    } else {
      antimicrob_tbl_final2 %>%
        dplyr::filter(MEDICATION_NAME %in% filter_mic$medicamento_mic)
    }
    
    antimicrob_tbl_final3 %>%
      group_by(MEDICATION_NAME, ADM_START_MONTH) %>%
      reframe(A_ratio_B = sum(DD) / unique(DDD_OMS), P = sum(ADMISSION_DURATION)) %>%
      ungroup() %>%
      mutate(DDD = A_ratio_B / P * 1000) %>%
      group_by(MEDICATION_NAME, ADM_START_MONTH) %>%
      
      # ! Este trecho retira duplicações de nome do medicamento em MEDICATION_NAME. Conferir se vai ser necessário com os dados reais!
      mutate(n_med_name = 1:n()) %>%
      ungroup() %>%
      filter(n_med_name == 1) %>%
      # ! Este trecho retira duplicações de nome do medicamento em MEDICATION_NAME. Conferir se vai ser necessário com os dados reais!
      
      select(MEDICATION_NAME, ADM_START_MONTH, DDD) %>%
      tidyr::complete(
        MEDICATION_NAME,
        ADM_START_MONTH = seq(min(ADM_START_MONTH), max(ADM_START_MONTH), by = "month"),
        fill = list(DDD = 0)
      ) %>%
      left_join(antimicrob_tbl_codes, by = "MEDICATION_NAME")
  })
  
  # Módulo dos gráficos para DDD
  
  ## DDD, comparação por antimicrobiano
  server_ddd_plot(id = "DDD_plot", filter_mic, DDD_tbl, filter_mic_ddd) 
  
  ## DDD, comparação por mês
  server_ddd_plot_month(id = "DDD_plot_month", DDD_month_tbl, antimicrob_date_range_floor) 
  
  ## DDD, tabela
  server_ddd_table(id = "DDD_tbl", DDD_month_tbl) 
  
  # Tabela de DOT, filtrada por data de entrada do paciente e setor hospitalar
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
  
 
  # Tabela de DOT, calculados por medicamento e mês
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
  
  # Módulo dos gráficos para DOT
  
  ## DOT, comparação por antimicrobiano
  server_dot_plot(id = "DOT_plot", filter_mic, DOT_tbl, filter_mic_dot) 
  
  ## DOT, comparação por mês
  server_dot_plot_month(id = "DOT_plot_month", DOT_month_tbl, antimicrob_date_range_floor) 
  
  ## DOT, tabela
  server_dot_table(id = "DOT_tbl", DOT_month_tbl) 

  ### LOT Length of Treatment
  LOT_tbl <- reactive({
    
    antimicrob_tbl_final %>%
      dplyr::filter(ADM_START_DATE >= filter_mic$date_range[1] &
                      ADM_START_DATE <= filter_mic$date_range[2])
    
  })
  
  # Tabela de LOT, calculados por mês. A métrica não é calculada para cada medicamento, pois LOT considera o tempo total de terapia do paciente considerando
  # todos os medicamentos
  
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
  
  # Módulo dos gráficos para LOT
  
  ## LOT, comparação por antimicrobiano
  server_lot_plot(id = "LOT_plot", filter_mic, LOT_tbl, filter_mic_lot) 
  
  ## LOT, comparação por mês
  server_lot_plot_month(id = "LOT_plot_month", LOT_month_tbl, antimicrob_date_range_floor) 
  
  ## LOT, tabela
  server_lot_table(id = "LOT_tbl", LOT_month_tbl) 
  
  ####################################################################
  ##### PAGINA 3: 'Aceitabilidade de Intervenções Farmacêuticas' #####
  ####################################################################
  
  ##### FILTROS PAG_3 #####
  
  # Configura botões de filtro como reativos, para serem utilizados dentro dos módulos
  filter_acpt_n <- reactiveValues(
    plot_acpt_time_setting = "Mensal"
  )
  
  filter_acpt_i_n <- reactiveValues(
    plot_acpt_tm_time_setting = "Mensal"
  )
  
  observeEvent(input$plot_acpt_time_setting, {
    filter_acpt_n$plot_acpt_time_setting <- input$plot_acpt_time_setting
  })
  
  observeEvent(input$plot_acpt_tm_time_setting, {
    filter_acpt_i_n$plot_acpt_tm_time_setting <- input$plot_acpt_tm_time_setting
  })
  
  ## Atualização de Botões (FILTRAR) ##
  
  # Valores reativos para armazenar filtros
  
  filter_aceitabilidade <- reactiveValues(
    aceitabilidade_date = aceitabilidade_date_range,
    aceitabilidade_setor = NULL,
    aceitabilidade_medico_id = NULL,
    aceitabilidade_user_id = NULL,
    aceitabilidade_adherence = "all",
    aceitabilidade_type = "sugestao"
  )
  
  # Atualizar os valores reativos quando o botão 'filtrar' for pressionado
  observeEvent(input$filter_btn_acpt, {
    filter_aceitabilidade$aceitabilidade_date <- input$aceitabilidade_date
    filter_aceitabilidade$aceitabilidade_setor <- input$aceitabilidade_setor
    filter_aceitabilidade$aceitabilidade_medico_id <- input$aceitabilidade_medico_id
    filter_aceitabilidade$aceitabilidade_user_id <- input$aceitabilidade_user_id
    filter_aceitabilidade$aceitabilidade_adherence <- input$aceitabilidade_adherence
    filter_aceitabilidade$aceitabilidade_type <- input$aceitabilidade_type
  })
  
  ## Atualização de Botões (RESET) ##
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
  
  ## Tabelas e Objetos Reativos da Página 3
  
  # Tabela de aceitabilidade das intervenções farmacêuticas, filtrada por todas as categorias exceto aceitação ou não da intervenção ('ALERT_ADHERENCE')
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
  
  # Valor máximo da contagem de aceitabilidade das intervenções farmacêuticas (para limite do eixo y) por semana
  aceitabilidade_tbl_filtered_max_week <- reactive({  
    
    aceitabilidade_tbl_filtered_prev() %>%
      count(ALERT_WEEK) %>%
      summarise(max_value = max(n, na.rm = T))
  })
  
  # Valor máximo da contagem de aceitabilidade das intervenções farmacêuticas (para limite do eixo y) por mês
  aceitabilidade_tbl_filtered_max_month <- reactive({  
    
    aceitabilidade_tbl_filtered_prev() %>%
      count(ALERT_MONTH) %>%
      summarise(max_value = max(n, na.rm = T))
  })
  
  # Valor máximo do tempo médio de resposta de aceitabilidade das intervenções farmacêuticas (para limite do eixo y) por semana
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
  
  # Valor máximo do tempo médio de resposta de aceitabilidade das intervenções farmacêuticas (para limite do eixo y) por mês
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
  
  # Tabela de aceitabilidade das intervenções farmacêuticas, filtrada pela aceitação ou não da intervenção ('ALERT_ADHERENCE')
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
  
  # Tabela de implementação das intervenções farmacêuticas, filtrada por todas as categorias exceto implementação ou não da intervenção ('ALERT_ADHERENCE')
  # Esta tabela considera somente os pedidos de sugestões que foram aceitos pelo usuário (ALERT_ADHERENCE == TRUE)
  # Observe que mesmo após constar no sistema que a sugestão foi aceita, a implementação pode de fato ser realizada ou não (IMPLEMENTATION_ADHERENCE == TRUE)
  
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
  
  # Valor máximo da contagem de resposta de implementação das intervenções farmacêuticas (para limite do eixo y) por semana
  aceitabilidade_implem_tbl_filtered_max_week <- reactive({  
    
    aceitabilidade_implem_tbl_filtered_prev() %>%
      count(IMPLEMENTATION_WEEK) %>%
      summarise(max_value = max(n, na.rm = T))
  })
  
  # Valor máximo da contagem de resposta de implementação das intervenções farmacêuticas (para limite do eixo y) por mês
  aceitabilidade_implem_tbl_filtered_max_month <- reactive({  
    
    aceitabilidade_implem_tbl_filtered_prev() %>%
      count(IMPLEMENTATION_MONTH) %>%
      summarise(max_value = max(n, na.rm = T))
  })
  
  # Valor máximo do tempo médio de resposta de implementação das intervenções farmacêuticas (para limite do eixo y) por semana
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
  
  # Valor máximo do tempo médio de resposta de implementação das intervenções farmacêuticas (para limite do eixo y) por mês
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
  
  # Tabela de implementação das intervenções farmacêuticas, filtrada pela implementação ou não da intervenção ('IMPLEMENTATION_ADHERENCE')
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
  
  # Gráfico circular com a frequência de aceite das sugestões de intervenções farmacêuticas (Tipo de intervenção = 'Sugestão')
  aceitabilidade_pie_server(id = "aceitabilidade_pie", aceitabilidade_tbl_filtered)
  
  # Gráfico circular com a frequência implementação das sugestões de intervenções farmacêuticas (Tipo de intervenção = 'Implementação')
  aceitabilidade_implemen_pie_server(id = "aceitabilidade_imp_pie", aceitabilidade_implem_tbl_filtered)
  
  # Define qual gráfico é renderizado de acordo com a escolha do botão 'Tipo de Intervenção'
  output$aceitabilidade_pie <- renderUI({
    if (filter_aceitabilidade$aceitabilidade_type == "sugestao") {
      aceitabilidade_pie_ui("aceitabilidade_pie")
    } else {
      aceitabilidade_implemen_pie_ui("aceitabilidade_imp_pie")
    }
  })
  
  # Aceitabilidade ao longo do tempo (contagem)
  
  ## Gráfico de aceitabilidade das sugestões de intervenção farmacêutica
  plot_aceitabilidade_server(id = "acpt_plot_time", aceitabilidade_tbl_filtered, aceitabilidade_tbl_filtered_max_month, 
                                         aceitabilidade_tbl_filtered_max_week, filter_aceitabilidade, filter_acpt_n) 
  
  ## Gráfico de implementação das sugestões de intervenção farmacêutica
  plot_aceitabilidade_implemen_server(id = "acpt_i_plot_time", aceitabilidade_implem_tbl_filtered, aceitabilidade_implem_tbl_filtered_max_month, 
                                      aceitabilidade_implem_tbl_filtered_max_week, filter_aceitabilidade, filter_acpt_n) 
  
  # Define qual gráfico é renderizado de acordo com a escolha do botão 'Tipo de Intervenção'
  output$plot_aceitabilidade_n_dynamic <- renderUI({
    if (filter_aceitabilidade$aceitabilidade_type == "sugestao") {
      plot_aceitabilidade_ui("acpt_plot_time")
    } else {
      plot_aceitabilidade_implemen_ui("acpt_i_plot_time")
    }
  })
  
  # Aceitabilidade ao longo do tempo (Tempo médio de resposta)
  
  ## Gráfico de aceitabilidade das sugestões de intervenção farmacêutica
  plot_aceitabilidade_tm_server(id = "acpt_tm_plot_time", tm_aceitabilidade_tbl_filtered_max_month, tm_aceitabilidade_tbl_filtered_max_week, 
                                            filter_aceitabilidade, aceitabilidade_tbl_filtered, filter_acpt_i_n)  
  
  ## Gráfico de implementação das sugestões de intervenção farmacêutica
  plot_aceitabilidade_implemen_tm_server(id = "acpt_tm_i_plot_time", tm_aceitabilidade_implem_tbl_filtered_max_month, 
                                         tm_aceitabilidade_implem_tbl_filtered_max_week, filter_aceitabilidade, 
                                         aceitabilidade_implem_tbl_filtered, filter_acpt_i_n)  
  
  # Define qual gráfico é renderizado de acordo com a escolha do botão 'Tipo de Intervenção'
  output$plot_aceitabilidade_tm_dynamic <- renderUI({
    if (filter_aceitabilidade$aceitabilidade_type == "sugestao") {
      plot_aceitabilidade_tm_ui("acpt_tm_plot_time")
    } else {
      plot_aceitabilidade_implemen_tm_ui("acpt_tm_i_plot_time")
    }
  })
  
  ############################################################
  ##### PAGINA 4: 'Saving de Intervenções Farmacêuticas' #####
  ############################################################
  
  ##### FILTROS PAG_4 #####
  
  # Configura botões de filtro como reativos, para serem utilizados dentro dos módulos
  filter_si_ratio <- reactiveValues(
    plot_saving_int_time_setting = "Mensal"
  )
  
  filter_si_sector <- reactiveValues(
    si_sector_button = "Destacar itens"
  )

  observeEvent(input$plot_saving_int_time_setting, {
    filter_si_ratio$plot_saving_int_time_setting <- input$plot_saving_int_time_setting
  })
  
  observeEvent(input$si_sector_button, {
    filter_si_sector$si_sector_button <- input$si_sector_button
  })
  
  ## Atualização de Botões (FILTRAR) ##
  
  # Valores reativos para armazenar filtros
  filter_saving_int <- reactiveValues(
    saving_int_date = aceitabilidade_date_range,
    saving_int_setor = NULL,
    saving_int_medicamento = NULL,
    saving_int_medico_id = NULL
  )
  
  # Atualizar os valores reativos quando o botão 'filtrar' for pressionado
  observeEvent(input$filter_btn_saving_int, {
    filter_saving_int$saving_int_date <- input$saving_int_date
    filter_saving_int$saving_int_setor <- input$saving_int_setor
    filter_saving_int$saving_int_medicamento <- input$saving_int_medicamento
    filter_saving_int$saving_int_medico_id <- input$saving_int_medico_id
  })
  
  ## Atualização de Botões (RESET) ##
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
  
  ## Tabelas e Objetos Reativos da Página 4

  # Tabela reativa de Saving de Intervenções Farmacêuticas, filtrada por todas as categorias exceto setor hospitalar
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

  # Tabela reativa de Saving de Intervenções Farmacêuticas, filtrada pelo setor hospitalar  
  saving_int_tbl <- reactive({  
    # Filtro por setor
    if (is.null(filter_saving_int$saving_int_setor)) {
      saving_int_tbl_filter_sector <- saving_int_tbl_prev()
    } else {
      saving_int_tbl_filter_sector <- saving_int_tbl_prev() %>%
        dplyr::filter(DESCRIPTION %in% filter_saving_int$saving_int_setor)
    }
  })

  ##  Value Box Custo Evitado Total  
  saving_total_value_server(id = "sitv", saving_int_tbl)
  
  ## Value Box Custo Médio Evitado por Conversão
  saving_per_conversion_value_server(id = "spcv", saving_int_tbl)
  
  ## Value Box Taxa de Conversão Efetivada das Sugestões IV-VO
  saving_ratio_server(id = "srvalue", saving_int_tbl)
  
  # Gráfico de Efetividade Econômica das Intervenções
  plot_saving_int_total_server(id = "psit", saving_int_tbl, format_currency_br)

  # Gráfico da Redução no Tempo Médio de Uso Intravenoso (IV) Antes da Conversão
  plot_saving_int_tm_server(id = "psitm", saving_int_tbl)
  
  # Gráfico da Taxa de Implementação das Intervenções Sugeridas
  plot_saving_int_ratio_server(id = "siratio", saving_int_tbl, filter_saving_int, filter_si_ratio) 
  
  # Gráfico da Redução do Custo Total de Antibióticos por Setor
  plot_si_cost_sector_server(id = "psics", saving_int_tbl_prev, filter_saving_int, filter_si_sector) 
  
  ################################################
  ##### PAGINA 5: 'Custo por antibióticos'   #####
  ################################################
  
  ##### FILTROS PAG_5 #####
  
  ## Atualização de Botões (FILTRAR) ##
  # Valores reativos para armazenar filtros
  filter_atb_cost <- reactiveValues(
    atb_cost_date = antimicrob_date_range,
    atb_cost_setor = NULL,
    atb_cost_medico_id = NULL,
    atb_cost_tuss = NULL,
    atb_cost_microorganism = NULL,
    atb_medication = NULL
  )
  
  # Atualizar os valores reativos quando o botão 'filtrar' for pressionado
  observeEvent(input$filter_btn_atb_cost, {
    filter_atb_cost$atb_cost_date <- input$atb_cost_date
    filter_atb_cost$atb_cost_setor <- input$atb_cost_setor
    filter_atb_cost$atb_cost_medico_id <- input$atb_cost_medico_id
    filter_atb_cost$atb_cost_tuss <- input$atb_cost_tuss
    filter_atb_cost$atb_cost_microorganism <- input$atb_cost_microorganism
    filter_atb_cost$atb_medication <- input$atb_medication
  })
  
  ## Atualização de Botões (RESET) ##
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
  
  ## Tabelas e Objetos Reativos da Página 5
  
  # Tabela de custo de antibióticos, filtrada por todas as categorias, exceto setor hospitalar
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
  
  # Tabela de custo de antibióticos, filtrada pelo setor hospitalar
  atb_cost_tbl <- reactive({ 
    if (is.null(filter_atb_cost$atb_cost_setor)) {
      atb_cost_tbl_filter_sector <- atb_cost_tbl_prev()
    } else {
      atb_cost_tbl_filter_sector <- atb_cost_tbl_prev() %>%
        dplyr::filter(DESCRIPTION %in% filter_atb_cost$atb_cost_setor)
    }
    atb_cost_tbl_filter_sector
  })
  
  # Gráfico de Efetividade Econômica das Intervenções
  atb_cost_total_value_server(id = "actv", atb_cost_tbl, format_currency_br)
  
  # Gráfico de Redução no Tempo Médio de Uso Intravenoso (IV) Antes da Conversão
  atb_mean_cost_value_server(id = "acmv", atb_cost_tbl, format_currency_br)
  
  # Gráfico da Taxa de Implementação das Intervenções Sugeridas
  plot_atb_cost_server(id = "pac", filter_atb_cost, atb_cost_tbl, format_currency_br)
  
  # Gráfico de Redução do Custo Total de Antibióticos por Setor
  atb_cost_sector_plot_server(id = "pasc", filter_atb_cost, atb_cost_tbl, atb_cost_tbl_prev, format_currency_br)
  
}

shinyApp(ui, server)