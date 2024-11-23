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

source("R/utils.R")
source("R/query_data.R")
Sys.setlocale('LC_ALL', 'pt_BR.UTF-8')

# LENDO A PUB_KEY - Deixe essa parte em branco
PUBKEY <- "-----BEGIN PUBLIC KEY-----
-----END PUBLIC KEY-----"
#pub_key <- read_pubkey(PUBKEY)

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
           selectizeInput("isolamento_setor", label = "Escolha um setor", 
                          choices = gestao_isolamento_setores_list,
                          selected = "Todos", multiple = FALSE,
                          options = NULL),  

           selectizeInput("isolamento_medico_id", label = "Médico Prescritor", 
                          choices = c("Todos", sort(unique(gestao_isolamento_tbl$PRACTITIONER_ID))), 
                          selected = "Todos", multiple = FALSE,
                          options = NULL),  
           
           column(5, actionButton("reset_btn_gi", "Reset", width = '125%')),
           column(1),
           column(5, actionButton("filter_btn_gi", "Filtrar", width = '125%'))
         )
      ),
      
      conditionalPanel(
        "input.nav === 'Consumo de antimicrobianos'",
        ### COLOQUE AQUI OS FILTROS DA PAGINA 2 ##
        fluidRow(
          
          dateRangeInput('antimicrobiano_data',
                         label = 'Data de início do tratamento',
                         start = antimicrob_date_range[1], end = antimicrob_date_range[2],
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
        "input.nav === 'Nome pag 4'",
        ### COLOQUE AQUI OS FILTROS DA PAGINA 4 ##
        fluidRow(
          column(5, actionButton("reset_btn_marker", "Reset", width = '125%')),
          column(1),
          column(5, actionButton("filter_btn_marker", "Filtrar", width = '125%'))
        )
      ),
      
      conditionalPanel(
        "input.nav === 'Nome pag 5'",
        ### COLOQUE AQUI OS FILTROS DA PAGINA 5 ##
        fluidRow(
          column(5, actionButton("reset_btn_marker", "Reset", width = '125%')),
          column(1),
          column(5, actionButton("filter_btn_marker", "Filtrar", width = '125%'))
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
                title = "Opções de visualização",
                # Panel with plot ----
                nav_panel("Opção A", dygraphOutput("plot1a")),
                
                nav_panel("Opção B", plotlyOutput("plot1b")),
                
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
                  plotlyOutput("plot_aceitabilidade_n")
                )
              ), # card    
              
              tableOutput("result")
              
              ),
    # 
    # nav_panel("Saving de Intervenções Farmacêuticas",
    #          # mod_p4_ui("pag_4")
    #          ),
    # 
    # nav_panel("Custos por antibióticos",
    #         #  mod_p5_ui("pag_5")
    #           ),
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
  gestao_isolamento_tbl_csv <- read.csv(file = "data/gestao_isolamento_tbl.csv")
  setores_hospitalares <- read.csv("data/setor_hospitalar.csv")
  
  gestao_isolamento_tbl <- gestao_isolamento_tbl_csv %>%
    mutate(
      ADMISSION_DATE = as.POSIXct(ADMISSION_DATE, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      collection_date_time = as.POSIXct(collection_date_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      result_status_date_time = as.POSIXct(result_status_date_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      RELEASE_DATE = as.POSIXct(RELEASE_DATE, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      dif = as.numeric(difftime(result_status_date_time, collection_date_time, units = "hours")),
      month = as.Date(floor_date(collection_date_time, unit = "month"))) %>%
    left_join(setores_hospitalares,  by = "UNIT_ID")
  
  gestao_isolamento_setores_list <- c("Todos", sort(unique(gestao_isolamento_tbl$DESCRIPTION)))
  #names(gestao_isolamento_setores_list) = gestao_isolamento_setores_list
  
  #PRD 2
  
  antimicrob_tbl_final <- read_csv(file = "data/antimicrob_tbl.csv")
  
  antimicrob_tbl_codes <- read.csv(file = "data/antimicrob_tbl_codes.csv")
  
  antimicrob_date_range <- range(antimicrob_tbl_final$ADM_START_DATE, na.rm = TRUE)
  
  antimicrob_setores_list <- sort(unique(antimicrob_tbl_final$DESCRIPTION))
  
#### Carregando os Modulos do Server ####
  
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
  
  observeEvent("", {
    if(vals$auth){
      mod_p1_server("pag_1", filtered_data_p1) # PASSE SEUS DADOS AQUI PARA COMUNICAR COM OS MODULOS
      mod_p2_server("pag_2", filtered_data_p2) # PASSE SEUS DADOS AQUI PARA COMUNICAR COM OS MODULOS
      mod_p3_server("pag_3", filtered_data_p3) # PASSE SEUS DADOS AQUI PARA COMUNICAR COM OS MODULOS
    } else {
      shinyalert("Erro de Acesso", "Entre no Munai BI através da Plataforma da Munai", type = "error")
    }
  })
  
  
  ##### FILTROS PAG_1 ##### DESENVOLVA POR DATA-SET REATIVOS
  # Reactive values to store filters
  filter_gi <- reactiveValues(
    isolamento_setor = "Todos",
    isolamento_medico_id = "Todos"
  )
  
  # Update reactive values when the button is pressed
  observeEvent(input$filter_btn_gi, {
    filter_gi$isolamento_setor <- input$isolamento_setor
    filter_gi$isolamento_medico_id <- input$isolamento_medico_id
  })
  
  ## Atualização de Botões ##
  # Pag 1
  
  observeEvent(input$reset_btn_gi, {
    
    updateSelectizeInput(session, "isolamento_setor", selected = "Todos")
    updateSelectizeInput(session, "isolamento_medico_id", selected = "Todos")
  
  })
  
  # Pag 2
  observeEvent(input$reset_btn_mic, {
    
    updateDateRangeInput(session, "antimicrobiano_data",
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
  
  dataset1 <- reactive({
    
    if (filter_gi$isolamento_setor == "Todos") {
      gestao_isolamento_tbl2 <- gestao_isolamento_tbl
    } else {
       gestao_isolamento_tbl2 <- gestao_isolamento_tbl %>%
          dplyr::filter(DESCRIPTION == filter_gi$isolamento_setor)
    }
    
    if (filter_gi$isolamento_medico_id == "Todos") {
      gestao_isolamento_tbl3 <- gestao_isolamento_tbl2
    } else {
      gestao_isolamento_tbl3 <- gestao_isolamento_tbl2 %>%
        dplyr::filter(PRACTITIONER_ID == filter_gi$isolamento_medico_id)
    }
    
    gestao_isolamento_tbl4 <- gestao_isolamento_tbl3 %>% 
      mutate(Horas = as.numeric(difftime(result_status_date_time, collection_date_time, units = "hours")),
             month = as.Date(floor_date(collection_date_time, unit = "month"))) |>
      group_by(month) |>
      summarise(Horas = mean(Horas)) %>%
      select(month, Horas) %>%
      arrange(month)
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
  
  # Preparar tabela em branco para plot de gestao de isolamento
  date_range_gi <- range(gestao_isolamento_tbl$month)
  date_seq_gi <- seq(date_range_gi[1], date_range_gi[2], by = "month")
  
  blank_dataset_gi = data.frame(month = date_seq_gi,
                                Horas = rep(NA, length(date_seq_gi)))
  
  ####
  first_row_isol_tbl <- blank_dataset_gi[1,]
  first_row_isol_tbl[,"Horas"] <- NA
  
  last_row_isol_tbl <- first_row_isol_tbl
  
  first_row_isol_tbl[,"month"] = min(blank_dataset_gi$month, na.rm = T) - 2
  last_row_isol_tbl[,"month"] = max(blank_dataset_gi$month, na.rm = T) + 2
  
  expanded_blank_dataset_gi <- rbind(first_row_isol_tbl, blank_dataset_gi, last_row_isol_tbl)
  
  xts_dataset_gi <- xts(expanded_blank_dataset_gi %>% select(Horas),
                        order.by = expanded_blank_dataset_gi$month)
  
  output$tabela <- renderTable({
    
    xts_dataset_gi_ggplot <- xts_dataset_gi[-c(1,nrow(xts_dataset_gi)),]
    xts_dataset_gi_ggplot[index(xts_dataset_gi),]$Horas <- 0
    xts_dataset_gi_ggplot[dataset1()$month,]$Horas <- dataset1()$Horas

    dataset_gi_ggplot <- data.frame(month = as.Date(index(xts_dataset_gi_ggplot)),
                                    Horas = xts_dataset_gi_ggplot$Horas)
    
  })
  
  output$plot1a <- renderDygraph({

    if(is.null(dataset1()) | nrow(dataset1()) == 0) {

      ### Visualização 1
      dygraph(xts_dataset_gi,
              main = "Tempo entre Coleta e Resultado (Eficiência BDmax)") |>
        dyRangeSelector() |> # seletor inferior de tempo
        dyCrosshair(direction = 'vertical') |> # linhas verticais pra acompanhar o mouse
        dyLimit(as.numeric(24), "24 horas", color = "red", labelLoc = "right") %>%
        dyEvent('2023-4-1', 'Solução Go-Live', labelLoc = c('bottom'), 
                strokePattern = 'solid', color = "grey") |> # indicador de evento
        dyAxis('y', label = 'Tempo (horas)', valueRange = c(0,max_dif_isolamento$Horas)) |> # label y
        # dyAxis('x', label = "", drawGrid = FALSE, valueRange = range(dataset1()$month) + c(-3,3)) |> # retirando as linhas verticais do gráfico
        dyOptions(useDataTimezone = TRUE, colors = '#08A18E', rightGap = 10, # colocando estilo mês/dia/ano quando passa o mouse
                  drawPoints = TRUE, 
                  pointSize = 3,
                  fillGraph  = TRUE,
                  animatedZooms = TRUE,
                  includeZero = TRUE,
                  digitsAfterDecimal = 1) |> 
        dyUnzoom() # dando opção de desfazer o zoom
    } else {
    
    xts_dataset_gi_plot <- xts_dataset_gi 
    xts_dataset_gi_plot$Horas[2:(nrow(xts_dataset_gi)-1)] <- 0
    xts_dataset_gi_plot[dataset1()$month,]$Horas <- dataset1()$Horas
      
    if(nrow(dataset1()) == 1) {
      
      ### Visualização 1
      dygraph(xts_dataset_gi_plot,
              main = "Tempo entre Coleta e Resultado (Eficiência BDmax)") |>
        dyRangeSelector() |> # seletor inferior de tempo
        dyBarChart() |> # tipo de gráfico
        dyCrosshair(direction = 'vertical') |> # linhas verticais pra acompanhar o mouse
        dyLimit(as.numeric(24), "24 horas", color = "red", labelLoc = "right") %>%
        dyEvent('2023-4-1', 'Solução Go-Live', labelLoc = c('bottom'), 
                strokePattern = 'solid', color = "grey") |> # indicador de evento
        dyAxis('y', label = 'Tempo (horas)', valueRange = c(0,max_dif_isolamento$Horas)) |> # label y
        # dyAxis('x', label = "", drawGrid = FALSE, valueRange = range(dataset1()$month) + c(-3,3)) |> # retirando as linhas verticais do gráfico
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
    dygraph(xts_dataset_gi_plot,
            main = "Tempo entre Coleta e Resultado (Eficiência BDmax)") |>
      dyRangeSelector() |> # seletor inferior de tempo
      dySeries("Horas", label = "Tempo (horas)") %>%
      dyCrosshair(direction = 'vertical') |> # linhas verticais pra acompanhar o mouse
      dyLimit(as.numeric(24), "24 horas", color = "red", labelLoc = "right") %>%
      dyEvent('2023-4-1', 'Solução Go-Live', labelLoc = c('bottom'), 
              strokePattern = 'solid', color = "grey") |> # indicador de evento
      dyAxis('y', label = 'Tempo (horas)', valueRange = c(0,max_dif_isolamento$Horas)) |> # label y
      # dyAxis('x', label = "", drawGrid = FALSE, valueRange = range(dataset1()$month) + c(-3,3)) |> # retirando as linhas verticais do gráfico
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
    
    xts_dataset_gi_ggplot <- xts_dataset_gi[-c(1,nrow(xts_dataset_gi)),]
    xts_dataset_gi_ggplot[index(xts_dataset_gi),]$Horas <- 0
    xts_dataset_gi_ggplot[dataset1()$month,]$Horas <- dataset1()$Horas
    
    dataset_gi_ggplot <- data.frame(month = index(xts_dataset_gi_ggplot),
                                    Horas = xts_dataset_gi_ggplot$Horas)

    p4 <- dataset_gi_ggplot %>%
      ggplot(aes(x = month, y = Horas)) +
      geom_line(color = '#08A18E') +
      geom_point(aes(text = paste0('Tempo: ', round(Horas,1), ' horas')),
                 color = '#08A18E') +
      annotate("text", x = max(dataset_gi_ggplot$month, na.rm = T), y = 26, 
               label = "24 horas",
               color = "red",
               size = 3) + 
      geom_hline(yintercept = 24, linetype = "dashed", color = "red") +
      xlab("") +
      ylab("Tempo (horas)") +
      theme_minimal() +
      labs(title = "Tempo entre Coleta e Resultado (Eficiência BDmax)") +
      scale_y_continuous(expand = c(0, 0), limits = c(0, max_dif_isolamento$Horas)) +
      scale_x_date(breaks = "month", date_labels = "%b %Y") +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            axis.line = element_line(),
            plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches"))

    ggplotly(p4, tooltip = 'text')
  })
  
  
  output$plot1c <- renderPlotly({
    
    xts_dataset_gi_ggplot <- xts_dataset_gi[-c(1,nrow(xts_dataset_gi)),]
    xts_dataset_gi_ggplot[index(xts_dataset_gi),]$Horas <- 0
    xts_dataset_gi_ggplot[dataset1()$month,]$Horas <- dataset1()$Horas
    
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
      labs(title = "Tempo entre Coleta e Resultado (Eficiência BDmax)") +
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
    filter_mic$date_range <- input$antimicrobiano_data
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
              P = sum(n_distinct(PATIENT_ID))) %>%
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
              P = sum(n_distinct(PATIENT_ID))) %>%
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
              P = sum(n_distinct(PATIENT_ID))) %>%
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
      group_by(ADMISSION_ID) %>%
      mutate(dias_uso_atm_total = sum(as.numeric(ADM_END_DATE - ADM_START_DATE))) %>%
      ungroup() %>%
      mutate(dias_uso_atm = as.numeric(ADM_END_DATE - ADM_START_DATE) ) %>%
      group_by(MEDICATION_NAME, ADM_START_MONTH) %>%
      reframe(dias_uso_atm_sum = sum(dias_uso_atm),
              P = sum(dias_uso_atm_total)) %>%
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
        mutate(LOT_admission = as.numeric(ADM_END_DATE - ADM_START_DATE)) %>%
        ungroup() %>%
        group_by(DESCRIPTION) %>%
        reframe(LOT = sum(LOT_admission)/sum(LOT_admission)*1000) %>%
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
        mutate(LOT_admission = as.numeric(ADM_END_DATE - ADM_START_DATE)) %>%
        ungroup() %>%
        group_by(DESCRIPTION) %>%
        reframe(LOT = sum(LOT_admission)/sum(LOT_admission)*1000) %>%
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
  
  # Calcular LOT por medicamento e mês
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
      mutate(LOT_admission = as.numeric(ADM_END_DATE - ADM_START_DATE)) %>%
      ungroup() %>%
      group_by(DESCRIPTION, ADM_START_MONTH) %>%
      reframe(LOT = sum(LOT_admission)/sum(LOT_admission)*1000) %>%
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
  
  aceitabilidade_tbl <- read_csv(file = "data/aceitabilidade_tbl.csv")
  
  aceitabilidade_date_range <- range(c(range(aceitabilidade_tbl$ALERT_DATE, na.rm = TRUE),
                                       range(aceitabilidade_tbl$ALERT_ADHERENCE_DATE, na.rm = TRUE)))
  
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
  
  aceitabilidade_implem_tbl_filtered <- reactive({
    
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
    
    # Filtro por resposta de aceite ou não à intervenção
    if (filter_aceitabilidade$aceitabilidade_adherence == "all") {
      acpt_tbl_filter_adherence_i <- acpt_tbl_filter_user_i
    } else {
      acpt_tbl_filter_adherence_i <- acpt_tbl_filter_user_i %>%
        dplyr::filter(IMPLEMENTATION_ADHERENCE == filter_aceitabilidade$aceitabilidade_adherence)
    }
    
    return(acpt_tbl_filter_adherence_i)
    
  })
  
  output$result <- renderTable({
    aceitabilidade_tbl_filtered() %>% 
           head()
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
      
      x_axis_range <- as.numeric(diff(range(range_x_month)))
      x_tick_label_breaks <- case_when(x_axis_range <= 370 ~ 1,
                                       x_axis_range > 370 & x_axis_range <= 540 ~ 2,
                                       x_axis_range > 540 & x_axis_range <= 730 ~ 3,
                                       x_axis_range > 730 ~ 4)
      
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
        text = ~paste0(str_to_title(format(ALERT_MONTH, format = "%B %Y")), "<br>", ALERT_ADHERENCE,": ", n),
        hoverinfo = "text"
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
            range = range_x_month + c(-15,0)
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
        text = ~paste0("Semana: ", format(ALERT_WEEK_PT, format = "%d-%m-%y"), "<br>", ALERT_ADHERENCE,": ", n),
        hoverinfo = "text"
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
            range = range_x_week + c(-5,4)
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
}

shinyApp(ui, server)