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


source("R/utils.R")
source("R/query_data.R")



# LEITURA DOS DADOS 
# df <- query_dados_simulados()



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
      "input.nav === 'PAGINA 1'",
      ### COLOQUE AQUI OS FILTROS DA PAGINA 1 ##
      fluidRow(
        column(5, actionButton("reset_btn", "Reset", width = '125%')),
        column(1),
        column(5, actionButton("filter_btn", "Filtrar", width = '125%'))
      )
    ),
    conditionalPanel(
      "input.nav === 'PAGINA 2'",
      ### COLOQUE AQUI OS FILTROS DA PAGINA 2 ##
      fluidRow(
        column(5, actionButton("reset_btn_mic", "Reset", width = '125%')),
        column(1),
        column(5, actionButton("filter_btn_mic", "Filtrar", width = '125%'))
      )
    ),
    conditionalPanel(
      "input.nav === 'PAGINA 3'",
      ### COLOQUE AQUI OS FILTROS DA PAGINA 3 ##
      fluidRow(
        column(5, actionButton("reset_btn_marker", "Reset", width = '125%')),
        column(1),
        column(5, actionButton("filter_btn_marker", "Filtrar", width = '125%'))
      )
    )
  ),
  nav_panel("PAGINA 1",
            mod_p1_ui("pag_1")),
  nav_panel("PAGINA 2",
            mod_p2_ui("pag_2")),
  nav_panel("PAGINA 3",
            mod_p3_ui("pag_3")),
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
  filtered_data_p1 <- reactive({
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
  
  
  
  
  
  
}

shinyApp(ui, server)