# library(DBI)
# library(noctua)
# library(stringr)
# 
# log_message <- function(message) {
#   message(paste(Sys.time(), "-", message))
# }
# 
# print_env_vars <- function() {
#   log_message("Imprimindo variáveis de ambiente...")
#   db_name <- Sys.getenv("DB_NAME")
#   db_host <- Sys.getenv("DB_HOST")
#   db_port <- Sys.getenv("DB_PORT")
#   db_user <- Sys.getenv("DB_USER")
#   db_password <- Sys.getenv("DB_PASSWORD")
#   
#   log_message(paste("DB_NAME:", db_name))
#   log_message(paste("DB_HOST:", db_host))
#   log_message(paste("DB_PORT:", db_port))
#   log_message(paste("DB_USER:", db_user))
# }
# 
# print_env_vars()
# 
# log_message("Iniciando a conexão com o banco de dados...")
# 
# con <- tryCatch({
#   dbConnect(RPostgres::Postgres(),
#             dbname = Sys.getenv("DB_NAME"),
#             host = Sys.getenv("DB_HOST"),
#             port = Sys.getenv("DB_PORT"),
#             user = Sys.getenv("DB_USER"),
#             password = Sys.getenv("DB_PASSWORD"))
# }, error = function(e) {
#   log_message(paste("Erro ao conectar ao banco de dados:", e$message))
#   stop(e)
# })
# 
# 
# log_message("Conexão com o banco de dados estabelecida com sucesso.")

# EXEMPLO DE CONEXÃO QUE FAZEMOS COM O POSTDEGREE
# query_df_sensibilidade <- function() {
#   df <- dbGetQuery(con, "SELECT 
#                    id_do_paciente,
#                    amostra,
#                    data_de_coleta,
#                    mes,
#                    ano,
#                    microorganismo_shiny as microorganismo,
#                    servico_hospitalar_shiny as servico_hospitalar,
#                    tipo_da_amostra_shiny as tipo_da_amostra,
#                    n_do_isolado,
#                    morfologia_shiny as morfologia,
#                    antibiotico_shiny as antibiotico,
#                    sir_final_shiny as sir_final,
#                    mic_cumulativo
#                    FROM 
#                    public.perfil_sensibilidade")
#   return(df)
# }


query_dados_simulados <- function() {
  
  df <- read.csv("SEUS_DADOS_SIMULADOS.csv")
  
  return(df)
  
}