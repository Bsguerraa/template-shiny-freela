substituir_pontos_por_virgulas <- function(valor) {
  # Verificar se o valor contém pontos e vírgulas
  if (grepl("\\.", valor) && grepl(",", valor)) {
    # Substituir pontos por um caractere temporário
    valor_temporario <- gsub("\\.", "@@@", valor)
    # Substituir vírgulas por pontos
    valor_temporario <- gsub(",", ".", valor_temporario)
    # Substituir o caractere temporário por vírgulas
    valor_convertido <- gsub("@@@", ",", valor_temporario)
  } else {
    # Se não contém pontos e vírgulas, apenas inverter os caracteres
    valor_convertido <- ifelse(grepl("\\.", valor), gsub("\\.", ",", valor), gsub(",", ".", valor))
  }
  
  return(valor_convertido)
}

# ADICIONAR LINHA VERTICAL NO GRAFICO DE HISTOGRAM #
vline <- function(x = 0, color = "grey") {
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1, 
    yref = "paper",
    x0 = x, 
    x1 = x, 
    line = list(color = color)
  )
}

# ADICIONAR LINHA HORIZONTAL NO GRAFICO DE BARRA #
hline <- function(y = 0, color = "grey" , ...) {
  list(
    type = "line", 
    y0 = y, 
    y1 = y, 
    xref = "paper",
    x0 = 0, 
    x1 = 1, 
    line = list(color = color, ...)
  )
}

# ADICIONAR LEGENDA NA LINHA HORIZONTAL #
text_hline <- function(valor, orientation) {
  list(
    x = 0,
    y = valor*1.05,
    text = paste("Média de:", valor),
    showarrow = FALSE,
    xanchor = orientation)}


# ADICIONAR LEGENDA NA LINHA VERTICAL #
text_vline <- function(df, orientation) {
    list(
      x = median(df$idade),
      y = df %>%
        count(intervalo) %>%
        pull(n) %>%
        max()*1.05,
      text = paste("Mediana de:", median(df$idade),"anos"),
      showarrow = FALSE,
      xanchor = orientation)}
  

# FACTOR ORDER #

#todos_meses <- seq.Date(from = as.Date("2021-01-01"), to = as.Date("2024-12-01"), by = "month")
#todos_meses <- format(todos_meses, "%b-%Y")

# Detectar o locale do sistema
locale <- Sys.getlocale("LC_TIME")

# Definir os meses em português e inglês
todos_meses_pt <- c("jan-2020", "fev-2020", "mar-2020", "abr-2020", "mai-2020", "jun-2020",
                    "jul-2020", "ago-2020", "set-2020", "out-2020", "nov-2020", "dez-2020",
                    "jan-2021", "fev-2021", "mar-2021", "abr-2021", "mai-2021", "jun-2021",
                    "jul-2021", "ago-2021", "set-2021", "out-2021", "nov-2021", "dez-2021",
                    "jan-2022", "fev-2022", "mar-2022", "abr-2022", "mai-2022", "jun-2022",
                    "jul-2022", "ago-2022", "set-2022", "out-2022", "nov-2022", "dez-2022",
                    "jan-2023", "fev-2023", "mar-2023", "abr-2023", "mai-2023", "jun-2023",
                    "jul-2023", "ago-2023", "set-2023", "out-2023", "nov-2023", "dez-2023",
                    "jan-2024", "fev-2024", "mar-2024", "abr-2024", "mai-2024", "jun-2024",
                    "jul-2024", "ago-2024", "set-2024", "out-2024", "nov-2024", "dez-2024")

todos_meses_en <- c("Jan-2021", "Feb-2021", "Mar-2021", "Apr-2021", "May-2021", "Jun-2021",
                    "Jul-2021", "Aug-2021", "Sep-2021", "Oct-2021", "Nov-2021", "Dec-2021",
                    "Jan-2022", "Feb-2022", "Mar-2022", "Apr-2022", "May-2022", "Jun-2022",
                    "Jul-2022", "Aug-2022", "Sep-2022", "Oct-2022", "Nov-2022", "Dec-2022",
                    "Jan-2023", "Feb-2023", "Mar-2023", "Apr-2023", "May-2023", "Jun-2023",
                    "Jul-2023", "Aug-2023", "Sep-2023", "Oct-2023", "Nov-2023", "Dec-2023",
                    "Jan-2024", "Feb-2024", "Mar-2024", "Apr-2024", "May-2024", "Jun-2024",
                    "Jul-2024", "Aug-2024", "Sep-2024", "Oct-2024", "Nov-2024", "Dec-2024")

# Selecionar os meses com base no locale
if (grepl("Portuguese_Brazil.utf8", locale)) {
  todos_meses <- todos_meses_pt
} else {
  todos_meses <- todos_meses_pt
}


ord_levels_data_mes <- c("janeiro",
                         "fevereiro",
                         "março",
                         "abril",
                         "maio",
                         "junho",
                         "julho",
                         "agosto",
                         "setembro",
                         "outubro",
                         "novembro",
                         "dezembro")


color_map <- c("R" = "red",
                 "S" = "darkgreen",
                 "I" = "orange")



conditional <- function(condition, success) {
  if (condition) success else TRUE
}