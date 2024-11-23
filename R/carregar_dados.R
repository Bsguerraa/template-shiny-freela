library(data.table)  

# LEITURA DOS DADOS 

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

antimicrob_tbl_final <- fread(file = "data/antimicrob_tbl.csv")

antimicrob_date_range <- range(antimicrob_tbl_final$ADM_START_DATE, na.rm = TRUE)

antimicrob_setores_list <- c("Todos", sort(unique(antimicrob_tbl_final$DESCRIPTION)))
#names(gestao_isolamento_setores_list) = gestao_isolamento_setores_list
