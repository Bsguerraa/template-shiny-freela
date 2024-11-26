require(dplyr)

gestao_isolamento_tbl_csv <- read.csv(file = "data/gestao_isolamento_tbl.csv")
setores_hospitalares <- read.csv("data/setor_hospitalar.csv")

aceitabilidade_tbl <- read_csv(file = "data/aceitabilidade_tbl.csv")

aceitabilidade_date_range <- range(c(range(aceitabilidade_tbl$ALERT_DATE, na.rm = TRUE),
                                     range(aceitabilidade_tbl$ALERT_ADHERENCE_DATE, na.rm = TRUE)))

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

gestao_isolamento_range <- as.Date(range(c(range(gestao_isolamento_tbl$ADMISSION_DATE),
                                           range(gestao_isolamento_tbl$collection_date_time),
                                           range(gestao_isolamento_tbl$result_status_date_time),
                                           range(gestao_isolamento_tbl$RELEASE_DATE))))


antimicrob_tbl_final <- read_csv(file = "data/antimicrob_tbl.csv")

antimicrob_tbl_codes <- read.csv(file = "data/antimicrob_tbl_codes.csv")

antimicrob_date_range <- range(antimicrob_tbl_final$THERAPY_START_DATE, na.rm = TRUE)

antimicrob_setores_list <- sort(unique(antimicrob_tbl_final$DESCRIPTION))