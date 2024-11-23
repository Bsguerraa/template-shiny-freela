library(tidyverse)
library(lubridate)
library(dygraphs)
library(plotly)
library(data.table)

# Utils
# Criar timestamp especifico:
# timestamp_especifico <- ISOdate(2024, 11, 13, 15, 30, 0) # ano, mês, dia, hora, minuto, segundo

### Geral

# Nomes e ID de pacientes
nomes_masculinos <- read.csv("data/ibge-mas-10000.csv")
nomes_masculinos <- nomes_masculinos %>% mutate(nc = nchar(nome)) %>% filter(nc <= 10)

nomes_femininos <- read.csv("data/ibge-fem-10000.csv")
nomes_femininos <- nomes_femininos %>% mutate(nc = nchar(nome)) %>% filter(nc <= 10)

names_id <- c(paste0(sample(nomes_masculinos$nome, 5000), "_", sample(111111:999999, 5000)),
                   paste0(sample(nomes_femininos$nome, 5000), "_", sample(111111:999999, 5000)))

# Nomes e ID dos médicos
practitioner_id <- sample(names_id, 100)

# Setores hospitalares
setores_hospitalares <- read.csv("data/setor_hospitalar.csv")

### Analytics 1 : Gestão de isolamento

# Criar variaçao aleatória para tempo de resposta (amostra - resultado) de acordo com o setor médico
setores_hospitalares <- setores_hospitalares %>% 
  mutate(dif_set = round(rnorm(25, sd = 10),0)*1000)

# Gerar dados artificiais para PRD 1
gestao_isolamento_tbl <- data.frame(PATIENT_ID = sample(names_id, 2400),
                                    PRACTITIONER_ID = sample(practitioner_id, 2400, replace = TRUE),
                                    UNIT_ID = sample(setores_hospitalares$UNIT_ID, 2400, replace = TRUE),
                                    ADMISSION_DATE = ISOdate(2023, 1:12, rep(1:20, each = 10*12), 11, 00, 0) - 28800,
                                    collection_date_time = ISOdate(2023, 1:12, rep(1:20, each = 10*12), 11, 00, 0)) %>%
  
  # Gerar variaçao aleatória para tempo de resposta (amostra - resultado)
  arrange(collection_date_time) %>%
  mutate(result_status_date_time = collection_date_time + (rep(c(13,10,8,9,5,7,6,5,3,2,4,1)*60*3, each = 200) + round(rnorm(2400, sd = 10),0))*60,
         RELEASE_DATE = result_status_date_time + round(rnorm(2400, sd = 30),0) + 60*60*6) %>%
  left_join(setores_hospitalares %>% select(UNIT_ID, dif_set), by = c("UNIT_ID")) %>%
  mutate(result_status_date_time = result_status_date_time + dif_set)

# Salvar arquivo
fwrite(gestao_isolamento_tbl, file = "data/gestao_isolamento_tbl.csv")

dataset <- gestao_isolamento_tbl %>% 
  mutate(dif = as.numeric(difftime(result_status_date_time, collection_date_time, units = "hours")),
         month = as.Date(floor_date(collection_date_time, unit = "month"))) |>
  group_by(month) |>
  summarise(dif = mean(dif))

### Analytics 2 : Consumo de antimicrobianos, DDD, DOT e LOT

# Refs: ATC codes https://atcddd.fhi.no/atc_ddd_index/
# AWaRe classification: https://www.who.int/publications/i/item/2021-aware-classification

# Tabela de DDD de acordo com recomendações da OMS
atb_ddd_oms <- read.csv2("data/atb_ddd.csv")

# Tradução do nome de antimicrobiano PT-EN
atb_translation <- read.csv("data/antibiotics_translation.csv")

# Lista da OSM com classe e código ATC de cada antimicrobiano
atb_aware_list <- read.csv2("data/who_aware_atb_list.csv")

# Doses e unidades de dose por apresentação do medicamento
atb_dose <- read.csv2("data/atb_doses.csv", dec = ".")

# Unificar tabelas de antimicrobianos
atb_dose2 <- atb_dose %>%
  mutate(DOSAGE_VALUE_G = ifelse(DOSAGE_UNIT == "mg", DOSAGE_VALUE_ORIGINAL/1000, DOSAGE_VALUE_ORIGINAL)) %>%
  mutate(DOSAGE_VALUE_G = round(DOSAGE_VALUE_G, 2)) %>%
  select(antibiotico, DOSAGE_VALUE_G)

atb_tbl <- atb_ddd_oms %>% 
  left_join(atb_translation, by = c("Nome" = "atb_pt")) %>%
  left_join(atb_aware_list, by = c("atb_en" = "Alias"))

antimicrob_tbl <- atb_tbl %>%
  select(Nome, ATC.code, Class, DDD_OMS) %>%
  rename(MEDICATION_CLASS_CODE = ATC.code,
         MEDICATION_NAME = Nome)

antimicrob_tbl2 <- atb_dose2 %>%
  left_join(antimicrob_tbl, by = c("antibiotico" = "MEDICATION_NAME"), 
            relationship = "many-to-many") %>%
  drop_na() %>%
  mutate(MEDICATION_NAME = factor(antibiotico),
         medication_code = 1:n()) %>%
  select(-antibiotico)

# Gerar dados artificiais de uso de antibióticos em pacientes

## Premissas: 
# Nome e ID de pacientes: Sorteado aleatoriamente.
# ADM_START_DATE: (Data de admissão) Data de início do tratamento do antimicrobiano, sorteados entre janeiro e junho de 2023
# UNIT_ID: Código do setor hospitalar. Sorteado aleatoriamente.
# medication_code: atribui número aleatório de acordo com lista de medicamentos disponíveis
# DOSAGE_TIMING_PERIOD: sorteia valores de 6, 8 ou 12 horas para período de administração do antimicrobiano
# DOSAGE_TIMING_PERIOD_UNIT: unidade do período de administração do antimicrobiano (horas)
# DOSAGE_ADM_DURATION: duração do tratamento, sorteio de valores entre 3 e 12 dias
# DOSAGE_ADM_DURATION_UNIT: duração do tratamento, em dias
# ADMISSION_ID: cria ID único para cada 'hospitalização'

# ADM_START_MONTH: mês de início do tratamento
# DOSAGE_TIMING_FREQUENCY: frequência diária da administração do medicamento
# ADM_END_DATE: data de encerramento do tratamento com determinado medicamento
# ADM_END_MONTH: mês de encerramento do tratamento com determinado medicamento

antimicrob_tbl3 <- data.frame(PATIENT_ID = sample(names_id, 2400),
                              ADM_START_DATE = sample(seq(as.Date("2023-01-01"),as.Date("2023-06-15"), by = "days"), 2400, replace = TRUE),
                              UNIT_ID = sample(setores_hospitalares$UNIT_ID, 2400, replace = TRUE),
                              medication_code = sample(1:nrow(antimicrob_tbl2), 2400, replace = TRUE),
                              DOSAGE_TIMING_PERIOD = sample(c(6,8,12), 2400, replace = TRUE),
                              DOSAGE_TIMING_PERIOD_UNIT = "horas",
                              DOSAGE_ADM_DURATION = sample(seq(3,12), 2400, replace = TRUE),
                              DOSAGE_ADM_DURATION_UNIT = "dias",
                              ADMISSION_ID = 1:2400) %>%
  
  mutate(ADM_START_MONTH = as.Date(floor_date(ADM_START_DATE, unit = "month")),
         DOSAGE_TIMING_FREQUENCY = 24/DOSAGE_TIMING_PERIOD,
         ADM_END_DATE = ADM_START_DATE + DOSAGE_ADM_DURATION,
         ADM_END_MONTH = as.Date(floor_date(ADM_END_DATE, unit = "month")))

# Criar troca na medicação para 10% dos pacientes
p_mudanca = nrow(antimicrob_tbl3)*0.1

# A partir da porcentagem de pacientes escolhida, atribui um novo medicamento e uma duração sorteada entre 2 e 6 dias
antimicrob_change_tbl <- antimicrob_tbl3[sample(1:nrow(antimicrob_tbl3), p_mudanca, replace = FALSE),] %>%
  mutate(ADM_START_DATE = ADM_END_DATE + 1,
         ADM_START_MONTH = as.Date(floor_date(ADM_START_DATE, unit = "month")),
         DOSAGE_ADM_DURATION = sample(seq(2,6), p_mudanca, replace = TRUE),
         medication_code = sample(1:nrow(antimicrob_tbl2), p_mudanca, replace = TRUE),
         ADM_END_DATE = ADM_START_DATE + DOSAGE_ADM_DURATION,
         ADM_END_MONTH = as.Date(floor_date(ADM_END_DATE, unit = "month")))

# Unifica as tabelas de prontuários eletrônicos, e anexa informações de medicamentos e setor hospitalar
antimicrob_tbl_final <- antimicrob_tbl3 %>%
  bind_rows(antimicrob_change_tbl) %>%
  left_join(setores_hospitalares %>% select(UNIT_ID, DESCRIPTION), by = c("UNIT_ID")) %>%
  left_join(antimicrob_tbl2, by = "medication_code") %>%
  select(ADMISSION_ID, UNIT_ID, DESCRIPTION, PATIENT_ID, ADM_START_DATE, ADM_START_MONTH, ADM_END_DATE, ADM_END_MONTH,
         DOSAGE_TIMING_FREQUENCY, DOSAGE_TIMING_PERIOD, DOSAGE_TIMING_PERIOD_UNIT,
         DOSAGE_ADM_DURATION, DOSAGE_ADM_DURATION_UNIT, MEDICATION_CLASS_CODE, MEDICATION_NAME, Class, 
         DOSAGE_VALUE_G, DDD_OMS) %>%
  mutate(DD = DOSAGE_VALUE_G * DOSAGE_TIMING_FREQUENCY)

# Salvar arquivo
fwrite(antimicrob_tbl_final, file = "data/antimicrob_tbl.csv")
fwrite(antimicrob_tbl %>% select(MEDICATION_NAME, MEDICATION_CLASS_CODE), file = "data/antimicrob_tbl_codes.csv")

# Analytics 3:

# ALERT_DATE: Data geração do alerta
# PATIENT_ID: Nome e ID de pacientes: Sorteado aleatoriamente.
# ALERT_ADHERENCE: Registro do aceite ao alerta gerado. Valor TRUE ou FALSE
# USER_ID: Usuário que realizou a ação do aceite ou rejeição. Sorteado aleatoriamente
# UNIT_ID: Código do setor hospitalar. Sorteado aleatoriamente.
# PRACTITIONER_ID: Médico prescritor. Sorteado aleatoriamente.
# ALERT_ADHERENCE_DATE: Data da Modificação prescrição - Alerta encerrado. Tempo aleatório semi-estruturado após a data de alerta.
# IMPLEMENTATION_ADHERENCE: Registro do implementação à adesão aceita. Valor TRUE ou FALSE

aceitabilidade_tbl <- data.frame(PATIENT_ID = sample(names_id, 2400),
                                 PRACTITIONER_ID = sample(practitioner_id, 2400, replace = TRUE),
                                 UNIT_ID = sample(setores_hospitalares$UNIT_ID, 2400, replace = TRUE),
                                 ALERT_DATE = ISOdate(2023, 1:12, rep(1:20, each = 10*12), 11, 00, 0) - 28800,
                                 ALERT_ADHERENCE = sample(c(TRUE, FALSE), 2400, prob = c(0.7, 0.3), replace = TRUE),
                                 USER_ID = sample(practitioner_id, 2400, replace = TRUE)) %>%
  mutate(ALERT_ADHERENCE_DATE = ALERT_DATE + (rep(c(13,10,8,9,5,7,6,5,3,2,4,1)*60*3, each = 200) + round(rnorm(2400, sd = 10),0))*60,
         IMPLEMENTATION_ADHERENCE = sample(c(TRUE, FALSE), 2400, replace = TRUE) & ALERT_ADHERENCE,
         ALERT_IMPLEMENTATION_DATE = ALERT_ADHERENCE_DATE,
         ALERT_IMPLEMENTATION_DATE = ALERT_IMPLEMENTATION_DATE + abs(round(rnorm(2400, sd = 10),0)*60*60),
         ALERT_WEEK = as.Date(floor_date(ALERT_DATE, unit = "week")),
         ALERT_MONTH = as.Date(floor_date(ALERT_DATE, unit = "month")),
         IMPLEMENTATION_WEEK = as.Date(floor_date(ALERT_IMPLEMENTATION_DATE, unit = "week")),
         IMPLEMENTATION_MONTH = as.Date(floor_date(ALERT_IMPLEMENTATION_DATE, unit = "month"))) %>%
  left_join(setores_hospitalares %>% select(UNIT_ID, DESCRIPTION), by = c("UNIT_ID")) 

aceitabilidade_tbl[!aceitabilidade_tbl$ALERT_ADHERENCE,"ALERT_IMPLEMENTATION_DATE"] <- NA
aceitabilidade_tbl[!aceitabilidade_tbl$ALERT_ADHERENCE,"IMPLEMENTATION_WEEK"] <- NA
aceitabilidade_tbl[!aceitabilidade_tbl$ALERT_ADHERENCE,"IMPLEMENTATION_MONTH"] <- NA

fwrite(aceitabilidade_tbl, file = "data/aceitabilidade_tbl.csv")

