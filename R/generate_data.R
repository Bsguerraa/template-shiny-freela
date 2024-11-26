# library(tidyverse)
# library(lubridate)
# library(dygraphs)
# library(plotly)
# library(data.table)
# 
# # Utils
# # Criar timestamp especifico:
# # timestamp_especifico <- ISOdate(2024, 11, 13, 15, 30, 0) # ano, mês, dia, hora, minuto, segundo
# 
# ### Geral
# 
# # Nomes e ID de pacientes
# nomes_masculinos <- read.csv("data/ibge-mas-10000.csv")
# nomes_masculinos <- nomes_masculinos %>% mutate(nc = nchar(nome)) %>% filter(nc <= 10)
# 
# nomes_femininos <- read.csv("data/ibge-fem-10000.csv")
# nomes_femininos <- nomes_femininos %>% mutate(nc = nchar(nome)) %>% filter(nc <= 10)
# 
# names_id <- c(paste0(sample(nomes_masculinos$nome, 5000), "_", sample(111111:999999, 5000)),
#                    paste0(sample(nomes_femininos$nome, 5000), "_", sample(111111:999999, 5000)))
# 
# # Nomes e ID dos médicos
# practitioner_id <- sample(names_id, 100)
# 
# # Setores hospitalares
# setores_hospitalares <- read.csv("data/setor_hospitalar.csv")
# 
# # Microorganismos
# microorganismos_especies <- read.csv("data/lista_microorganismos.csv")
# 
# 
# # Procedimentos TUSS
# procedimentos_tuss <- read.csv("data/procedimentos_tuss.csv")
# 
# procedimentos_tuss <- procedimentos_tuss %>% mutate(tuss_id_completo = paste0(TUSS, " - ", Procedimento),
#                                                     codigo_procedimento = 1:nrow(.))
# 
# # Refs: ATC codes https://atcddd.fhi.no/atc_ddd_index/
# # AWaRe classification: https://www.who.int/publications/i/item/2021-aware-classification
# 
# # Tabela de DDD de acordo com recomendações da OMS
# atb_ddd_oms <- read.csv2("data/atb_ddd.csv")
# 
# # Tradução do nome de antimicrobiano PT-EN
# atb_translation <- read.csv("data/antibiotics_translation.csv")
# 
# # Lista da OSM com classe e código ATC de cada antimicrobiano
# atb_aware_list <- read.csv2("data/who_aware_atb_list.csv")
# 
# # Doses e unidades de dose por apresentação do medicamento
# atb_dose <- read.csv2("data/atb_doses.csv", dec = ".")
# 
# # Unificar tabelas de antimicrobianos
# atb_dose2 <- atb_dose %>%
#   mutate(DOSAGE_VALUE_G = ifelse(DOSAGE_UNIT == "mg", DOSAGE_VALUE_ORIGINAL/1000, DOSAGE_VALUE_ORIGINAL)) %>%
#   mutate(DOSAGE_VALUE_G = round(DOSAGE_VALUE_G, 2)) %>%
#   dplyr::select(antibiotico, DOSAGE_VALUE_G)
# 
# atb_tbl <- atb_ddd_oms %>%
#   left_join(atb_translation, by = c("Nome" = "atb_pt")) %>%
#   left_join(atb_aware_list, by = c("atb_en" = "Alias"))
# 
# antimicrob_tbl <- atb_tbl %>%
#   dplyr::select(Nome, ATC.code, Class, DDD_OMS) %>%
#   rename(MEDICATION_CLASS_CODE = ATC.code,
#          MEDICATION_NAME = Nome)
# 
# antimicrob_tbl2 <- atb_dose2 %>%
#   left_join(antimicrob_tbl, by = c("antibiotico" = "MEDICATION_NAME"),
#             relationship = "many-to-many") %>%
#   drop_na() %>%
#   mutate(MEDICATION_NAME = factor(antibiotico),
#          medication_code = 1:n(),
#          medication_name_code = as.numeric(MEDICATION_NAME),
#          medication_price = factor(medication_name_code,
#                                    levels = c(1:max(medication_name_code)),
#                                    labels = seq(10, 100, length.out = max(21))
#          ),
#          medication_price = as.numeric(as.character(medication_price))) %>%
#   group_by(MEDICATION_NAME) %>%
#   mutate(g_ratio = DOSAGE_VALUE_G/max(DOSAGE_VALUE_G)) %>%
#   ungroup() %>%
#   mutate(medication_price = round(medication_price*g_ratio,2)) %>%
#   dplyr::select(-antibiotico, -medication_name_code, -g_ratio)
# 
# ### Analytics 1 : Gestão de isolamento
# 
# # Criar variaçao aleatória para tempo de resposta (amostra - resultado) de acordo com o setor médico
# setores_hospitalares <- setores_hospitalares %>%
#   mutate(dif_set = round(rnorm(25, sd = 10),0)*1000)
# 
# # Gerar dados artificiais para PRD 1
# gestao_isolamento_tbl <- data.frame(PATIENT_ID = sample(names_id, 2400),
#                                     PRACTITIONER_ID = sample(practitioner_id, 2400, replace = TRUE),
#                                     UNIT_ID = sample(setores_hospitalares$UNIT_ID, 2400, replace = TRUE),
#                                     ADMISSION_DATE = ISOdate(2023, 1:12, rep(1:20, each = 10*12), 11, 00, 0) - 28800,
#                                     collection_date_time = ISOdate(2023, 1:12, rep(1:20, each = 10*12), 11, 00, 0)
#                                     ) %>%
# 
#   # Gerar variaçao aleatória para tempo de resposta (amostra - resultado)
#   arrange(collection_date_time) %>%
#   mutate(result_status_date_time = collection_date_time + (rep(c(13,10,8,9,5,7,6,5,3,2,4,1)*60*3, each = 200) + round(rnorm(2400, sd = 10),0))*60,
#          RELEASE_DATE = result_status_date_time + round(rnorm(2400, sd = 30),0) + 60*60*6) %>%
#   left_join(setores_hospitalares %>% dplyr::select(UNIT_ID, dif_set), by = c("UNIT_ID")) %>%
#   mutate(result_status_date_time = result_status_date_time + dif_set)
# 
# 
# # Salvar arquivo
# fwrite(gestao_isolamento_tbl, file = "data/gestao_isolamento_tbl.csv")
# 
# ### Analytics 2 : Consumo de antimicrobianos, DDD, DOT e LOT
# 
# 
# # Gerar dados artificiais de uso de antibióticos em pacientes
# 
# ## Premissas:
# # Nome e ID de pacientes: Sorteado aleatoriamente.
# # THERAPY_START_DATE: (Data de admissão) Data de início do tratamento do antimicrobiano, sorteados entre janeiro e junho de 2023
# # UNIT_ID: Código do setor hospitalar. Sorteado aleatoriamente.
# # medication_code: atribui número aleatório de acordo com lista de medicamentos disponíveis
# # DOSAGE_TIMING_PERIOD: sorteia valores de 6, 8 ou 12 horas para período de administração do antimicrobiano
# # DOSAGE_TIMING_PERIOD_UNIT: unidade do período de administração do antimicrobiano (horas)
# # THERAPY_DURATION: duração do tratamento, sorteio de valores entre 3 e 12 dias
# # THERAPY_DURATION_UNIT: duração do tratamento, em dias
# # ADMISSION_ID: cria ID único para cada 'hospitalização'
# 
# # THERAPY_START_MONTH: mês de início do tratamento
# # DOSAGE_TIMING_FREQUENCY: frequência diária da administração do medicamento
# # THERAPY_END_DATE: data de encerramento do tratamento com determinado medicamento
# # THERAPY_END_MONTH: mês de encerramento do tratamento com determinado medicamento
# 
# antimicrob_tbl3 <- data.frame(PATIENT_ID = sample(names_id, 2400),
#                               THERAPY_START_DATE = sample(seq(as.Date("2023-01-10"),as.Date("2023-06-15"), by = "days"), 2400, replace = TRUE),
#                               UNIT_ID = sample(setores_hospitalares$UNIT_ID, 2400, replace = TRUE),
#                               medication_code = sample(1:nrow(antimicrob_tbl2), 2400, replace = TRUE),
#                               PRACTITIONER_ID = sample(practitioner_id, 2400, replace = TRUE),
#                               DOSAGE_TIMING_PERIOD = sample(c(6,8,12), 2400, replace = TRUE),
#                               DOSAGE_TIMING_PERIOD_UNIT = "horas",
#                               THERAPY_DURATION = sample(seq(3,12), 2400, replace = TRUE),
#                               THERAPY_DURATION_UNIT = "dias",
#                               ADMISSION_ID = 1:2400,
#                               MICROORGANISM_ID = sample(microorganismos_especies$Nome, 2400, replace = TRUE),
#                               codigo_procedimento = sample(1:nrow(procedimentos_tuss), 2400, replace = TRUE)) %>%
# 
#   mutate(THERAPY_START_MONTH = as.Date(floor_date(THERAPY_START_DATE, unit = "month")),
#          THERAPY_START_WEEK = as.Date(floor_date(THERAPY_START_DATE, unit = "week")),
#          DOSAGE_TIMING_FREQUENCY = 24/DOSAGE_TIMING_PERIOD,
#          THERAPY_END_DATE = THERAPY_START_DATE + THERAPY_DURATION,
#          THERAPY_END_MONTH = as.Date(floor_date(THERAPY_END_DATE, unit = "month")),
#          THERAPY_END_WEEK = as.Date(floor_date(THERAPY_END_DATE, unit = "week")),
#          ADM_START_DATE = THERAPY_START_DATE - sample(c(0,1,2,3),2400,prob = c(0.6,0.2,0.1,0.1),replace = TRUE)
#   ) %>%
#   left_join(procedimentos_tuss, by = c("codigo_procedimento")) %>%
#   select(-codigo_procedimento)
# 
# # Criar troca na medicação para 10% dos pacientes
# p_add = 0.1
# p_mudanca = nrow(antimicrob_tbl3)*0.1
# 
# # A partir da porcentagem de pacientes escolhida, atribui um novo medicamento e uma duração sorteada entre 2 e 6 dias
# antimicrob_change_tbl <- antimicrob_tbl3[sample(1:nrow(antimicrob_tbl3), p_mudanca, replace = FALSE),] %>%
#   mutate(THERAPY_START_DATE = THERAPY_END_DATE + 1,
#          THERAPY_START_MONTH = as.Date(floor_date(THERAPY_START_DATE, unit = "month")),
#          THERAPY_START_WEEK = as.Date(floor_date(THERAPY_START_DATE, unit = "week")),
#          THERAPY_DURATION = sample(seq(2,6), p_mudanca, replace = TRUE),
#          medication_code = sample(1:nrow(antimicrob_tbl2), p_mudanca, replace = TRUE),
#          THERAPY_END_DATE = THERAPY_START_DATE + THERAPY_DURATION,
#          THERAPY_END_MONTH = as.Date(floor_date(THERAPY_END_DATE, unit = "month")),
#          THERAPY_END_WEEK = as.Date(floor_date(THERAPY_END_DATE, unit = "week")))
# 
# END_ADD_DAYS <- antimicrob_tbl3 %>% dplyr::select(ADMISSION_ID) %>%
#   group_by(ADMISSION_ID) %>%
#   mutate(add = 0) %>%
#   ungroup() %>%
#   mutate(add = sample(c(0,1,2),2400,prob = c(0.8,0.1,0.1),replace = TRUE))
# 
# # Unifica as tabelas de prontuários eletrônicos, e anexa informações de medicamentos e setor hospitalar
# antimicrob_tbl_final <- antimicrob_tbl3 %>%
#   bind_rows(antimicrob_change_tbl) %>%
#   group_by(ADMISSION_ID) %>%
#   mutate(ADM_START_DATE = min(ADM_START_DATE),
#          ADM_END_DATE = max(THERAPY_END_DATE)
#   ) %>%
#   ungroup() %>%
#   left_join(END_ADD_DAYS, by = "ADMISSION_ID") %>%
#   mutate(ADM_START_MONTH =  as.Date(floor_date(ADM_START_DATE, unit = "month")),
#          ADM_START_WEEK =  as.Date(floor_date(ADM_START_DATE, unit = "week")),
#          ADM_END_DATE =ADM_END_DATE + add,
#          ADM_END_MONTH =  as.Date(floor_date(ADM_END_DATE, unit = "month")),
#          ADM_END_WEEK =  as.Date(floor_date(ADM_END_DATE, unit = "week")),
#          ADMISSION_DURATION = as.numeric(ADM_END_DATE - ADM_START_DATE)) %>%
#   left_join(setores_hospitalares %>% dplyr::select(UNIT_ID, DESCRIPTION), by = c("UNIT_ID")) %>%
#   left_join(antimicrob_tbl2, by = "medication_code") %>%
#   dplyr::select(ADMISSION_ID, UNIT_ID, DESCRIPTION, PATIENT_ID, PRACTITIONER_ID, ADM_START_DATE, ADM_START_MONTH, ADM_START_WEEK,
#          THERAPY_START_DATE, THERAPY_START_MONTH, THERAPY_START_WEEK, THERAPY_END_DATE, THERAPY_END_MONTH, THERAPY_END_WEEK,
#          ADM_END_DATE, ADM_END_MONTH, ADM_END_WEEK, ADMISSION_DURATION,
#          DOSAGE_TIMING_FREQUENCY, DOSAGE_TIMING_PERIOD, DOSAGE_TIMING_PERIOD_UNIT,
#          THERAPY_DURATION, THERAPY_DURATION_UNIT, MICROORGANISM_ID, MEDICATION_CLASS_CODE, MEDICATION_NAME, Class,
#          DOSAGE_VALUE_G, DDD_OMS, medication_price, TUSS, Procedimento, tuss_id_completo) %>%
#   mutate(DD = DOSAGE_VALUE_G * DOSAGE_TIMING_FREQUENCY,
#          TOTAL_COST = DOSAGE_TIMING_FREQUENCY * THERAPY_DURATION * medication_price)
# 
# # Salvar arquivo
# fwrite(antimicrob_tbl_final, file = "data/antimicrob_tbl.csv")
# fwrite(antimicrob_tbl %>% dplyr::select(MEDICATION_NAME, MEDICATION_CLASS_CODE), file = "data/antimicrob_tbl_codes.csv")
# 
# # Analytics 3:
# 
# # ALERT_DATE: Data geração do alerta
# # PATIENT_ID: Nome e ID de pacientes: Sorteado aleatoriamente.
# # ALERT_ADHERENCE: Registro do aceite ao alerta gerado. Valor TRUE ou FALSE
# # USER_ID: Usuário que realizou a ação do aceite ou rejeição. Sorteado aleatoriamente
# # UNIT_ID: Código do setor hospitalar. Sorteado aleatoriamente.
# # PRACTITIONER_ID: Médico prescritor. Sorteado aleatoriamente.
# # ALERT_ADHERENCE_DATE: Data da Modificação prescrição - Alerta encerrado. Tempo aleatório semi-estruturado após a data de alerta.
# # IMPLEMENTATION_ADHERENCE: Registro do implementação à adesão aceita. Valor TRUE ou FALSE
# # SAVING_COST: custo evitado por cada implementação aceita
# 
# custos <- seq(100,1000, by= 100)
# 
# aceitabilidade_tbl <- data.frame(PATIENT_ID = sample(names_id, 2400),
#                                  PRACTITIONER_ID = sample(practitioner_id, 2400, replace = TRUE),
#                                  UNIT_ID = sample(setores_hospitalares$UNIT_ID, 2400, replace = TRUE),
#                                  ALERT_DATE = ISOdate(2023, 1:12, rep(1:20, each = 10*12), 11, 00, 0) - 28800,
#                                  ALERT_ADHERENCE = sample(c(TRUE, FALSE), 2400, prob = c(0.7, 0.3), replace = TRUE),
#                                  USER_ID = sample(practitioner_id, 2400, replace = TRUE),
#                                  COST = sample(custos, 2400, replace = TRUE),
#                                  THERAPY_DURATION = sample(seq(8,12), 2400, replace = TRUE),
#                                  medication_code = sample(1:nrow(antimicrob_tbl2), 2400, replace = TRUE)) %>%
#   mutate(ALERT_ADHERENCE_DATE = ALERT_DATE + (rep(c(13,10,8,9,5,7,6,5,3,2,4,1)*60*3, each = 200) + round(rnorm(2400, sd = 10),0))*60,
#          IMPLEMENTATION_ADHERENCE = sample(c(TRUE, FALSE), 2400, replace = TRUE) & ALERT_ADHERENCE,
#          ALERT_IMPLEMENTATION_DATE = ALERT_ADHERENCE_DATE,
#          ALERT_IMPLEMENTATION_DATE = ALERT_IMPLEMENTATION_DATE + abs(round(rnorm(2400, sd = 10),0)*60*60),
#          ALERT_WEEK = as.Date(floor_date(ALERT_DATE, unit = "week")),
#          ALERT_MONTH = as.Date(floor_date(ALERT_DATE, unit = "month")),
#          # Reducao do custo caso implementacao ocorra
#          SAVING_COST = COST*sample(c(0.9, 0.7, 0.5)),
#          EFFECTIVE_COST = ifelse(IMPLEMENTATION_ADHERENCE, SAVING_COST, COST),
#          THERAPY_DURATION = ifelse(IMPLEMENTATION_ADHERENCE, round(THERAPY_DURATION*sample(c(0.8, 0.6, 0.4)), 0),THERAPY_DURATION),
#          IMPLEMENTATION_WEEK = as.Date(floor_date(ALERT_IMPLEMENTATION_DATE, unit = "week")),
#          IMPLEMENTATION_MONTH = as.Date(floor_date(ALERT_IMPLEMENTATION_DATE, unit = "month"))) %>%
#   left_join(setores_hospitalares %>% dplyr::select(UNIT_ID, DESCRIPTION), by = c("UNIT_ID")) %>%
#   left_join(antimicrob_tbl2 %>% dplyr::select(MEDICATION_NAME, medication_code),
#             by = "medication_code") %>%
#   dplyr::select(-medication_code)
# 
# aceitabilidade_tbl[!aceitabilidade_tbl$ALERT_ADHERENCE,"ALERT_IMPLEMENTATION_DATE"] <- NA
# aceitabilidade_tbl[!aceitabilidade_tbl$ALERT_ADHERENCE,"IMPLEMENTATION_WEEK"] <- NA
# aceitabilidade_tbl[!aceitabilidade_tbl$ALERT_ADHERENCE,"IMPLEMENTATION_MONTH"] <- NA
# 
# fwrite(aceitabilidade_tbl, file = "data/aceitabilidade_tbl.csv")
# 
