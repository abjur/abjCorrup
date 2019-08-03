library(tidyverse)

source("R/calcula_tempo.R")
source("R/classifica_corrup.R")
source("R/classifica_desfecho.R")
source("R/classifica_movs.R")
source("R/gera_base_final.R")
source("R/spread_infos.R")
source("R/sumarize_movs.R")
source("R/gera_analises.R")

d <- readr::read_rds("data/d_parsed.rds")
tribunais <- c("JFAL", "JFRJ", "JFSP", "TJSP", "TJAL", "TJRJ", "TJDFT", "JFDF")
base_final <- purrr::map_dfr(tribunais, ~gera_base_final(d, .x))
readr::write_rds(base_final, "data/base_final.rds", compress = "xz")

base_final <- readr::read_rds("data/base_final.rds")

# filtros adicionais
# re_banned <- "[aA]liment|[Mm]edici|[pP]articular"
bd_final_assunto <- base_final %>% 
  mutate(assunto = if_else(is.na(assunto), objeto, assunto)) %>% 
  select(-objeto, -baixa, -situacao) %>% 
  mutate(copia_assunto = assunto,
         assunto = assunto %>% 
           tolower() %>% 
           abjutils::rm_accent() %>% 
           str_remove_all("[^a-z -]") %>% 
           str_squish()) %>% 
  filter(!is.na(assunto))
  
d_crimes <- crimes()$re %>% 
  set_names(crimes()$label) %>% 
  purrr::map_dfr(~{
    bd_final_assunto %>% 
      select(tribunal, n_processo, assunto) %>% 
      mutate(crime = str_extract(assunto, .x)) %>% 
      filter(!is.na(crime))
  }, .id = "crime_label") %>% 
  distinct(n_processo, .keep_all = TRUE) %>% 
  select(-assunto)

re_date <- "[0-9]{2}(-|/)[0-9]{2}(-|/)(19|20)[0-9]{2}"
d_nupps <- bd_final_assunto %>% 
  filter(!classe %in% c(
    "Procedimento Especial da Lei Antitóxicos - Criminal (Lei 11.343/06)",
    "Auto de Prisão em Flagrante",
    "Ação Penal de Competência do Júri",
    "Liberdade Provisória com ou sem fiança",
    "Petição - Criminal",
    "Processo de Apuração de Ato Infracional - ECA",
    "Habeas Corpus - Criminal",
    "Sequestro - Criminal",
    "Processo Crime Militar (art. 34 e ss do CPPM)",
    "Arresto / Hipoteca Legal - Criminal"
  )) %>% 
  inner_join(d_crimes, c("tribunal", "n_processo")) %>% 
  mutate(dt_dist = str_extract(data_distribuicao, re_date)) %>% 
  mutate(dt_dist = lubridate::dmy(dt_dist)) %>% 
  # check date
  filter(is.na(dt_dist) | (dt_dist < "2018-09-01" & dt_dist > "1970-01-01")) %>% 
  mutate(status = status %>% 
           abjutils::rm_accent() %>% 
           stringr::str_remove_all("[^a-zA-Z ]") %>% 
           stringr::str_squish() %>% 
           stringr::str_replace_na("Vazio") %>% 
           stringr::str_replace("^$", "Vazio")) %>% 
  select(tribunal,
         n_processo,
         classe,
         crime = crime_label,
         dt_dist,
         lugar, 
         status,
         # Fases
         fase_pre = pre_distribuicao,
         fase_inquerito = aceite_da_denuncia,
         fase_instrucao = instrucao_probatoria,
         fase_sentenca = sentenca,
         fase_recursal = recursos,
         # Tempos
         starts_with("tempo"),
         # Desfechos
         desf_prescricao = prescricao,
         desf_absolv = absolvido,
         desf_condenacao = condenacao,
         desf_morte = morte_do_agente,
         desf_arquivou_outro = arquivou,
         desf_sem_punicao = sem_punicao,
         desf_ativo = em_curso,
         desf_arquivou_denuncia = denuncia_arquivada
  ) %>% 
  replace_na(list(desf_arquivou_denuncia = FALSE,
                  classe = "Vazio")) %>% 
  mutate(classe = classe %>% 
           str_remove_all("[^[:alnum:] ]|[0-9]") %>% 
           str_squish() %>% 
           str_to_title()) %>% 
  # se condenou e arquivou -> condenou
  mutate(
    desf_arquivou_outro = case_when(
      desf_prescricao + desf_absolv + desf_condenacao + 
        desf_morte + desf_sem_punicao + desf_ativo +
        desf_arquivou_denuncia > 0 ~ FALSE,
      TRUE ~ desf_arquivou_outro
    ),
    desfecho = case_when(
      desf_prescricao ~ "Prescrição",
      desf_arquivou_denuncia ~ "Denúncia arquivada",
      desf_ativo ~ "Ativo",
      desf_morte ~ "Morte",
      desf_condenacao & desf_absolv ~ "Resultados mistos",
      desf_condenacao ~ "Condenação",
      desf_absolv ~ "Absolvição",
      desf_arquivou_outro ~ "Arquivado - outro",
      TRUE ~ "Inconclusivo"
    )
  ) %>% 
  mutate(desfecho = if_else(
    desfecho == "Ativo" & status %in% c("Extinto", "Cancelado", "Baixado"),
    "Arquivado - outro", desfecho)) %>% 
  select(-starts_with("desf_"))


## rascunhos e verificacoes

# d_nupps %>% 
#   group_by_at(vars(starts_with("desf_"))) %>% 
#   summarise(n = n()) %>% 
#   ungroup() %>% 
#   mutate(n_true = pmap_int(select(., starts_with("desf_")), sum)) %>% 
#   arrange(desc(n), n_true) %>% 
#   View()
# 
  # 
# glimpse(d_nupps)
# 
# d_nupps %>% 
#   count(tribunal, desf_arquivou_denuncia, desf_condenacao) %>% 
#   View()
# 
# 
# d_nupps %>% 
#   count(arquivamento > 0) %>% 
#   mutate(prop = n/sum(n))
# 
# d_nupps %>% 
#   
#   count(status, sort = TRUE)
#   
# 
# 
# d_nupps %>% 
#   filter(is.na(dt_dist2)) %>% 
#   select(data_distribuicao, dt_dist) %>% 
#   View
# 
# 
# 
# d_nupps %>%
#   tabela_contagem("crime_label")
