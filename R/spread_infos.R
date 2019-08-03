spread_jfal <- function(tabela_infos){
  
  arruma_coluna_autuado <- function(key, value, type = 'key'){
    
    autuado_flag <- str_detect(key, regex("autuado", ignore_case = TRUE))
    extracao_datas <- str_extract(key, "[0-9]{2}/[0-9]{2}/[0-9]{4}")
    
    value2 = case_when(
      autuado_flag ~ extracao_datas,
      TRUE ~ value
    )
    
    key2 = case_when(
      autuado_flag ~ "data_distribuicao",
      TRUE ~ key
    )
    
    if(type == 'key'){
      return(key2)
    } else {
      return(value2)
    }
  }
  
  tabela_infos %>% 
    mutate(key2 = arruma_coluna_autuado(key, value, 'key'),
           value2 = arruma_coluna_autuado(key, value, 'value')) %>% 
    select(-key, -value) %>% 
    rename(key = key2, value = value2) %>% 
    spread(key, value) %>% 
    janitor::clean_names()
}

spread_jfrj <- function(tabela_infos){
  arruma_coluna_autuado <- function(key, value, type = 'key'){
    
    autuado_flag <- str_detect(key, regex("autuado", ignore_case = TRUE))
    extracao_datas <- str_extract(key, "[0-9]{2}/[0-9]{2}/[0-9]{4}")
    
    value2 = case_when(
      autuado_flag ~ extracao_datas,
      TRUE ~ value
    )
    
    key2 = case_when(
      autuado_flag ~ "data_distribuicao",
      TRUE ~ key
    )
    
    if(type == 'key'){
      return(key2)
    } else {
      return(value2)
    }
  }
  
  tabela_infos %>% 
    mutate(key2 = arruma_coluna_autuado(key, value, 'key'),
           value2 = arruma_coluna_autuado(key, value, 'value')) %>% 
    select(-key, -value) %>% 
    rename(key = key2, value = value2) %>% 
    group_by(n_processo, key) %>% 
    summarise(value = paste0(value, collapse = ", ")) %>% 
    ungroup() %>% 
    spread(key, value) %>% 
    janitor::clean_names()
}

spread_jfsp <- function(tabela_infos){
  
  arruma_coluna_autuado <- function(key, value, type = 'key'){
    
    autuado_flag <- str_detect(key, regex("autuado", ignore_case = TRUE))
    extracao_datas <- str_extract(key, "[0-9]{2}/[0-9]{2}/[0-9]{4}")
    
    value2 = case_when(
      autuado_flag ~ extracao_datas,
      TRUE ~ value
    )
    
    key2 = case_when(
      autuado_flag ~ "data_distribuicao",
      TRUE ~ key
    )
    
    if(type == "key"){
      return(key2)
    } else {
      return(value2)
    }
  }
  
  tabela_infos %>% 
    mutate(key2 = arruma_coluna_autuado(key, value, 'key'),
           value2 = arruma_coluna_autuado(key, value, 'value')) %>% 
    select(-key, -value) %>% 
    rename(key = key2, value = value2) %>% 
    group_by(n_processo, key) %>% 
    summarise(value = paste0(value, collapse = ", ")) %>% 
    ungroup() %>% 
    spread(key, value) %>% 
    janitor::clean_names()
}

spread_tjal <- function(tabela_infos){
  tabela_infos %>% 
    mutate(key = case_when(
      str_detect(value, "R\\$") ~ "Valor",
      str_detect(value, "Vara|CEJUSC") ~ "Lugar",
      TRUE ~ key)) %>% 
    group_by(n_processo, key) %>% 
    summarise(value = paste0(value, collapse = "")) %>% 
    ungroup() %>% 
    filter(str_detect(key, "Processo|Lugar|Distribuição|Valor|Assunto|Classe")) %>% 
    spread(key, value) %>% 
    janitor::clean_names() %>% 
    purrr::set_names(abjutils::rm_accent)
}

spread_tjdft <- function(tabela_infos){
  tabela_infos %>% 
    mutate(key = str_remove_all(key, "i_")) %>% 
    spread(key, value) %>% 
    janitor::clean_names() %>% 
    purrr::set_names(abjutils::rm_accent)
}

spread_tjrj <- function(tabela_infos){
  classifica_key_tjrj <- function(key, value){
    case_when(
      str_detect(value, regex("comarca", ignore_case = TRUE)) ~ "comarca",
      str_detect(value, "[0-9]{7}-[0-9]{2}\\.[0-9]{4}\\.8\\.19\\.[0-9]{4}") ~ "n_processo",
      str_detect(value, "[0-9]{4}\\.[0-9]{3}\\.[0-9]{6}") ~ "n_processo_antigo",
      str_detect(value, regex("distribu[íi]do", ignore_case = TRUE)) ~ "data_distribuicao",
      str_detect(value, regex("vara", ignore_case = TRUE)) ~ "vara",
      str_detect(value, "[Aa]utor") ~ "autor",
      str_detect(value, "[Rr][eé]u ") ~ "reu",
      str_detect(value, "[Aa]cusado") ~ "acusado",
      str_detect(value, "[Ii]ndiciado") ~ "indiciado",
      str_detect(value, "[Dd]enunciado") ~ "denunciado",
      str_detect(value, regex("testemunha", ignore_case = TRUE)) ~ "testemunhas",
      str_detect(key, regex("alterado", ignore_case = TRUE))|
        str_detect(value, regex("alterado", ignore_case = TRUE)) ~ "alteracao",
      str_detect(value, regex("tj", ignore_case = TRUE))|
        str_detect(key, regex("tj", ignore_case = TRUE))~ "data_consulta",
      TRUE ~ key
    )
  }
  
  tabela_infos %>% 
    mutate(key = classifica_key_tjrj(key, value)) %>% 
    filter(!str_detect(key, "key")) %>%
    group_by(n_processo, key) %>%
    summarise(value = paste0(value, collapse = ", ")) %>% 
    ungroup() %>% 
    spread(key, value) %>% 
    janitor::clean_names()
}

spread_tjsp <- function(tabela_infos){
  tabela_infos %>% 
    mutate(key = case_when(
      str_detect(value, "R\\$") ~ "Valor",
      str_detect(value, "Vara|CEJUSC") ~ "Lugar",
      TRUE ~ key)) %>% 
    group_by(n_processo, key) %>% 
    summarise(value = paste0(value, collapse = "")) %>% 
    ungroup() %>% 
    filter(str_detect(key, "Processo|Lugar|Distribuição|Valor|Assunto|Classe")) %>% 
    spread(key, value) %>% 
    janitor::clean_names() %>% 
    purrr::set_names(abjutils::rm_accent)
}

filtra_e_desaninha_tribunal <- function(d, trib, corrupcao = TRUE){
  resultado <- d %>% 
    filter(tribunal == trib)
  
  if(corrupcao){
    resultado <- resultado %>% 
      filter(corrup)
  }
  
  resultado <- resultado %>% 
    unnest(infos)
  
  return(resultado)
}

spread_jfdf <- function(tabela_infos) {
  tabela_infos %>% 
    filter(!is.na(key)) %>% 
    spread(key, value) %>% 
    transmute(n_processo,
              baixa = NA_character_,
              classe, 
              assunto = assunto_da_peticao,
              objeto = assunto_da_peticao,
              data_distribuicao = data_de_autuacao,
              lugar = vara,
              situacao = NA_character_,
              status = NA_character_)
}

spread_infos <- function(d_parsed){
  
  funcoes_spread <- c(spread_jfal, spread_jfrj, spread_jfsp,
                      spread_tjal, spread_tjdft, spread_jfdf,
                      spread_tjrj, spread_tjsp)
  
  tribs <- c("JFAL", "JFRJ", "JFSP", "TJAL", "TJDFT", "JFDF", "TJRJ", "TJSP")
  
  corrup <- c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE)
  
  lista_de_parametros <- list(funcoes = funcoes_spread,
                              tribunal = tribs,
                              corrupcao = corrup)
  
  infos_parseadas <- purrr::pmap(lista_de_parametros, function(funcoes, tribunal, corrupcao){
    d_parsed %>% 
      filtra_e_desaninha_tribunal(tribunal, corrupcao) %>% 
      funcoes()
  }) %>% 
    set_names(tribs)
  
  jfal_consolidado <- infos_parseadas$JFAL %>% 
    select(tribunal, n_processo, baixa_definitiva,
           classe, objetos, data_distribuicao, localizacao_atual) %>% 
    rename(objeto = objetos, baixa = baixa_definitiva,
           lugar = localizacao_atual)
  
  jfrj_consolidado <- infos_parseadas$JFRJ %>%
    mutate(tribunal = "JFRJ") %>% 
    select(tribunal, n_processo, baixa, objetos, data_distribuicao) %>% 
    rename(objeto = objetos)
  
  jfsp_consolidado <- infos_parseadas$JFSP %>% 
    mutate(tribunal = "JFSP") %>% 
    select(tribunal, n_processo, situacao, classe, assunto, tipo_distribuicao, secretaria) %>% 
    rename(objeto = assunto,
           data_distribuicao = tipo_distribuicao,
           lugar = secretaria)
  
  tjal_consolidado <- infos_parseadas$TJAL %>% 
    mutate(tribunal = "TJAL",
           corrup = str_detect(assunto, regex_assuntos())) %>% 
    filter(corrup) %>% 
    select(tribunal, n_processo, processo, assunto, classe, distribuicao, lugar) %>% 
    rename(data_distribuicao = distribuicao,
           status = processo)
  
  tjdft_consolidado <- infos_parseadas$TJDFT %>% 
    mutate(tribunal = "TJDFT") %>% 
    select(tribunal, n_processo, assunto_processual, classe_processual, data_distribuicao, descricao_vara, nome_circunscricao) %>% 
    mutate(lugar = paste0(descricao_vara, ", ", nome_circunscricao)) %>% 
    rename(assunto = assunto_processual,
           classe = classe_processual) %>% 
    select(-descricao_vara, -nome_circunscricao)
  
  tjrj_consolidado <- infos_parseadas$TJRJ %>% 
    mutate(tribunal = "TJRJ") %>% 
    select(tribunal, n_processo, assunto, classe, data_distribuicao, vara, comarca) %>% 
    mutate(lugar = paste0(vara, ", ", comarca)) %>% 
    select(-vara, -comarca)
  
  tjsp_consolidado <- infos_parseadas$TJSP %>% 
    mutate(tribunal = "TJSP") %>% 
    select(tribunal, n_processo, processo, assunto, classe, distribuicao, lugar) %>% 
    rename(data_distribuicao = distribuicao,
           status = processo)
  
  jfdf_consolidado <- infos_parseadas$JFDF %>% 
    mutate(tribunal = "JFDF")
  
  bind_rows(
    jfal_consolidado,
    jfrj_consolidado,
    jfsp_consolidado,
    tjal_consolidado,
    tjdft_consolidado,
    jfdf_consolidado,
    tjrj_consolidado,
    tjsp_consolidado
  ) %>% 
    mutate(n_processo = str_remove_all(n_processo, "[-.]") %>% 
             str_extract("[0-9]+"))
}
