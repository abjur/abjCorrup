lista_de_regex <- function(){
  list(
    despacho_decisao = c("Despacho", "Decisão", "Denúncia recebida"),
    audiencias = c("Audiência Não Realizada", "Audiência Designada", "Audiência Não Realizada", "Audiência Realizada", "Audiência Adiada", "Audiência Convertida em Diligência"),
    arquivamento = "Arquivamento"
  )
}

summarize_movs <- function(tabela_movs){
  
  movimentacoes_de_interesse <- lista_de_regex()
  
  movimentacoes_classificadas <- tabela_movs %>% 
    janitor::clean_names() %>% 
    mutate(
      descricao = titulo,
      descricao = str_to_lower(descricao) %>% 
        abjutils::rm_accent(),
      texto = str_to_lower(texto) %>% 
        abjutils::rm_accent(),
      descricao_e_titulo = paste0(descricao, texto),
      tipo_movimentacao = classifica_tipo_movimentacao(descricao_e_titulo),
      agente_sin = classifica_agente(descricao_e_titulo, tipo_movimentacao))

  movimentacoes_unicas <- length(movimentacoes_classificadas$tipo_movimentacao %>% unique)
  
  message(paste0("Temos ", movimentacoes_unicas, " classificações distintas. Deveria ser mais ou menos umas 50"))
  
  movimentacoes_ultra_classificadas <- movimentacoes_classificadas %>% 
    mutate(reu = str_detect(agente_sin, "Réu")|str_detect(tipo_movimentacao, regex("recebimento", ignore_case = TRUE)),
           reu = case_when(
             is.na(reu) ~ FALSE,
             TRUE ~ reu
           ),
           mp = str_detect(agente_sin, "MP")|str_detect(tipo_movimentacao, regex("recebimento", ignore_case = TRUE)),
           mp = case_when(
             is.na(mp) ~ FALSE,
             TRUE ~ mp
           ),
           j2 = str_detect(agente_sin, "J2")|str_detect(tipo_movimentacao, regex("recebimento", ignore_case = TRUE)),
           j2 = case_when(
             is.na(j2) ~ FALSE,
             TRUE ~ j2
           ),
           fecha = !str_detect(tipo_movimentacao, regex("concluso|conclusao", ignore_case = TRUE)) & str_detect(tipo_movimentacao, regex("recebimento|procedencia|prescricao|despacho|decisao|sentença", ignore_case = TRUE)),
           abre = str_detect(tipo_movimentacao, regex("remessa|carga|concluso|conclusao", ignore_case = TRUE)),
           juiz = str_detect(tipo_movimentacao, regex("concluso|conclusao|decis[ãa]o|despacho|sentença|proced[êe]ncia|presci[cç][ãa]o|arquivamento", ignore_case = TRUE)), 
           outras_partes = str_detect(tipo_movimentacao, regex("recebimento|carga|remessa", ignore_case = TRUE))) %>% 
    arrange(n_processo, data) %>% 
    group_by(n_processo) %>% 
    #filter(data <= min(data[tipo_movimentacao == "Distribuicao"])) %>% 
    mutate(
      id = 1:n(),
      primeira_distribuicao = procura_marco(id, tipo_movimentacao, "Distribuição", "min"),
      primeira_distribuicao = ifelse(all(!primeira_distribuicao) & id == 1, TRUE, primeira_distribuicao),
      data_primeira_dist = min(data[primeira_distribuicao], na.rm = TRUE),
      tipo_movimentacao = ifelse(data <= data_primeira_dist, "Pré distribuição", tipo_movimentacao),
      primeiro_despacho = procura_marco(id, tipo_movimentacao, movimentacoes_de_interesse$despacho_decisao, 'min'),  
      ultima_audiencia = procura_marco(id, tipo_movimentacao, movimentacoes_de_interesse$audiencias, 'max'),
      ultima_sentenca = procura_marco(id, tipo_movimentacao, "Sentença", 'max'),
      ultimo_concluso_sentenca = procura_marco(id, tipo_movimentacao, "Concluso para sentença", 'max'),
      ultimo_arquivamento = procura_marco(id, tipo_movimentacao, movimentacoes_de_interesse$arquivamento, 'max'),
      fase = case_when(
        data < min(data[primeira_distribuicao], na.rm = TRUE) ~ "Pré distribuição",
        lag(primeira_distribuicao) ~ "Aceite da denúncia",
        lag(primeiro_despacho) ~ "Instrução probatória",
        lag(ultimo_concluso_sentenca) ~ "Elaboração da sentença",
        lag(ultima_sentenca) ~ "Recursos",
        lag(ultimo_arquivamento) ~ "Arquivamento",
        TRUE ~ NA_character_
      ),
      fase = ifelse(primeira_distribuicao, "Distribuição", fase),
      fase = ifelse(ultimo_concluso_sentenca, "Elaboração da sentença", fase)) %>% 
    fill(fase)
  
  desfechos_processo <- movimentacoes_ultra_classificadas %>%
    group_by(n_processo, fase) %>% 
    mutate(id2 = 1:n()) %>% 
    summarise(
      data_inicio = min(data),
      data_fim = max(data),
      tempo_reu = calcula_tempo(data, abre, fecha, reu),
      tempo_j2 = calcula_tempo(data, abre, fecha, j2),
      tempo_mp = calcula_tempo(data, abre, fecha, mp),
      tempo_juiz = calcula_tempo(data, abre, fecha, juiz),
      primeiro_mandado_expedido = min(data[procura_marco(id2, tipo_movimentacao, "Mandado Expedido", "min")]),
      ultimo_mandado_cumprido = max(data[procura_marco(id2, tipo_movimentacao, "Mandado/Ofício Cumprido", "max")]),
      numero_audiencias = sum(str_detect(tipo_movimentacao, "Audiência Realizada")),
      numero_mandados = sum(str_detect(tipo_movimentacao, "Mandado Expedido")),
      numero_cartas = sum(str_detect(tipo_movimentacao, "Carta"))
    ) %>% 
    mutate(tempo_total = data_fim - data_inicio,
           tempo_mandado = ultimo_mandado_cumprido - primeiro_mandado_expedido)
  
  return(desfechos_processo)
}

spread_desfecho_processo <- function(desfecho_processo, trib = "nao importa"){
  
  desfecho_processo_sanitizado <- desfecho_processo %>% 
    mutate(tempo_mandado = ifelse(is.infinite(tempo_mandado), 0, tempo_mandado))
  
  tempos_totais <- desfecho_processo_sanitizado %>% 
    group_by(n_processo) %>% 
    summarise(
      tempo_reu = sum(tempo_reu),
      tempo_j2 = sum(tempo_j2),
      tempo_mp = sum(tempo_mp),
      tempo_juiz = sum(tempo_juiz),
      tempo_mandado = sum(tempo_mandado),
      numero_audiencias = sum(numero_audiencias),
      numero_mandados = sum(numero_mandados),
      numero_cartas = sum(numero_cartas)
    )
  
  tempos_por_fase <- desfecho_processo_sanitizado %>% 
    filter(!is.na(fase)) %>% 
    mutate(fase = case_when(
      fase == "Aceite da denúncia" ~ "aceite_da_denuncia",
      fase == "Instrução probatória" ~ "instrucao_probatoria",
      fase == "Elaboração da sentença" ~ "sentenca",
      fase == "Recursos" ~ "recursos",
      fase == "Arquivamento" ~ "arquivamento",
      TRUE ~ "pre_distribuicao"
    )) %>%
    group_by(n_processo, fase) %>% 
    summarise(tempo_fase = as.numeric(sum(tempo_total, na.rm = TRUE))) %>% 
    spread(fase, tempo_fase, fill = 0)
  
    if(trib == "TJDFT"){
      tempos_por_fase <- tempos_por_fase %>% 
        mutate(sentenca = 0)
    }
    
    tempos_por_fase <- tempos_por_fase %>% 
      mutate(tempo_total = aceite_da_denuncia + arquivamento + instrucao_probatoria + recursos + sentenca)

  consolidado <- tempos_por_fase %>% 
    inner_join(tempos_totais)
}

