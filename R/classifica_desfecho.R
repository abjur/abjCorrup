classifica_desfecho <- function(tabela_movs, trib) {
  
  if (trib == "JFDF") {
    tabela_movs %>% 
      mutate(descricao = titulo,
             descricao_e_titulo = paste0(titulo, texto),
             descricao = str_to_lower(descricao) %>% 
               abjutils::rm_accent()) %>% 
      group_by(n_processo) %>%
      summarise(
        prescricao = any(str_detect(descricao, "prescricao")),
        absolvido = any(str_detect(descricao, "absolutoria|improcedente")),
        condenacao = any(str_detect(descricao, "condenatoria|parcialmente procedente|procedente")),
        morte_do_agente = any(str_detect(descricao, "extincao da punibilidade morte")),
        arquivou = any(str_detect(descricao, "^baixa|remessa ordenada arquivo|baixa arquivados|arquivamento|baixa entregu")),
        denuncia_arquivada = any(str_detect(descricao, "arquivamento inqu"))
      ) %>% 
      mutate(
        sem_punicao = (prescricao | morte_do_agente) & !absolvido & !condenacao,
        em_curso = !prescricao & !absolvido & !condenacao & !morte_do_agente & !arquivou)
  } else {
    tabela_movs %>% 
      mutate(descricao = titulo,
             descricao_e_titulo = paste0(titulo, texto),
             descricao = str_to_lower(descricao) %>% 
               abjutils::rm_accent()) %>% 
      group_by(n_processo) %>%
      summarise(
        prescricao = any(str_detect(descricao, "prescricao")),
        absolvido = any(str_detect(descricao, "absolutoria|improcedente")),
        condenacao = any(str_detect(descricao, "condenatoria|parcialmente procedente|procedente")),
        morte_do_agente = any(str_detect(descricao, "extincao da punibilidade morte")),
        arquivou = any(str_detect(descricao, "arquivamento")),
        denuncia_arquivada = FALSE
      ) %>% 
      mutate(
        sem_punicao = (prescricao | morte_do_agente) & !absolvido & !condenacao,
        em_curso = !prescricao & !absolvido & !condenacao & !morte_do_agente & !arquivou)
  }
  
}
