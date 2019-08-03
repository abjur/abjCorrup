gera_base_final <- function(d, trib){
  informacoes <- d %>% 
    spread_infos() %>% 
    filter(tribunal == trib)
  
  tabela_movs <- d %>% 
    filter(n_processo %in% informacoes$n_processo) %>%  
    unnest(movs)
  
  tempos <- tabela_movs %>% 
    summarize_movs() %>% 
    spread_desfecho_processo(trib)
  
  desfechos <- tabela_movs %>% 
    classifica_desfecho(trib)
  
  tabela_dados <- inner_join(informacoes, tempos, by = 'n_processo') %>% 
    inner_join(desfechos, by = 'n_processo') %>% 
    mutate(denuncia_arquivada = denuncia_arquivada | 
             (arquivou & sentenca == 0 & numero_audiencias == 0 & numero_mandados == 0))

}
