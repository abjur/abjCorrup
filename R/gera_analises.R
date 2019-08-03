# Caracterização da base -------------------------------------------------------
tabela_contagem <- function(d, variavel = "classe", trib, n = NULL) {
  v <- sym(variavel)
  d_filter <- filter(d, tribunal %in% trib) %>% 
    replace_na(setNames(list("Vazio"), variavel)) %>% 
    mutate(v = str_extract(!!v, "[^(]+"))
  
  if(!is.null(n)) {
    d_lump <- d_filter %>% 
      mutate(v = fct_lump(fct_infreq(v), n, other_level = "Outros"))
  } else {
    d_lump <- d_filter %>% 
      mutate(v = fct_lump(fct_infreq(v), prop = 5/nrow(d_filter), 
                          other_level = "Outros"))
  }
  d_lump %>% 
    count(v) %>% 
    arrange(v) %>% 
    mutate(prop = n/sum(n)) %>% 
    janitor::adorn_totals()
}

# tabela_contagem("assunto")


# Análise tempos fases ---------------------------------------------------------

grafico_tempo_ator <- function(d, trib, resp = "prop") {
  
  resp <- sym(resp)
  y_lab <- if_else(resp == "prop", 
                   "Proporção do tempo total do processo",
                   "Tempo médio dos processos por ator")
  
  p <- d %>% 
    filter(tribunal == trib) %>% 
    select(n_processo, desfecho, tempo_juiz, tempo_reu, tempo_mp) %>% 
    mutate_if(lubridate::is.difftime, ~as.numeric(.x, units = "days")) %>% 
    gather(tipo_tempo, tempo, -n_processo, -desfecho) %>% 
    group_by(desfecho,  tipo_tempo) %>% 
    summarise(tempo = mean(tempo), s_tempo = sum(tempo)) %>% 
    mutate(prop = s_tempo / sum(s_tempo)) %>% 
    ungroup() %>% 
    mutate(tipo_tempo = case_when(
      tipo_tempo == "tempo_juiz" ~ "Juiz",
      tipo_tempo == "tempo_reu" ~ "Réu",
      tipo_tempo == "tempo_mp" ~ "Ministério Público"
    )) %>% 
    mutate(
      morreu = fct_rev(if_else(desfecho == "Ativo", "Ativo", "Encerrado")),
      desfecho = fct_reorder(desfecho, as.numeric(morreu) * tempo, sum)
    ) %>% 
    ggplot(aes(x = desfecho, y = !!resp, fill = tipo_tempo,
               linetype = morreu)) +
    geom_col(size = .5, colour = "black") + 
    scale_fill_viridis_d(begin = 0.3, end = 0.8) +
    coord_flip() +
    theme_minimal(14) +
    labs(x = "Desfecho", y = y_lab, 
         fill = "Ator", linetype = "Status")
  
  if (resp == "prop") {
    p + scale_y_continuous(labels = scales::percent)
  } else {
    p
  }
}

grafico_tempo_fase <- function(d, trib, resp = "prop") {
  
  resp <- sym(resp)
  y_lab <- if_else(resp == "prop", 
                   "Proporção do tempo total do processo",
                   "Tempo médio dos processos na fase")
  
  p <- d %>% 
    filter(tribunal == trib) %>% 
    select(n_processo, desfecho, starts_with("fase_")) %>% 
    gather(fase, tempo, -n_processo, -desfecho) %>% 
    group_by(desfecho,  fase) %>% 
    summarise(tempo = mean(tempo)) %>% 
    mutate(prop = tempo / sum(tempo)) %>% 
    ungroup() %>% 
    mutate(fase = case_when(
      fase == "fase_inquerito" ~ "Inquérito Policial",
      fase == "fase_instrucao" ~ "Instrução",
      fase == "fase_pre" ~ "Antes da distribuição judicial",
      fase == "fase_recursal" ~ "Recursal",
      fase == "fase_sentenca" ~ "Sentença",
    )) %>% 
    mutate(fase = fct_rev(lvls_reorder(fase, c(1, 2, 3, 5, 4)))) %>%     
    mutate(
      morreu = fct_rev(if_else(desfecho == "Ativo", "Ativo", "Encerrado")),
      desfecho = fct_reorder(desfecho, as.numeric(morreu) * tempo, sum)
    ) %>%     
    ggplot(aes(x = desfecho, y = !!resp, fill = fase,
               linetype = morreu)) +
    geom_col(size = .5, colour = "black") + 
    scale_fill_viridis_d(begin = 0.1, end = 0.9) +
    coord_flip() +
    theme_minimal(14) +
    labs(x = "Desfecho", y = y_lab, 
         fill = "Fase do processo", 
         linetype = "Status")
  
  if (resp == "prop") {
    p + scale_y_continuous(labels = scales::percent)
  } else {
    p
  }
}


# regressao


modelagem <- function() {
  d_nupps_reg <- d_nupps %>% 
    filter(desfecho != "Ativo", tribunal != "JFAL") %>% 
    mutate(ano_dist = as.factor(case_when(
      lubridate::year(dt_dist) <= 2000 ~ "(1990-2000]",
      lubridate::year(dt_dist) <= 2010 ~ "(2000-2010]",
      lubridate::year(dt_dist) <= 2020 ~ "(2010-2018]",
      TRUE ~ "Vazio"
    ))) %>% 
    mutate(crime = fct_infreq(crime)) %>% 
    mutate(prescreveu = desfecho == "Prescrição") %>% 
    select(
      prescreveu, tribunal, crime, ano_dist,
      starts_with("fase"),
      tempo_reu, tempo_mp, tempo_juiz
    ) %>% 
    mutate_at(vars(matches("^tempo_|^fase_")), 
              funs(as.numeric(., units = "days") / 365))
  
  modelo <- glm2::glm2(prescreveu ~ ., family = binomial(), data = d_nupps_reg)
  modelo_lm <- lm(prescreveu ~ ., data = d_nupps_reg)
  
  intervalos <- confint(modelo) %>% 
    as.data.frame() %>% 
    rownames_to_column("term") %>% 
    as_tibble() %>% 
    janitor::clean_names()
  
  sig <- identity
  
  modelo %>% 
    broom::tidy() %>% 
    filter(term != "(Intercept)") %>% 
    inner_join(intervalos, "term") %>% 
    filter(!is.na(x2_5_percent), !is.na(x97_5_percent), x2_5_percent > -10) %>% 
    mutate(signif = if_else(x2_5_percent > 0 | x97_5_percent < 0,
                            "Significante", "Não significante")) %>% 
    mutate(term = fct_reorder(term, estimate, .desc = TRUE)) %>% 
    ggplot(aes(x = sig(estimate), 
               y = term, colour = signif)) +
    geom_vline(aes(xintercept = 0), linetype = 2, colour = "gray70") +
    geom_point() +
    scale_colour_manual(values = c("black", "red")) +
    geom_errorbarh(aes(xmin = sig(x2_5_percent), xmax = sig(x97_5_percent))) +
    theme_minimal(14) +
    labs(x = "Estimativa (log-Razão de chances)", 
         y = "Variável", colour = "Resultado") +
    theme(legend.position = "bottom")
  
}

modelagem_prop <- function(dd) {
  d_nupps_reg <- dd %>% 
    filter(desfecho != "Ativo") %>% 
    mutate(ano_dist = as.factor(case_when(
      lubridate::year(dt_dist) <= 2000 ~ "(1990-2000]",
      lubridate::year(dt_dist) <= 2010 ~ "(2000-2010]",
      lubridate::year(dt_dist) <= 2020 ~ "(2010-2018]",
      TRUE ~ "Vazio"
    ))) %>% 
    mutate(crime = fct_infreq(crime)) %>% 
    mutate(prescreveu = desfecho == "Prescrição") %>% 
    mutate(peculato = crime == "Peculato") %>% 
    select(
      prescreveu, peculato,
      starts_with("fase"), fatores, tempo_juiz
    ) %>% 
    mutate(tempo_total = fase_pre + fase_inquerito +
             fase_instrucao + fase_sentenca + fase_recursal) %>% 
    mutate_at(vars(matches("^tempo_|^fase_")), 
              funs(as.numeric(., units = "days") / tempo_total)) %>% 
    select(prescreveu, peculato, fase_inquerito, fatores)
  
  modelo <- glm(prescreveu ~ ., family = binomial(), data = d_nupps_reg)
  modelo_lm <- lm(prescreveu ~ ., data = d_nupps_reg)
  
  intervalos <- confint(modelo, level = .95) %>% 
    as.data.frame() %>% 
    rownames_to_column("term") %>% 
    as_tibble() %>% 
    janitor::clean_names()
  
  sig <- identity
  
  modelo %>% 
    broom::tidy() %>% 
    filter(term != "fase_pre") %>% 
    filter(term != "(Intercept)") %>% 
    inner_join(intervalos, "term") %>% 
    filter(!is.na(x2_5_percent), !is.na(x97_5_percent), x2_5_percent > -10) %>% 
    mutate(signif = if_else(x2_5_percent > 0 | x97_5_percent < 0,
                            "Significante", "Não significante")) %>% 
    mutate(term = fct_reorder(term, estimate, .desc = TRUE)) %>% 
    ggplot(aes(x = sig(estimate), 
               y = term, colour = signif)) +
    geom_vline(aes(xintercept = 0), linetype = 2, colour = "gray70") +
    geom_point() +
    scale_colour_manual(values = c("black", "red")) +
    geom_errorbarh(aes(xmin = sig(x2_5_percent), xmax = sig(x97_5_percent))) +
    theme_minimal(14) +
    labs(x = "Estimativa (log-Razão de chances)", 
         y = "Variável", colour = "Resultado") +
    theme(legend.position = "bottom")
}


# Análise desfechos ------------------------------------------------------------

grafico_desfecho <- function(d, trib = "TJSP", descartar_ativos = TRUE) {
  d %>% 
    filter(!descartar_ativos | (desfecho != "Ativo"),
           tribunal == trib) %>% 
    count(desfecho) %>% 
    mutate(prop = n/sum(n)) %>% 
    mutate(desfecho = fct_reorder(desfecho, prop)) %>% 
    ggplot(aes(x = desfecho, y = prop)) +
    geom_col(colour = "black", fill = "royalblue", 
             alpha = 0.7, size = 0.4, width = 0.6) +
    scale_y_continuous(labels = scales::percent) +
    coord_flip() +
    theme_minimal(14) +
    labs(x = "Desfecho", y = "Proporção")
}

grafico_prescreveu_crime <- function(d, trib = "TJSP", descartar_ativos = TRUE) {
  d_plot <- d %>% 
    filter(!descartar_ativos | (desfecho != "Ativo"),
           tribunal == trib) %>% 
    mutate(prescreveu = if_else(desfecho == "Prescrição", "Sim", "Não")) %>% 
    count(crime, prescreveu) %>% 
    group_by(crime) %>% 
    mutate(tot = sum(n), prop = n/tot) %>% 
    ungroup() %>% 
    filter(prescreveu == "Sim") %>% 
    arrange(crime, desc(prop)) %>% 
    mutate(crime = fct_reorder2(str_wrap(crime, 22), prescreveu, 
                                prop, last2, .desc = FALSE))
  d_plot %>% 
    ggplot(aes(x = crime, y = prop, fill = prescreveu)) +
    geom_col(colour = "black", fill = "royalblue", 
             alpha = 0.7, size = 0.4, width = 0.6) +
    geom_text(aes(label = as.character(tot), y = -.01, hjust = 1)) +
    scale_y_continuous(labels = scales::percent, limits = c(-0.01, max(d_plot$prop))) +
    coord_flip() +
    theme_minimal(14) +
    labs(x = "Crime", 
         y = "Proporção de processos prescritos",
         fill = "Prescreveu")
}


# g <- tribunais %>% 
#   magrittr::extract(.!="TJRJ") %>% 
#   purrr::map(~grafico_prescreveu_crime(d_nupps, .x))
# 
# purrr::reduce(g, ggplot2:::`+.gg`)
# 
# 
# library(patchwork)
# f <- function(x, y) qplot(x, y)
# gg_list <- with(mtcars, list(f(mpg, cyl), f(mpg, disp), f(mpg, disp)))


gerar_analises <- function(trib) {
  
  tribunal <- trib
  
  contagem_classe <- tabela_contagem(d_nupps, "classe", trib, 100)
  tabela_classe <- tabela_contagem(d_nupps, "classe", trib) %>% 
    mutate(prop = scales::percent(prop)) %>% 
    set_names(c("Classe", "Freq.", "Prop (%)"))
  
  contagem_crime <- tabela_contagem(d_nupps, "crime", trib, 100)
  tabela_crime <- tabela_contagem(d_nupps, "crime", trib, 100) %>% 
    mutate(prop = scales::percent(prop)) %>% 
    set_names(c("Crime", "Freq.", "Prop (%)"))
  
  contagem_desfecho <- tabela_contagem(d_nupps, "desfecho", trib, 100)
  tabela_desfecho <- tabela_contagem(d_nupps, "desfecho", trib, 100) %>% 
    mutate(prop = scales::percent(prop)) %>% 
    set_names(c("Crime", "Freq.", "Prop (%)"))
  
  g_desfecho_encerrados <- grafico_desfecho(d_nupps, trib, TRUE)
  g_desfecho_todos <- grafico_desfecho(d_nupps, trib, FALSE)
  g_prescricao_encerrados <- grafico_prescreveu_crime(d_nupps, trib, TRUE)
  g_prescricao_todos <- grafico_prescreveu_crime(d_nupps, trib, FALSE)
  g_tempo_fase_tot <- grafico_tempo_fase(d_nupps, trib, "tempo")
  g_tempo_fase_prop <- grafico_tempo_fase(d_nupps, trib, "prop")
  g_tempo_ator_tot <- grafico_tempo_ator(d_nupps, trib, "tempo")
  g_tempo_ator_prop <- grafico_tempo_ator(d_nupps, trib, "prop")
  
  g_tempo_ator <- (g_tempo_ator_tot + guides(fill = FALSE, linetype = FALSE)) +
    (g_tempo_ator_prop + theme(legend.position = "bottom")) +
    plot_layout(ncol = 1)
  
  
  labels <- c(
    stringr::str_glue("Classes mais comuns dos processos no {tribunal}."),
    stringr::str_glue("Crimes mais comuns dos processos no {tribunal}."),
    stringr::str_glue("Frequências dos desfechos no {tribunal}."),
    stringr::str_glue("Gráfico da proporção de cada desfecho nos processos do {tribunal}, considerando somente os processos encerrados."),
    stringr::str_glue("Gráfico da proporção de prescrição por tipo de crime no {tribunal}, considerando todos os processos."),
    stringr::str_glue("Gráfico da proporção de prescrição por tipo de crime no {tribunal}, considerando apenas os processos encerrados."),
    stringr::str_glue("Gráfico dos tempos médios de cada fase nos processos do {tribunal}, separados por tipo de desfecho."),
    stringr::str_glue("Gráfico da proporção do tempo de cada fase nos processsos do {tribunal}, separado por tipo de desfecho."),
    stringr::str_glue("Gráfico dos tempos médios e proporção de cada ator nos processsos do {tribunal}, separados por tipo de desfecho.")
  )

  analises <- tibble::tribble(
    ~id, ~label, ~dados, ~vis,
    1, labels[1], contagem_classe,              tabela_classe,
    2, labels[2], contagem_crime,               tabela_crime,
    3, labels[3], contagem_desfecho,            tabela_desfecho,
    4, labels[4], g_desfecho_encerrados$data,   g_desfecho_encerrados,
    5, labels[5], g_prescricao_todos$data,      g_prescricao_todos,
    6, labels[6], g_prescricao_encerrados$data, g_prescricao_encerrados,
    7, labels[7], g_tempo_fase_tot$data,        g_tempo_fase_tot,
    8, labels[8], g_tempo_fase_prop$data,       g_tempo_fase_prop,
    9, labels[9], g_tempo_ator$data,            g_tempo_ator
  )
  analises
   
}




