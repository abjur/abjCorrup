assuntos_filtrar <- function() {
  c(
    "Advocacia administrativa", 
    "Concussão", 
    "Excesso de exação", 
    "Extravio,? sonegação", 
    "Inserção de dados falsos", 
    "corrupção",
    "Lavagem ou Ocultação de Bens", 
    "não autorizada", 
    "Peculato", 
    "Contra a Administração em Geral", 
    "Tráfico de influência"
  ) %>% 
    tibble::enframe() %>% 
    dplyr::mutate(value = value %>% 
                    stringr::str_to_upper() %>% 
                    abjutils::rm_accent())
}

crimes <- function() {
  
  tibble::tribble(
    ~crime, ~label,
    "advocacia administrativa",    "Advocacia administrativa",
    "concussao",    "Concussão",
    "excesso de exacao",    "Concussão",
    "insercao de dados falsos",    "Inserção de dados falsos",
    "corrupcao passiva",    "Corrupção passiva",
    "corrupcao ativa",    "Corrupção ativa",
    "lavagem(quot)? ou ocultacao de bens",    "Lavagem de dinheiro",
    "peculato",    "Peculato",
    "(?<!particular )contra a administracao em geral", "Contra a Administração em Geral",
    "trafico de influencia",    "Tráfico de influência",
  ) %>% 
    mutate(re = purrr::map(crime, regex, ignore_case = TRUE))
  
}

regex_assuntos <- function(){
  c(
    "Advocacia administrativa", 
    "Concuss[aã]o", 
    "Excesso de exa[cç][aã]o", 
    "Extravio,? sonegação", 
    "Inserção de dados falsos", 
    "corrup[cç][aã]o",
    "Lavagem ou Oculta[cç][aã]o de Bens", 
    "não autorizada", 
    "Peculato", 
    "Contra a Administra[çc][aã]o em Geral", 
    "Tr[aá]fico de influ[eê]ncia"
  ) %>% 
    paste(collapse = "|") %>% 
    regex(ignore_case = TRUE)
}
