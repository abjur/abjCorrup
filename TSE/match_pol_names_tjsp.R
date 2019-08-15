# This code downloads the SP' politician data from TSE and merge the 
# names from the defendants in the ABJ_corrup database.

# Cleaning the workspace
rm(list = ls())

# Setting the path
setwd("/home/leopoldo/Dropbox/UBC_files/2019_S/Summer_Paper/abjCorrup_local/")

# Loading the packages
library(tidyverse)
library(abjutils)
library(fuzzyjoin)
library(stringdist)

# Loading the elections data
tse_pol <- readRDS("data/tse_data_06_16_sp.rds")

# Loading the court proceeding data
abj_reus <- readRDS("data/d_parsed.rds")
abj_nupps <- readRDS("data/dnupps.rds")
#abj_assunto <- readRDS("data/bd_final_assunto.rds")
#rm(abj_assunto)

aux <- tibble(n_processo = abj_nupps$n_processo)
abj_cp <- inner_join(abj_reus, aux, by="n_processo")
abj_cp <- abj_cp %>% filter(tribunal=="TJSP")
rm(aux, abj_nupps, abj_reus)

saveRDS(abj_cp, file = "data/d_parsed_tjsp_nupps.rds")

#Stacking up the defendants by court proceeding code
aux_list <- list()
for (i in 1:nrow(abj_cp)) {
  aux <- abj_cp$partes[[i]]
  aux$n_processo <- abj_cp$n_processo[i]
  aux_list[[i]] <- aux
}

abj_cp_reus <- bind_rows(aux_list)

#Now let's clean the defendant df
cw <- as.data.frame(table(abj_cp_reus$parte), stringsAsFactors = FALSE)
colnames(cw)[1] <- "party_name"

#We will assume that the following codes are defendents
reu_names <- c("Acusado", "ApteRcrte", "AutCoatora", "AutoradoFato",
"AutordoFato", "AutoridadeCoat", "Averiguada", "Averiguado", "CoR", "CoRu",
"Denunciado", "DenunciadoTer", "Denuncido", "Exectda", "Indiciada", 
"Indiciado", "Infrator", "Infratora", "Imptdo", "Investigado", "LitisPas", 
"Noticiado", "Paciente", "R", "Recorrente", "Recorrido", "Reqda", 
"Reqdo", "Reqte", "Ru")

cw$reu <- 0
for (name in reu_names) {
    cw$reu <- ifelse(cw$party_name==name,1,cw$reu)
}

cw$Freq <- NULL
rm(name, reu_names)

#Adding the crosswalk (cw) to the original df
abj_cp_reus <- inner_join(abj_cp_reus, cw, by = c("parte"="party_name"))

colnames(abj_cp_reus)[ncol(abj_cp_reus)] <- "is_defendant"

abj_cp_reus2 <- abj_cp_reus %>% 
                filter(is_defendant==1) %>% 
                select(n_processo, nome) 

#Cleaning the ws
rm(abj_cp, abj_cp_reus, aux, aux_list, cw, i)

#Standardizing the names
abj_cp_reus3 <- abj_cp_reus2 %>%
  mutate(nome = str_replace_all(nome, "(?<= Ou | ou )(.*)", "") ) %>%
  mutate(nome = str_replace_all(nome, " Ou | ou ", "") ) %>%
  mutate(clean = tolower(nome)) %>%
  mutate(clean = gsub("[[:blank:]]", "", clean) ) %>% 
  mutate(clean = rm_accent(clean) ) %>% 
  mutate(clean = str_replace_all(clean, "ª", "a") ) %>%
  mutate(clean = str_replace_all(clean, "º", "o") ) %>%
  mutate(clean = str_replace_all(clean, "-|_|`|'|´", "") )


#Getting rid of weird cases
abj_cp_reus4 <- abj_cp_reus3 %>% 
  #Filtering weird cases
  filter( clean!="" & 
          clean!="jp" & 
          clean!="aaa" & 
          clean!="emssa" & 
          clean!="aapurar" & 
          clean!="xxxx" & 
          clean!="advaguardregrodrigomaiaribeiro" & 
          clean!="aesclarecer") %>% 
  #Cleaning some names
  mutate(clean = str_replace_all(clean,
"reupreso|reupresoporoutroprocesso|extintaapunibilidade|cienteem|", "") ) %>%
  mutate(clean = str_replace_all(clean,
"nocollyfashionfirmaativanomefantasiade", "") ) %>%
  mutate(clean = str_replace_all(clean, "absolvido|arquivadonafasedeinquerito", "") ) %>%
  #Getting rid of some parties that are not defendant
  mutate( notreu = str_detect(clean, "defpub|ministeriopublico|defensoria|corregedoria|promotoria|delegadodepolicia")) %>%
  mutate(ncharnome = nchar(clean)) %>%
  arrange(desc(ncharnome)) %>% 
  filter( ncharnome<47 & notreu==FALSE) %>% 
  mutate( notreu = str_detect(clean, 
"advogad|comercio|industria|engenharia|servico|tecnologia|centro|prefeitura|municipio|ltda|distrito|delegacia|justica|consultoria|participacoes")) %>%
  filter( notreu==FALSE) %>% 
  select(n_processo, nome, clean)

#Note that I am getting rid of names that appear more than one time at the database.
#I will fix this latter.

abj_cp_reus_wr <- abj_cp_reus4 %>% 
                  arrange(clean)

abj_cp_reus_nr <- abj_cp_reus4 %>% 
                  distinct(clean, .keep_all= TRUE) %>% 
                  arrange(clean)

rm(abj_cp_reus2, abj_cp_reus3, abj_cp_reus4)

#Now, let's look to the politician database
#Standardizing the names
tse_pol_wr <- tse_pol %>%
                select(nome = NOME_CANDIDATO, cargo = DESCRICAO_CARGO) %>% 
                mutate(clean = tolower(nome)) %>%
                mutate(clean = gsub("[[:blank:]]", "", clean) ) %>%
                mutate(clean = str_replace_all(clean, "ª", "a") ) %>%
                mutate(clean = str_replace_all(clean, "º", "o") ) %>%
                mutate(clean = rm_accent(clean) ) %>% 
                mutate(clean = str_replace_all(clean, "-|_|`|'|´", "") ) %>% 
                arrange(clean)

tse_pol_nr <- tse_pol_wr %>%
                distinct(clean, .keep_all= TRUE) %>%
                arrange(clean)

rm(tse_pol)                

# Note that we are throwing candidates who were candidate for more than one position.
# We will fix this after the matching.

# Matching the names.
# First, let's matchthe exactly, then we will try a fuzzy join
names_joined <- left_join(abj_cp_reus_nr , tse_pol_nr, by="clean")
names_joined_suc <- names_joined %>% 
  filter(!is.na(nome.y)) %>% 
  select(n_processo=n_processo, nome=nome.x, clean = clean)

names_joined_rst <- names_joined %>% 
  filter(is.na(nome.y)) %>% 
  select(n_processo=n_processo, nome=nome.x, clean = clean)

aux <- left_join(tse_pol_nr, abj_cp_reus_nr, by="clean")
tse_pol_rst <- aux %>% 
  filter(is.na(nome.y)) %>% 
  select(nome=nome.x, cargo=cargo, clean = clean)
rm(aux)

#Doing fuzzy matching with the rest
#I will separate in several batches because I do not have RAM memory in order to do it at once
#I am assuming that the names in the TSE data base are the correct ones, since they have
#acess to the politician CPF and RG
aux_list = list()

aux_list[[1]] <- stringdist_inner_join(tse_pol_rst[1:30000,], names_joined_rst, by="clean" , 
                                       method = "lv", max_dist = 2, distance_col = "dist")
aux_list[[2]] <- stringdist_inner_join(tse_pol_rst[30001:60000,], names_joined_rst, by="clean" , 
                                       method = "lv", max_dist = 2, distance_col = "dist")
aux_list[[3]] <- stringdist_inner_join(tse_pol_rst[60001:90000,], names_joined_rst, by="clean" , 
                                       method = "lv", max_dist = 2, distance_col = "dist")
aux_list[[4]] <- stringdist_inner_join(tse_pol_rst[90001:120000,], names_joined_rst, by="clean" , 
                                       method = "lv", max_dist = 2, distance_col = "dist")
aux_list[[5]] <- stringdist_inner_join(tse_pol_rst[120001:150000,], names_joined_rst, by="clean" , 
                                       method = "lv", max_dist = 2, distance_col = "dist")
aux_list[[6]] <- stringdist_inner_join(tse_pol_rst[150001:nrow(tse_pol_rst),], names_joined_rst, by="clean" , 
                                       method = "lv", max_dist = 2, distance_col = "dist")

names_joined_fuz <- bind_rows(aux_list)

#Now, let's join both data sets identifying the origin of the match
names_joined_fuz$fuz <- 1

names_joined_nfuz <- inner_join(tse_pol_nr, abj_cp_reus_nr, by="clean")
colnames(names_joined_nfuz)[3] <- "clean.x"
names_joined_nfuz$clean.y <- names_joined_nfuz$clean.x
names_joined_nfuz$dist <- 0
names_joined_nfuz$fuz <- 0

names_joined_final <- bind_rows(names_joined_nfuz, names_joined_fuz)

rm(aux_list, names_joined, names_joined_fuz, names_joined_nfuz, 
   names_joined_rst, names_joined_suc, tse_pol_rst)

# #Saving up to this point
# save(abj_cp_reus_nr, abj_cp_reus_wr, names_joined_final, tse_pol_nr, tse_pol_wr, file = "tempdata.rda")

#Identify if there is duplicates after the matching
#In politicians
aux <- names_joined_final %>% 
  group_by(clean.x) %>%
  mutate(rcount = n()) %>% 
  ungroup()

#I will throw bad matches here to eliminate the duplicates
aux2 <- aux %>%
  filter(rcount==1 | (rcount>1 & (clean.y == "flaviolopesdasilva"))) %>% 
  select(-rcount)

names_joined_final <- aux2
rm(aux, aux2)

#In parties names
aux <- names_joined_final %>% 
  group_by(clean.y) %>%
  mutate(ismin = if_else(min(dist)==dist,1,0)) %>% 
  ungroup() %>%
  filter(ismin==1) %>%
  group_by(clean.y) %>% 
  mutate(rcount = n()) %>%
  ungroup() %>%
  arrange(clean.y)

aux2 <- aux %>%
  mutate(name.xf = word(nome.x), 
         name.yf = word(nome.y)) %>% 
  mutate(name.xl = word(nome.x, start = -1, end = -1), 
         name.yl = word(nome.y, start = -1, end = -1))

aux2$name_df <- stringdist(tolower(aux2$name.xf), tolower(aux2$name.yf), method="lv")
aux2$name_dl <- stringdist(tolower(aux2$name.xl), tolower(aux2$name.yl), method="lv")

#Let's separate the first word of the name
aux3 <- aux2 %>% 
  group_by(clean.y) %>%
  mutate(ismin = if_else(min(name_df)==name_df,1,0)) %>% 
  ungroup() %>%
  filter(ismin==1) %>%
  group_by(clean.y) %>% 
  mutate(rcount = n()) %>%
  ungroup() %>%
  arrange(clean.y)

#Let's separate the last word of the name
aux4 <- aux3 %>% 
  group_by(clean.y) %>%
  mutate(ismin = if_else(min(name_dl)==name_dl,1,0)) %>% 
  ungroup() %>%
  filter(ismin==1) %>%
  group_by(clean.y) %>% 
  mutate(rcount = n()) %>%
  ungroup() %>%
  arrange(clean.y)

#Let's separate the first letter of the first word of the name
aux5 <- aux4 %>%
  mutate(firstletter = (!(str_sub(name.xf,1,1) == str_sub(name.yf,1,1)))*1) %>% 
  group_by(clean.y) %>%
  mutate(ismin = if_else(min(firstletter)==firstletter,1,0)) %>% 
  ungroup() %>%
  filter(ismin==1) %>%
  group_by(clean.y) %>% 
  mutate(rcount = n()) %>%
  ungroup() %>%
  arrange(clean.y)

# For the last 31 names I dropped the bad matches
aux6 <- aux5 %>%
  filter(rcount==1 | (rcount>1 & (clean.x == "adilsonmartins" | 
                                    clean.x == "ademilsonrodriguesdeoliveira" | 
                                    clean.x == "edivardasilva" | 
                                    clean.x == "geraldomenin" |
                                    clean.x == "marcospaulinodasilva" |
                                    clean.x == "ademilsonrodriguesdeoliveira" |
                                    clean.x == "wilsondonascimento" |
                                    clean.x == "veraaparecidadeoliveira")  )) %>%
  group_by(clean.y) %>% 
  mutate(rcount = n()) %>%
  ungroup() %>%
  arrange(clean.y)

aux7 <- aux6 %>%
  filter(rcount>1) 

names_joined_final <- aux6 %>% 
  select(nome.x, cargo, clean.x, n_processo, nome.y, clean.y, fuz)

rm(aux)
rm(list = c(paste("aux",c(2:7),sep="")))

#Now, for each politician selected, I need to check if they were candidate to
#more than one position

aux <- names_joined_final %>% 
  select(clean.x, clean.y)

tse_pol_merg <- inner_join(tse_pol_wr, aux, by=c("clean"="clean.x"))

#Removing the duplicates
aux <- tse_pol_merg %>% 
  distinct(clean, cargo, .keep_all= TRUE) %>% 
  arrange(clean, cargo)

aux2 <- aux %>% 
  #group_by(nome, clean, clean.y) %>%
  group_by(clean) %>% 
  summarize(nome2 = first(nome), 
            clean.y2 = first(clean.y), 
            cargo2 = paste(cargo, collapse="|"))

aux3 <- aux2 %>% select(clean, cargo2)

names_joined_final2 <- left_join(names_joined_final, aux3, by=c("clean.x"="clean"))

rm(aux, aux2, aux3)

#Now, let's merge the politician info to the original court proceeding df
abj_cp_reus_final <- left_join(abj_cp_reus_wr, names_joined_final2, by=c("clean"="clean.y"))

abj_cp_reus_final2 = tibble(n_processo = abj_cp_reus_final$n_processo.x,
                                nome_jstc = abj_cp_reus_final$nome,
                                clean_jstc = abj_cp_reus_final$clean,
                                nome_tse = abj_cp_reus_final$nome.x,
                                clean_tse = abj_cp_reus_final$clean.x,
                                cargo = abj_cp_reus_final$cargo2,
                                fuzz_join = abj_cp_reus_final$fuz)

rm(abj_cp_reus_nr, abj_cp_reus_wr, abj_cp_reus_final)
rm(names_joined_final, tse_pol_merg, tse_pol_nr)

#Saving the results
saveRDS(abj_cp_reus_final2, file = "data/abj_reus_pol_1a_tjsp.rds")
