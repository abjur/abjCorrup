# This code downloads the SP' politician data from TSE and merge the 
# names from the defendants in the ABJ_corrup database.

# Cleaning the workspace
rm(list = ls())

# Setting the path
setwd("/home/leopoldo/Dropbox/UBC_files/2019_S/Summer_Paper/abjCorrup_local/")

# Loading the packages
library(tidyverse)
library(abjutils)
library(esaj)

# Loading the elections data2
abj_reus_pol <- readRDS("data/abj_reus_pol_1a_tjsp.rds")
dnupps <- readRDS("data/dnupps.rds")
d_parsed <- readRDS("data/d_parsed_tjsp_nupps.rds")

# Making the table of the report
aux <- abj_reus_pol$cargo
aux[is.na(abj_reus_pol$cargo)] = "NÃO POLÍTICO"
tab <- tabela(aux,label = "Cargo")

# Let's check the court proceeding without a defendant
dnupps <- dnupps %>% 
  filter(tribunal == "TJSP") %>% 
  arrange(n_processo)

# Note that we have several court proceeding without the name of the defendant
aux <- abj_reus_pol %>%
  distinct(n_processo) %>%
  arrange(n_processo) %>% 
  mutate(existdef = 1)

aux2 <- full_join(d_parsed, aux, by="n_processo") 
aux2$existdef[is.na(aux2$existdef)] = 0
tab2 <- tabela(aux2$existdef,label = "Processo tem réu?")

# Filtering the processes without defendant
nodef <- aux2 %>% 
  filter(existdef==0)

id_ndf <- tibble(id_clean = nodef$n_processo, id = build_id(nodef$n_processo))

#Testing some stuff
download_cpopg(id_ndf[1,1], "data/")

download_decision("10000034", "data/")
download_decision("10000034", "~/Desktop/")