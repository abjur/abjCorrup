calcula_tempo <- function(datas, abre, fecha, pegar){
  
  datas_validas <- datas[pegar]
  fecha_pegar <- fecha[pegar]
  abre_pegar <- abre[pegar]
  
  fechas_validos <- datas_validas[fecha_pegar & lag(abre_pegar)]
  abres_validos <- datas_validas[abre_pegar & lead(fecha_pegar)]
  
  fechas_validos <- fechas_validos[!is.na(fechas_validos)]
  abres_validos <- abres_validos[!is.na(abres_validos)]
  
  total <- sum(fechas_validos - abres_validos, na.rm = TRUE)
}

procura_marco <- function(vetor_id, tipo_mov, classe, tipo = "min"){
  if(tipo == "min"){
    aonde <- min(which(tipo_mov %in% classe))
  } else {
    aonde <- max(which(tipo_mov %in% classe))
  }
  
  if(length(aonde) == 0){
    rep(FALSE, length(vetor_id))
  } else {
    vetor_id == aonde
  }
}