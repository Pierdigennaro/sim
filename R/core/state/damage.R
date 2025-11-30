applica_danno_a_giocatore <- function(gioco, player_index, amount) {
  amount <- as.integer(amount)
  if (is.na(amount) || amount <= 0L) return(gioco)
  
  g <- gioco$giocatori[[player_index]]
  vecchi_pv <- g$punti_vita %||% 0L
  g$punti_vita <- vecchi_pv - amount
  gioco$giocatori[[player_index]] <- g
  
  gioco <- aggiungi_log(
    gioco,
    paste0(
      g$nome, " subisce ", amount, " danni (",
      vecchi_pv, " → ", g$punti_vita, ")."
    )
  )
  
  return(gioco)
}

applica_danno_a_creatura <- function(gioco, loc, amount) {
  amount <- as.integer(amount)
  if (is.na(amount) || amount <= 0L) return(gioco)
  if (loc$zone != "campo") return(gioco)
  
  campo <- get_zona(gioco, "campo")
  carta <- campo[[loc$index]]
  
  danno_vecchio <- carta$danno %||% 0L
  carta$danno   <- danno_vecchio + amount
  
  campo[[loc$index]] <- carta
  gioco <- set_zona(gioco, "campo", campo)
  
  gioco <- aggiungi_log(
    gioco,
    paste0(
      carta$nome, " subisce ", amount, " danni (danno totale ora: ",
      carta$danno, ")."
    )
  )
  
  return(gioco)
}
