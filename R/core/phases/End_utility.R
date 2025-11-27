reset_variabili_turno <- function(gioco) {
  gioco$magie_lanciate_giocatore_attivo_turno <- 0
  gioco$terre_giocate_questo_turno <- 0
  gioco$trigger_inizio_turno <- list()
  return(gioco)
}

passa_al_prossimo_giocatore <- function(gioco) {
  
  # gestisci turni extra
  if (gioco$turni_extra > 0) {
    gioco$turni_extra <- gioco$turni_extra - 1
    return(gioco)  # stesso giocatore continua
  }
  
  numero <- length(gioco$giocatori)
  nuovo <- (gioco$giocatore_attivo_index %% numero) + 1
  
  gioco$giocatore_attivo_index <- nuovo
  
  mess <- paste("Il turno passa a", gioco$giocatori[[nuovo]]$nome)
  gioco <- aggiungi_log(gioco, mess)
  
  return(gioco)
}
