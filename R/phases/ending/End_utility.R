# Resetta le variabili di turno del giocatore attivo
reset_variabili_turno <- function(gioco) {
  idx <- gioco$indice_giocatore_attivo
  
  # terre giocate dal giocatore attivo in questo turno
  if (!is.null(gioco$giocatori[[idx]]$terre_giocate_turno)) {
    gioco$giocatori[[idx]]$terre_giocate_turno <- 0L
  }
  
  # contatore magie lanciate da ciascun giocatore nel turno corrente
  if (!is.null(gioco$magie_lanciate_turno_corrente)) {
    gioco$magie_lanciate_turno_corrente[idx] <- 0L
  }
  
  # eventuali trigger schedulati per "inizio turno"
  gioco$trigger_inizio_turno <- list()
  
  return(gioco)
}

# Passa il turno al prossimo giocatore
passa_al_prossimo_giocatore <- function(gioco) {
  n <- length(gioco$giocatori)
  if (n == 0L) return(gioco)
  
  gioco$indice_giocatore_attivo <- (gioco$indice_giocatore_attivo %% n) + 1L
  gioco$turno_corrente <- gioco$turno_corrente + 1L
  
  mess <- paste("Il turno passa a", gioco$giocatori[[gioco$indice_giocatore_attivo]]$nome)
  gioco <- aggiungi_log(gioco, mess, tipo = "turno")
  
  return(gioco)
}
