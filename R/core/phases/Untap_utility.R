phase_in_giocatoreattivo <- function(gioco) {
  temp <- F
  # Cicla su tutte le carte nel campo globale
  for (i in seq_along(gioco$campo)) {
    carta <- gioco$campo[[i]]
    # Se la carta è controllata dal giocatore e tappata → stappala
    if (carta$controllore == gioco$get_giocatore_attivo()$nome && isTRUE(carta$phase_out)) {
      temp <- T
      gioco$campo[[i]]$phase_out <- FALSE
    }
  }
  # Sincronizza i campi dei giocatori
  gioco <- aggiorna_campi_giocatori(gioco)
  # Aggiungi un messaggio al log
  if(isTRUE(temp)){
    messaggio <- paste0(gioco$get_giocatore_attivo()$nome, " phase-in tutte le sue carte.")
    gioco$log <- append(gioco$log, messaggio)}
  return(gioco)
}

aggiorna_ciclo_giorno_notte <- function(gioco) {
  
  # Se non c'è un ciclo attivo, non succede nulla
  if (gioco$ciclo_giorno_notte == "nessuno") {
    return(gioco)
  }
  
  magie <- gioco$magie_lanciate_giocatore_attivo_turno_precedente
  
  # Giorno → Notte
  if (gioco$ciclo_giorno_notte == "giorno" && magie == 0) {
    gioco$ciclo_giorno_notte <- "notte"
    gioco <- aggiungi_log(gioco, "È diventata notte (nessuna magia lanciata dal giocatore precedente).")
  }
  # Notte → Giorno
  else if (gioco$ciclo_giorno_notte == "notte" && magie >= 2) {
    gioco$ciclo_giorno_notte <- "giorno"
    gioco <- aggiungi_log(gioco, "È diventato giorno (due o più magie lanciate dal giocatore precedente).")
  }
  
  return(gioco)
}

stappa_giocatoreattivo <- function(gioco) {
  # Cicla su tutte le carte nel campo globale
  for (i in seq_along(gioco$campo)) {
    carta <- gioco$campo[[i]]
    # Se la carta è controllata dal giocatore e tappata → stappala
    lapply(campo, function(carta) {
      if (carta$controllore == gioco$get_giocatore_attivo()$nome && isTRUE(carta$tappato) && isTRUE(carta$puo_stappare) && !isTRUE(carta$phase_out)) {
        gioco$campo[[i]]$tappata <- FALSE
      }
    })
  }
  # Sincronizza i campi dei giocatori
  gioco <- aggiorna_campi_giocatori(gioco)
  # Aggiungi un messaggio al log
  messaggio <- paste0(gioco$get_giocatore_attivo()$nome, " stappa tutte le sue carte.")
  gioco$log <- append(gioco$log, messaggio)
  return(gioco)
}



