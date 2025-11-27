gioca_partita <- function(gioco, max_turni = 200) {
  
  messaggio <- paste("=== INIZIO PARTITA ===")
  gioco <- aggiungi_log(gioco, messaggio, tipo = "inizio")
  
  while (gioco$stato == "in_corso" && gioco$turno_corrente <= max_turni) {
    
    g_attivo <- gioco$get_giocatore_attivo()
    messaggio <- paste("---- Turno", gioco$turno_corrente, ":", g_attivo$nome, ", Turno ",  (floor((gioco$turno_corrente - gioco$indice_giocatore_attivo) / length(gioco$giocatori)) + 1))
    gioco <- aggiungi_log(gioco, messaggio, tipo = NULL)
    
    # 2.1 Turno completo ----
    gioco <- esegui_turno(gioco)
    
    # 2.2 Controllo condizioni di vittoria ----
    gioco <- controlla_vittoria(gioco)
    
    # 2.3 Passaggio turno ----
    if (gioco$stato == "in_corso") {
      gioco$indice_giocatore_attivo <- (gioco$indice_giocatore_attivo %% length(gioco$giocatori)) + 1
      gioco$turno_corrente <- gioco$turno_corrente + 1
    }
  }
  
  messaggio <- paste("=== FINE PARTITA ===")
  gioco <- aggiungi_log(gioco, messaggio, tipo = "fine")
  return(gioco)
}

esegui_turno <- function(gioco) {
  giocatore <- gioco$get_giocatore_attivo()
  messaggio <- paste("───────────────\nINIZIO TURNO DI", giocatore$nome, "\n───────────────\n")
  gioco <- aggiungi_log(gioco, messaggio, tipo = "inizio turno")
  
  # giocatore$n_turno <- giocatore$n_turno + 1
  # gioco <- aggiungi_log(gioco, paste("Inizio turno", giocatore$n_turno, "di", giocatore$nome))
  gioco <- reset_variabili_turno(gioco)
  
  gioco <- fase_iniziale(gioco)
  gioco <- fase_principale1(gioco)
  gioco <- fase_combattimento(gioco)
  gioco <- fase_principale2(gioco)
  gioco <- fase_finale(gioco)
  
  gioco <- passa_al_prossimo_giocatore(gioco)
  
  messaggio <- paste("───────────────\nFINE TURNO DI", giocatore$nome, "\n───────────────\n")
  gioco <- aggiungi_log(gioco, messaggio, tipo = "fine turno")
  return(list(gioco = gioco))
}

