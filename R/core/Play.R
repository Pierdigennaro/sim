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

esegui_turno <- function(gioco, verbose = FALSE) {
  # Giocatore attivo all'inizio del turno
  giocatore_attivo <- get_giocatore_attivo(gioco)
  nome_giocatore   <- giocatore_attivo$nome
  
  # Log inizio turno
  msg_start <- paste(
    "───────────────",
    paste("INIZIO TURNO DI", nome_giocatore),
    "───────────────",
    sep = "\n"
  )
  gioco <- aggiungi_log(gioco, msg_start, tipo = "inizio_turno")
  if (verbose) cat(msg_start, "\n")
  
  # Reset variabili dipendenti dal turno
  gioco <- reset_variabili_turno(gioco)
  
  # === Fase iniziale (STAP, mantenimento, pescata) ===
  gioco <- fase_iniziale(gioco)
  gioco <- controlla_azioni_stato(gioco)
  
  # === Prima fase principale ===
  # ATTENZIONE al nome: assicurati di avere la funzione
  # 'fase_principale_1' (o fai un alias se si chiama 'fase_principale_1')
  gioco <- fase_principale_1(gioco)
  gioco <- controlla_azioni_stato(gioco)
  
  # === Combattimento ===
  gioco <- fase_combattimento(gioco)
  gioco <- controlla_azioni_stato(gioco)
  
  # === Seconda fase principale ===
  gioco <- fase_principale2(gioco)
  gioco <- controlla_azioni_stato(gioco)
  
  # === Fase finale (end step + cleanup) ===
  gioco <- fase_finale(gioco)
  gioco <- controlla_azioni_stato(gioco)
  
  # Passa al prossimo giocatore / turno
  gioco <- passa_al_prossimo_giocatore(gioco)
  
  # Log fine turno (del giocatore che ha appena giocato)
  msg_end <- paste(
    "───────────────",
    paste("FINE TURNO DI", nome_giocatore),
    "───────────────",
    sep = "\n"
  )
  gioco <- aggiungi_log(gioco, msg_end, tipo = "fine_turno")
  if (verbose) cat(msg_end, "\n")
  
  return(gioco)
}
