fase_principale_1 <- function(gioco) {
  
  # Log della fase
  messaggio <- "=== Prima Fase Principale ==="
  gioco <- aggiungi_log(gioco, messaggio, tipo = "fase")
  gioco$fase_corrente <- "prima fase principale"
  
  # (1) Abilità innescate all’inizio della fase principale
  # Placeholder per futuro
  # gioco <- gestisci_inneschi_main_phase(gioco)
  
  # (2) Il giocatore attivo ottiene priorità
  gioco <- assegna_priorita_giocatore_attivo(gioco)
  
  # (3) Placeholder: scelta del giocatore su cosa fare
  # gioco <- gestisci_azioni_giocatore_attivo(gioco)
  
  # (4) Log di fine della fase
  messaggio <- "=== FINE Prima Fase Principale ==="
  gioco <- aggiungi_log(gioco, messaggio, tipo = "fase")
  
  return(gioco)
}
