fase_iniziale <- function(gioco) {
  messaggio <- paste("=== Fase Iniziale ===")
  gioco <- aggiungi_log(gioco, messaggio, tipo = "fase")
  gioco <- sottofase_untap(gioco)
  gioco <- sottofase_upkeep(gioco)
  gioco <- sottofase_pesca(gioco)
  return(gioco)
}

sottofase_untap <- function(gioco) {
  messaggio <- paste("Sottofase di STAP (Untap)")
  gioco <- aggiungi_log(gioco, messaggio, tipo = "sottofase")
  gioco$fase_corrente <- "stap"
  gioco <- phase_in_giocatoreattivo(gioco)
  gioco <- aggiorna_ciclo_giorno_notte(gioco)
  gioco <- stappa_giocatoreattivo(gioco)
  return(gioco)
}


sottofase_upkeep <- function(gioco) {
  messaggio <- paste("Sottofase di Mantenimento (Upkeep)")
  gioco <- aggiungi_log(gioco, messaggio, tipo = "sottofase")
  gioco$fase_corrente <- "mantenimento"
  # 1. Metti in pila tutte le abilità innescate durante la sottofase di STAP o all’inizio dell’upkeep
  gioco <- gestisci_inneschi_upkeep(gioco)
  
  controlla_azioni_stato(gioco)
  
  # 2. Il giocatore attivo ottiene priorità
  gioco <- assegna_priorita_giocatore_attivo(gioco)
  
  # (3) Gestione eventuali sottofasi multiple o effetti particolari
  # Placeholder per future carte come Paradox Haze
  
  return(gioco)
}

sottofase_draw <- function(gioco) {
  messaggio <- "Sottofase di Acquisizione (Draw Step)"
  gioco <- aggiungi_log(gioco, messaggio, tipo = "sottofase")
  gioco$fase_corrente <- "pesca"
  # 1. Inneschi "all’inizio della draw step"
  gioco <- gestisci_inneschi_draw(gioco)
  
  # 2. Controllo delle azioni generate dallo stato
  gioco <- controlla_azioni_stato(gioco)
  
  # 3. Il giocatore attivo pesca una carta
  gioco <- pesca_carta(gioco, gioco$giocatore_attivo)
  
  # 4. Controllo di stato dopo la pescata
  gioco <- controlla_azioni_stato(gioco)
  
  # 5. Inneschi legati alla pescata
  gioco <- gestisci_inneschi_pescata(gioco)
  
  # 6. Priorità al giocatore attivo
  gioco <- assegna_priorita_giocatore_attivo(gioco)
  
  return(gioco)
}
