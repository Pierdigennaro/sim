# Stub minimale: assegna priorità a un giocatore specifico
assegna_priorita <- function(gioco, indice_giocatore) {
  # Controllino veloce sugli indici
  n <- length(gioco$giocatori %||% list())
  if (n == 0L) {
    stop("assegna_priorita: nessun giocatore presente in gioco.")
  }
  if (indice_giocatore < 1L || indice_giocatore > n) {
    stop("assegna_priorita: indice_giocatore fuori dai limiti.")
  }
  
  # Salvo chi ha la priorità come indice
  gioco$giocatore_con_priorita <- indice_giocatore
  
  # Recupero il giocatore per il log
  giocatore <- gioco$giocatori[[indice_giocatore]]
  nome <- giocatore$nome
  if (is.null(nome)) {
    nome <- paste0("Giocatore_", indice_giocatore)
  }
  
  # Log minimale
  gioco <- aggiungi_log(
    gioco,
    paste0("Il giocatore ", nome, " ottiene priorità.")
  )
  
  # TODO in futuro:
  # - aprire una "finestra di priorità"
  # - permettere al giocatore di lanciare magie/attivare abilità
  # - passare la priorità agli altri giocatori
  
  return(gioco)
}

# Stub minimale: assegna priorità al giocatore attivo
assegna_priorita_giocatore_attivo <- function(gioco) {
  if (is.null(gioco$indice_giocatore_attivo)) {
    stop("assegna_priorita_giocatore_attivo: indice_giocatore_attivo non impostato.")
  }
  
  gioco <- assegna_priorita(gioco, gioco$indice_giocatore_attivo)
  return(gioco)
}
