crea_giocatore <- function(
    mazzo,                  # oggetto mazzo restituito da crea_mazzo()
    formato     = "commander",
    comandante  = NULL,     # carta comandante o NULL
    nome        = NULL,
    id          = NULL
) {
  # Nome di default
  if (is.null(nome)) {
    nome <- paste0("Giocatore_", as.integer(runif(1, 1, 1e6)))
  }
  
  # Se mazzo NON è un oggetto creato con crea_mazzo (manca $carte),
  # lo converto al volo assumendo sia una semplice lista di carte.
  if (is.null(mazzo$carte)) {
    mazzo <- crea_mazzo(lista_carte = mazzo,
                        nome        = paste0("Mazzo di ", nome),
                        owner       = nome)
  }
  
  # Punti vita iniziali per il formato
  punti_vita_iniziali <- if (tolower(formato) == "commander") 40L else 20L
  
  # ID del giocatore
  if (is.null(id)) {
    id <- paste0("player_", as.integer(runif(1, 1, 1e9)))
  }
  
  giocatore <- list(
    id        = id,
    nome      = nome,
    formato   = formato,
    
    # Stato di gioco base
    punti_vita = punti_vita_iniziali,
    veleno     = 0L,
    ha_perso   = FALSE,
    
    # Flag per SBA "ha tentato di pescare da grimorio vuoto"
    ha_tentato_pescata_grimorio_vuoto = FALSE,
    
    # Comandante / commander tax
    comandante           = comandante,
    comandante_tassa     = 0L,
    comandante_posizione = if (!is.null(comandante)) "zona_comando" else NULL,
    
    # Zone personali
    mazzo         = mazzo,     # oggetto mazzo (non ancora mischiato di default)
    mano          = list(),
    mano_iniziale = list(),    # la riempiremo in setup partita
    cimitero      = list(),
    esilio        = list(),
    
    # Conteggio e stato
    turni_iniziati      = 0L,
    terre_giocate_turno = 0L,
    
    # Risorse / stato aggiuntivo
    mana_pool = list(),        # la struttura la decideremo meglio più avanti
    
    # Campi "legacy" che idealmente vorremo togliere:
    # il campo globale della partita dovrebbe essere gioco$campo, non qui
    campo          = list(),   # sconsiglio di usarlo, meglio gioco$campo
    terre_in_campo = list()    # ridondante, in futuro da rimuovere
  )
  
  return(giocatore)
}

inizializza_giocatore_in_partita <- function(giocatore, n_carte_iniziali = 7L) {
  # Mischia il mazzo del giocatore
  giocatore$mazzo <- mischia_mazzo(giocatore$mazzo)
  
  # Pesca le prime n carte come mano iniziale
  mano_iniziale <- list()
  for (i in seq_len(n_carte_iniziali)) {
    risultato <- pesca_carta_mazzo(giocatore$mazzo)
    giocatore$mazzo <- risultato$mazzo
    if (is.null(risultato$carta)) {
      # Grimorio finito prima della mano completa: evento raro, ma possibile
      break
    }
    carta <- risultato$carta
    # queste carte sono in mano fin dall'inizio: aggiorno zona
    carta$zona <- "mano"
    mano_iniziale[[length(mano_iniziale) + 1L]] <- carta
  }
  
  giocatore$mano          <- mano_iniziale
  giocatore$mano_iniziale <- mano_iniziale
  
  return(giocatore)
}

stampa_giocatore <- function(giocatore, mostra_mano = TRUE) {
  cat("───────────────────────────────\n")
  cat("Nome: ", giocatore$nome, "\n", sep = "")
  cat("ID: ", giocatore$id, "\n", sep = "")
  cat("Formato: ", giocatore$formato %||% "sconosciuto", "\n", sep = "")
  
  cat("Punti vita: ", giocatore$punti_vita, "\n", sep = "")
  cat("Segnalini veleno: ", giocatore$veleno, "\n", sep = "")
  if (isTRUE(giocatore$ha_perso)) {
    cat("Stato: HA PERSO LA PARTITA\n")
  }
  
  cat("Comandante: ",
      if (!is.null(giocatore$comandante)) giocatore$comandante$nome else "Nessuno",
      "\n", sep = "")
  
  # dimensioni zone personali
  n_mano    <- length(giocatore$mano)
  n_mazzo   <- if (!is.null(giocatore$mazzo$carte)) length(giocatore$mazzo$carte) else length(giocatore$mazzo)
  n_cim     <- length(giocatore$cimitero)
  n_esilio  <- length(giocatore$esilio)
  
  cat("Carte in mano: ", n_mano, "\n", sep = "")
  cat("Carte nel mazzo: ", n_mazzo, "\n", sep = "")
  cat("Carte nel cimitero: ", n_cim, "\n", sep = "")
  cat("Carte in esilio: ", n_esilio, "\n", sep = "")
  cat("Terre giocate questo turno: ", giocatore$terre_giocate_turno, "\n", sep = "")
  cat("Turni iniziati: ", giocatore$turni_iniziati, "\n", sep = "")
  
  cat("───────────────────────────────\n")
  
  # opzionale: elenco delle carte in mano
  if (mostra_mano && n_mano > 0) {
    cat("Carte in mano:\n")
    for (i in seq_len(n_mano)) {
      cat(sprintf("  - %s\n", giocatore$mano[[i]]$nome))
    }
    cat("───────────────────────────────\n")
  }
}
