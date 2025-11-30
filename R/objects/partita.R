crea_partita <- function(giocatori,
                         ordine_casuale = TRUE,
                         id_partita = NULL,
                         formato = NULL) {
  # Controlli base
  if (length(giocatori) < 2L) {
    stop("crea_partita: servono almeno due giocatori.")
  }
  
  # Determino (ed eventualmente randomizzo) l'ordine di turno
  idx <- seq_along(giocatori)
  if (ordine_casuale) {
    ordine <- sample(idx, size = length(idx), replace = FALSE)
    giocatori <- giocatori[ordine]
  } else {
    ordine <- idx
  }
  
  # ID partita
  if (is.null(id_partita)) {
    id_partita <- paste0("game_", as.integer(runif(1, 1, 1e9)))
  }
  
  # Formato: se non specificato, provo a leggerlo dal primo giocatore
  if (is.null(formato)) {
    formato <- giocatori[[1]]$formato %||% "sconosciuto"
  }
  
  # Stato iniziale di turno e fase
  turno_iniziale   <- 1L
  attivo_iniziale  <- 1L   # il primo della lista è il giocatore attivo
  
  gioco <- list(
    id_partita = id_partita,
    formato    = formato,
    
    # Giocatori e ordine di turno
    giocatori        = giocatori,
    ordine_giocatori = ordine,   # mappa rispetto all'ordine di input
    
    # Turni / fasi
    turno_corrente          = turno_iniziale,   # numero di turno globale
    indice_giocatore_attivo = attivo_iniziale,  # indice nella lista 'giocatori'
    fase_corrente           = "pre_partita",    # o "inizio_turno", lo decideremo nel motore
    sottofase_corrente      = NULL,
    stato                   = "in_corso",       # "in_corso" | "conclusa"
    
    # Zone globali
    campo  = list(),   # battlefield
    pila   = list(),   # stack
    esilio = list(),   # esilio globale
    
    # Meccaniche globali (giorno/notte, ecc.)
    ciclo_giorno_notte = "nessuno",   # "giorno" | "notte" | "nessuno"
    
    # Tracking magie lanciate (per giocatore)
    magie_lanciate_turno_corrente   = integer(length(giocatori)),
    magie_lanciate_turno_precedente = integer(length(giocatori)),
    
    # Eventi e log
    eventi_in_coda = list(),
    log            = list()
  )
  
  # Log di avvio
  nomi <- vapply(giocatori, function(g) g$nome, character(1))
  messaggio <- paste("Inizia la partita tra", paste(nomi, collapse = " vs "))
  gioco <- aggiungi_log(gioco, messaggio, tipo = "inizio")
  
  return(gioco)
}

crea_partita_2_4 <- function(giocatore1,
                             giocatore2,
                             giocatore3 = NULL,
                             giocatore4 = NULL,
                             ordine_casuale = TRUE,
                             id_partita = NULL,
                             formato = NULL) {
  giocatori <- list(giocatore1, giocatore2)
  if (!is.null(giocatore3)) giocatori <- c(giocatori, list(giocatore3))
  if (!is.null(giocatore4)) giocatori <- c(giocatori, list(giocatore4))
  
  crea_partita(
    giocatori      = giocatori,
    ordine_casuale = ordine_casuale,
    id_partita     = id_partita,
    formato        = formato
  )
}
