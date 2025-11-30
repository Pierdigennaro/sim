crea_mazzo <- function(lista_carte,
                       nome  = "Mazzo senza nome",
                       owner = NULL) {
  
  # Normalizzo ogni carta per l'uso nel mazzo
  carte_norm <- lapply(lista_carte, function(c) {
    # se manca owner, uso quello passato al mazzo (se c'è)
    c$owner      <- owner %||% c$owner %||% "player1"
    # se manca controller, di default coincide con owner
    c$controller <- c$controller %||% c$owner
    # una carta nel mazzo non è tappata e non ha danno assegnato
    c$tappata    <- FALSE
    c$danno      <- c$danno %||% 0L
    # zona coerente
    c$zona       <- "mazzo"
    # token / copia sempre definiti
    c$token      <- isTRUE(c$token)
    c$copia      <- isTRUE(c$copia)
    c
  })
  
  list(
    nome  = nome,
    owner = owner,  # opzionale, ma utile
    carte = carte_norm,
    id    = paste0("deck_", as.integer(runif(1, 1, 1e9)))
  )
}

# funzione di comodo per contare le carte (invece di tenere n_carte)
conta_carte_mazzo <- function(mazzo) {
  length(mazzo$carte)
}

stampa_mazzo <- function(mazzo, n = 5, titolo_extra = NULL) {
  tot <- conta_carte_mazzo(mazzo)
  
  cat("Mazzo:", mazzo$nome, " (", tot, " carte )\n", sep = "")
  if (!is.null(titolo_extra)) {
    cat(titolo_extra, "\n")
  }
  
  if (tot == 0) {
    cat("  [mazzo vuoto]\n")
    return(invisible(NULL))
  }
  
  n_mostrare <- min(n, tot)
  cat("Prime", n_mostrare, "carte:\n")
  for (i in seq_len(n_mostrare)) {
    carta <- mazzo$carte[[i]]
    cat(sprintf(" %2d) %s\n", i, carta$nome))
  }
  
  invisible(NULL)
}

stampa_mazzo_giocatore <- function(giocatore, n = 5) {
  if (is.null(giocatore$mazzo)) {
    cat("Il giocatore", giocatore$nome, "non ha un mazzo associato.\n")
    return(invisible(NULL))
  }
  titolo <- paste("Giocatore:", giocatore$nome)
  stampa_mazzo(giocatore$mazzo, n = n, titolo_extra = titolo)
}

riepilogo_mazzo <- function(mazzo) {
  if (length(mazzo$carte) == 0) {
    cat("Mazzo vuoto.\n")
    return(invisible(NULL))
  }
  
  nomi <- vapply(mazzo$carte, `[[`, "", "nome")
  tab  <- sort(table(nomi), decreasing = TRUE)
  
  cat("Riepilogo mazzo", mazzo$nome, ":\n")
  for (nome in names(tab)) {
    cat(sprintf("%2d x %s\n", tab[[nome]], nome))
  }
  invisible(NULL)
}

# Mischia un oggetto mazzo (funzione pura)
mischia_mazzo <- function(mazzo) {
  n <- length(mazzo$carte)
  
  # 0 o 1 carta: nessun effetto
  if (n <= 1L) {
    return(mazzo)
  }
  
  # sample() su liste va bene: permuta gli elementi
  mazzo$carte <- sample(mazzo$carte, size = n, replace = FALSE)
  
  return(mazzo)
}

# Wrapper di comodo a livello giocatore (compatibilità)
mischia_mazzo_giocatore <- function(giocatore) {
  if (is.null(giocatore$mazzo)) {
    warning("Il giocatore non ha un mazzo.")
    return(giocatore)
  }
  giocatore$mazzo <- mischia_mazzo(giocatore$mazzo)
  return(giocatore)
}

pesca_carta_mazzo <- function(mazzo) {
  # se l'oggetto non è ben formato, meglio fallire rumorosamente
  if (is.null(mazzo$carte)) {
    stop("pesca_carta_mazzo: il mazzo non ha il campo 'carte'.")
  }
  
  n <- length(mazzo$carte)
  
  # Grimorio vuoto: nessuna carta pescata
  if (n == 0L) {
    return(list(
      mazzo = mazzo,
      carta = NULL
    ))
  }
  
  # per convenzione: la 'cima del grimorio' è l'indice 1
  carta_pescata <- mazzo$carte[[1L]]
  
  # rimuovo la carta dal mazzo
  mazzo$carte <- mazzo$carte[-1L]
  
  # NOTA IMPORTANTE:
  # qui NON aggiorniamo carta_pescata$zona ("mano") né owner/controller:
  # questo è lavoro della funzione di livello gioco (pesca_carta / sposta_carta).
  
  return(list(
    mazzo = mazzo,
    carta = carta_pescata
  ))
}


