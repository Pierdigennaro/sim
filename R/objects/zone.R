# zona: "mazzo", "mano", "cimitero", "campo", "pila", "esilio"
get_zona <- function(gioco, zona, indice_giocatore = NULL) {
  zona <- tolower(zona)
  
  # Zone globali
  if (zona %in% c("campo", "pila", "esilio")) {
    return(gioco[[zona]])
  }
  
  # Zone personali: serve il giocatore
  if (is.null(indice_giocatore)) {
    stop("get_zona: per la zona '", zona, "' serve indice_giocatore.")
  }
  
  giocatore <- gioco$giocatori[[indice_giocatore]]
  
  if (zona == "mazzo") {
    # ritorno direttamente le carte del mazzo
    return(giocatore$mazzo$carte)
  }
  if (zona == "mano") {
    return(giocatore$mano)
  }
  if (zona == "cimitero") {
    return(giocatore$cimitero)
  }
  
  stop("get_zona: zona '", zona, "' non gestita.")
}

set_zona <- function(gioco, zona, valore, indice_giocatore = NULL) {
  zona <- tolower(zona)
  
  if (zona %in% c("campo", "pila", "esilio")) {
    gioco[[zona]] <- valore
    return(gioco)
  }
  
  if (is.null(indice_giocatore)) {
    stop("set_zona: per la zona '", zona, "' serve indice_giocatore.")
  }
  
  if (zona == "mazzo") {
    gioco$giocatori[[indice_giocatore]]$mazzo$carte <- valore
    return(gioco)
  }
  if (zona == "mano") {
    gioco$giocatori[[indice_giocatore]]$mano <- valore
    return(gioco)
  }
  if (zona == "cimitero") {
    gioco$giocatori[[indice_giocatore]]$cimitero <- valore
    return(gioco)
  }
  
  stop("set_zona: zona '", zona, "' non gestita.")
}

sposta_carta <- function(gioco,
                         da_zona,
                         a_zona,
                         indice_carta,
                         da_giocatore = NULL,
                         a_giocatore  = NULL) {
  da_zona <- tolower(da_zona)
  a_zona  <- tolower(a_zona)
  
  # Normalizzo gli identificativi dei giocatori (possono essere indice, id o nome)
  if (!is.null(da_giocatore) && !is.numeric(da_giocatore)) {
    da_giocatore <- trova_indice_giocatore(gioco, da_giocatore)
  }
  if (!is.null(a_giocatore) && !is.numeric(a_giocatore)) {
    a_giocatore <- trova_indice_giocatore(gioco, a_giocatore)
  }
  
  # Leggo la zona di origine
  lista_orig <- get_zona(gioco, da_zona, da_giocatore)
  n_orig <- length(lista_orig)
  
  if (indice_carta < 1L || indice_carta > n_orig) {
    stop("sposta_carta: indice_carta fuori range per la zona '", da_zona, "'.")
  }
  
  carta <- lista_orig[[indice_carta]]
  # Rimuovo dalla zona di origine
  lista_orig[[indice_carta]] <- NULL
  
  # Aggiorno la zona nella carta (campo derivato, ma utile per debug)
  carta$zona <- a_zona
  
  # Se va sul campo, imposto il controller se possibile
  if (a_zona == "campo" && !is.null(a_giocatore)) {
    carta$controller <- gioco$giocatori[[a_giocatore]]$id %||%
      gioco$giocatori[[a_giocatore]]$nome
  }
  
  # Leggo zona di destinazione
  lista_dest <- get_zona(gioco, a_zona, a_giocatore)
  lista_dest[[length(lista_dest) + 1L]] <- carta
  
  # Scrivo le due zone aggiornate
  gioco <- set_zona(gioco, da_zona, lista_orig, da_giocatore)
  gioco <- set_zona(gioco, a_zona, lista_dest, a_giocatore)
  
  return(gioco)
}
