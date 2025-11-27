mischia_mazzo <- function(giocatore) {
  if (length(giocatore$mazzo$carte) > 1) {
    giocatore$mazzo$carte <- sample(giocatore$mazzo$carte)
  }
  return(giocatore)
}

pesca_carta_mazzo <- function(mazzo) {
  if (length(mazzo$carte) == 0) {
    return(list(mazzo = mazzo, carta = NULL))
  }
  carta_pescata <- mazzo$carte[[1]]
  mazzo$carte <- mazzo$carte[-1]
  return(list(mazzo = mazzo, carta = carta_pescata))
}

aggiungi_carta <- function(giocatore, carta, posizione="in_fondo") {
  if (posizione=="in_fondo") {
    giocatore$mazzo$carte <- append(giocatore$mazzo$carte, carta)
  }else if(posizione=="in_cima") {
    giocatore$mazzo$carte <- append(carta,giocatore$mazzo$carte)
  }
  return(giocatore)
}

assegna_priorita_giocatore_attivo <- function(gioco) {
  giocatore <- gioco$get_giocatore_attivo()
  gioco <- aggiungi_log(gioco, paste0("Il giocatore ", giocatore$nome, " ottiene priorità."))
  
  # Placeholder: eventuale IA o input giocatore
  # gioco <- gestisci_azioni_priorita(gioco, giocatore)
  
  return(gioco)
}

gestisci_inneschi_pescata <- function(gioco, indice_giocatore, carta_pescata) {
  giocatore <- gioco$giocatori[[indice_giocatore]]
  
  for (permanente in giocatore$campo) {
    if (!is.null(permanente$abilita_innescate)) {
      for (abilita in permanente$abilita_innescate) {
        if (abilita$tipo_evento == "pesca_carta") {
          gioco$pila <- append(gioco$pila, list(list(
            fonte = permanente$nome,
            abilita = abilita,
            controllore = giocatore$nome
          )))
          messaggio <- paste("Si innesca l'abilità di", permanente$nome, ":", abilita$descrizione)
          gioco <- aggiungi_log(gioco, messaggio, tipo = "innesco")
        }
      }
    }
  }
  
  return(gioco)
}

pesca_carta <- function(gioco, indice_giocatore) {
  giocatore <- gioco$giocatori[[indice_giocatore]]
  risultato <- pesca_carta_mazzo(giocatore$mazzo)
  giocatore$mazzo <- risultato$mazzo
  
  if (is.null(risultato$carta)) {
    # Grimorio vuoto → perdita (regola 704.5b)
    gioco <- aggiungi_log(gioco, paste(giocatore$nome, "non può pescare: grimorio vuoto!"), tipo = "warning")
    gioco <- controlla_azioni_stato(gioco)
  } else { # QUANDO SI FA CONTROLLO AZIONI? PRIMA O DOPO I TRIGGER DI PESCATA?
    giocatore$mano <- append(giocatore$mano, list(risultato$carta))
    gioco <- aggiungi_log(gioco, paste(giocatore$nome, "pesca", risultato$carta$nome), tipo = "azione")
    
    # Inneschi legati alla pescata
    gioco <- gestisci_inneschi_pescata(gioco, indice_giocatore, risultato$carta)
  }
  
  gioco$giocatori[[indice_giocatore]] <- giocatore
  return(gioco)
}
