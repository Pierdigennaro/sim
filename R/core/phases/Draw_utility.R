gestisci_inneschi_acquisizione <- function(gioco) {
  # Trova le carte nel campo con abilità "trigger_draw"
  inneschi <- list()
  for (i in seq_along(gioco$campo)) {
    carta <- gioco$campo[[i]]
    abilita <- carta$abilita
    if (!is.null(abilita)) {
      if ("trigger_acquisizione" %in% abilita$tipo ) {
        inneschi <- append(inneschi, list(list(
          sorgente = carta$nome,
          descrizione = abilita$descrizione,
          tipo = abilita$tipo
        )))
      }
    }
  }
  
  # Metti tutti gli inneschi trovati in pila il giocatore che ha priorità sceglie l'ordine
  if (length(inneschi) > 0) {
    gioco$pila <- append(gioco$pila, inneschi)
    gioco <- aggiungi_log(gioco, paste0(length(inneschi), " abilità innescate messe in pila all'inizio della sottofase di acquisizione."))
  } else {
    gioco <- aggiungi_log(gioco, "Nessuna abilità innescata durante la sottofase di acquisizione.")
  }
  
  return(gioco)
}
