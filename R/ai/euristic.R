gioca_terra <- function(giocatore, indice_carta) {
  if (giocatore$terre_giocate_turno >= 1) return(giocatore) # limite una terra/turno
  carta <- giocatore$mano[[indice_carta]]
  if (!"Terra" %in% carta$tipo) return(giocatore)
  giocatore$campo <- append(giocatore$campo, list(carta))
  giocatore$mano[[indice_carta]] <- NULL
  giocatore$terre_giocate_turno <- giocatore$terre_giocate_turno + 1
  return(giocatore)
}

