salva_snapshot <- function(gioco, tag = "") {
  gioco$snapshots <- append(gioco$snapshots, list(
    list(
      timestamp = Sys.time(),
      tag = tag,
      turno_corrente = gioco$turno_corrente,
      turno_giocatore_attivo = gioco$conta_turno_giocatore_attivo,
      giocatore_attivo = gioco$get_giocatore_attivo,
      stato = gioco
    )
  ))
  return(gioco)
}
