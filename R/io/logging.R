aggiungi_log <- function(gioco, messaggio, tipo = "info", mostra = TRUE) {
  timestamp <- format(Sys.time(), "%H:%M:%S")
  if (is.null(tipo)) {
    testo <- paste0("[", timestamp, "] ",messaggio)
  }else{testo <- paste0("[", timestamp, "][", tipo, "] ", messaggio)}
  if (is.null(gioco$log)) gioco$log <- character()
  gioco$log <- append(gioco$log, testo)
  if (mostra) message(testo)
  invisible(gioco)
}
