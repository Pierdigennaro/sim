# Funzione principale: esegue l’intera partita
gioca_partita <- function(gioco, max_turni = 200, verbose = TRUE) {
  # Se lo stato non è impostato, lo inizializzo
  if (is.null(gioco$stato)) {
    gioco$stato <- "in_corso"
  }
  if (is.null(gioco$turno_corrente)) {
    gioco$turno_corrente <- 1L
  }
  if (is.null(gioco$indice_giocatore_attivo)) {
    gioco$indice_giocatore_attivo <- 1L
  }
  
  msg_start <- "=== INIZIO PARTITA ==="
  gioco <- aggiungi_log(gioco, msg_start, tipo = "partita")
  if (verbose) cat(msg_start, "\n")
  
  # Loop principale di partita
  while (identical(gioco$stato, "in_corso") &&
         gioco$turno_corrente <= max_turni) {
    
    # Esegui un turno completo del giocatore attivo
    gioco <- esegui_turno(gioco, verbose = verbose)
    
    # Dopo ogni turno: controlla azioni di stato e condizioni di vittoria
    gioco <- controllo_azioni_stato(gioco)
    gioco <- controlla_vittoria(gioco)
    
    # Se la partita è finita, esci dal loop
    if (!identical(gioco$stato, "in_corso")) break
  }
  
  # Se sono finiti i turni senza vincitore → pareggio tecnico
  if (identical(gioco$stato, "in_corso") &&
      gioco$turno_corrente > max_turni) {
    gioco$stato <- "pareggio"
    gioco$motivazione_fine <- paste(
      "Raggiunto il limite di", max_turni, "turni."
    )
    gioco <- aggiungi_log(
      gioco,
      paste0("La partita termina in pareggio (limite di ",
             max_turni, " turni)."),
      tipo = "fine_partita"
    )
  }
  
  msg_end <- "=== FINE PARTITA ==="
  gioco <- aggiungi_log(gioco, msg_end, tipo = "fine_partita")
  if (verbose) cat(msg_end, "\n")
  
  return(gioco)
}

esegui_turno <- function(gioco, verbose = TRUE) {
  # Recupero giocatore attivo: preferisco usare la closure se esiste,
  # altrimenti accedo direttamente alla lista.
  g_attivo <- if (is.function(gioco$get_giocatore_attivo)) {
    gioco$get_giocatore_attivo()
  } else {
    gioco$giocatori[[gioco$indice_giocatore_attivo]]
  }
  
  nome_giocatore <- g_attivo$nome
  if (is.null(nome_giocatore)) {
    nome_giocatore <- paste0("Giocatore_", gioco$indice_giocatore_attivo)
  }
  
  msg_start <- paste(
    "───────────────",
    paste("INIZIO TURNO DI", nome_giocatore),
    "───────────────",
    sep = "\n"
  )
  gioco <- aggiungi_log(gioco, msg_start, tipo = "inizio_turno")
  if (verbose) cat(msg_start, "\n")
  
  # Resetta i contatori di turno del giocatore attivo
  gioco <- reset_variabili_turno(gioco)
  
  # Fasi del turno (in ordine CR):
  # 1) Fase iniziale (stap / mantenimento / pescata)
  gioco <- fase_iniziale(gioco)
  
  # 2) Prima fase principale
  #    (nel codice delle fasi il nome è fase_principale_1)
  gioco <- fase_principale_1(gioco)
  
  # 3) Fase di combattimento
  gioco <- fase_combattimento(gioco)
  
  # 4) Seconda fase principale
  gioco <- fase_principale2(gioco)
  
  # 5) Fase finale (end step + cleanup)
  gioco <- fase_finale(gioco)
  
  # Un ultimo controllo SBA a fine turno
  gioco <- controllo_azioni_stato(gioco)
  
  # Passa al prossimo giocatore / incrementa il numero di turno
  gioco <- passa_al_prossimo_giocatore(gioco)
  
  msg_end <- paste(
    "───────────────",
    paste("FINE TURNO DI", nome_giocatore),
    "───────────────",
    sep = "\n"
  )
  gioco <- aggiungi_log(gioco, msg_end, tipo = "fine_turno")
  if (verbose) cat(msg_end, "\n")
  
  return(gioco)
}

controlla_vittoria <- function(gioco) {
  # Mi aspetto che controllo_azioni_stato abbia già settato $ha_perso
  # dove serve (vita <=0, grimorio vuoto, ecc.)
  ha_perso <- vapply(
    gioco$giocatori,
    function(g) isTRUE(g$ha_perso),
    logical(1)
  )
  
  n_in_gioco <- sum(!ha_perso)
  
  # Nessuno ha perso → partita ancora in corso
  if (n_in_gioco ==  length(gioco$giocatori)) {
    return(gioco)
  }
  
  # Tutti hanno perso → pareggio
  if (n_in_gioco == 0L) {
    gioco$stato <- "pareggio"
    gioco$motivazione_fine <- "Tutti i giocatori hanno perso (SBA)."
    
    gioco <- aggiungi_log(
      gioco,
      "Tutti i giocatori hanno perso: la partita termina in pareggio.",
      tipo = "fine_partita"
    )
    return(gioco)
  }
  
  # Esattamente un giocatore non ha perso → abbiamo un vincitore
  if (n_in_gioco == 1L) {
    idx_vincitore <- which(!ha_perso)
    vincitore <- gioco$giocatori[[idx_vincitore]]
    
    gioco$stato <- "finita"
    gioco$vincitore_indice <- idx_vincitore
    gioco$vincitore_nome   <- vincitore$nome
    
    gioco <- aggiungi_log(
      gioco,
      paste0("La partita termina: vince ", vincitore$nome, "."),
      tipo = "fine_partita"
    )
    
    return(gioco)
  }
  
  # Caso intermedio: >1 giocatore in vita e almeno 1 con ha_perso==TRUE
  # (es. in multiplayer, qualcuno è stato eliminato ma la partita continua)
  # In questo caso non chiudo la partita, mi limito a loggare.
  if (n_in_gioco >= 2L && n_in_gioco < length(gioco$giocatori)) {
    msg <- paste(
      "Alcuni giocatori sono stati eliminati, ma la partita continua con",
      n_in_gioco, "giocatori ancora in gioco."
    )
    gioco <- aggiungi_log(gioco, msg, tipo = "info")
  }
  
  return(gioco)
}
