crea_partita <- function(giocatore1, giocatore2, giocatore3 = NULL, giocatore4 = NULL, ordine_casuale = TRUE) {
  
  # 1.1 Crea lista giocatori ----
  giocatori <- list(giocatore1, giocatore2)
  if (!is.null(giocatore3)) giocatori <- append(giocatori, list(giocatore3))
  if (!is.null(giocatore4)) giocatori <- append(giocatori, list(giocatore4))
  
  if (ordine_casuale) giocatori <- sample(giocatori, replace = FALSE)
  
  # 1.2 Stato iniziale ----
  turno_corrente = 1
  gioco <- list(
    giocatori = giocatori,
    turno_corrente = 1,
    indice_giocatore_attivo = ((turno_corrente-1) %% length(giocatori)+1),
    fase_corrente = "inizio",
    stato = "in_corso",
    pila = list(),
    campo_globale = list(),
    ciclo_giorno_notte = "nessuno",
    magie_lanciate_turno = 0,
    magie_lanciate_giocatore_attivo_turno = 0,
    magie_lanciate_giocatore_attivo_turno_precedente = 0,
    fase_corrente=NULL,
    log = list()
  )
  
  # 1.3 Funzioni helper integrate ----
  gioco$get_giocatore_attivo <- function() gioco$giocatori[[gioco$indice_giocatore_attivo]]
  gioco$get_giocatori_non_attivi <- function() gioco$giocatori[-gioco$indice_giocatore_attivo]
  gioco$conta_turno_giocatore_attivo <- function() (floor((gioco$turno_corrente - gioco$indice_giocatore_attivo) / length(gioco$giocatori)) + 1)
  
  # 1.4 Log di avvio ----
  nomi <- sapply(giocatori, function(g) g$nome)
  
  messaggio <- paste("Inizia la partita tra:", paste(nomi, collapse = " vs "))
  gioco <- aggiungi_log(gioco, messaggio, tipo = "inizio")
  
  invisible(gioco)
}

controlla_azioni_stato <- function(gioco) {
  repeat {
    azioni_applicate <- FALSE
    
    # 1. Giocatori perdono se 0 o meno punti vita
    for (g in seq_along(gioco$giocatori)) {
      if (gioco$giocatori[[g]]$vita <= 0) {
        nome <- gioco$giocatori[[g]]$nome
        gioco <- aggiungi_log(gioco, paste0(nome, " ha 0 o meno punti vita e perde la partita."))
        gioco$giocatori[[g]]$ha_perso <- TRUE
        azioni_applicate <- TRUE
      }
    }
    
    # 2. Giocatori perdono se hanno pescato da grimorio vuoto
    for (g in seq_along(gioco$giocatori)) {
      if (isTRUE(gioco$giocatori[[g]]$ha_pescato_da_mazzo_vuoto)) {
        nome <- gioco$giocatori[[g]]$nome
        gioco <- aggiungi_log(gioco, paste0(nome, " ha pescato da un grimorio vuoto e perde la partita."))
        gioco$giocatori[[g]]$ha_perso <- TRUE
        azioni_applicate <- TRUE
      }
    }
    
    # 3. Giocatori perdono con 10 o più segnalini veleno
    for (g in seq_along(gioco$giocatori)) {
      if (gioco$giocatori[[g]]$veleno >= 10) {
        nome <- gioco$giocatori[[g]]$nome
        gioco <- aggiungi_log(gioco, paste0(nome, " ha 10 o più segnalini veleno e perde la partita."))
        gioco$giocatori[[g]]$ha_perso <- TRUE
        azioni_applicate <- TRUE
      }
    }
    
    # 4. Pedine o copie fuori dal campo cessano di esistere
    for (zona in c("mano", "cimitero", "esilio", "mazzo")) {
      carte_fuori <- which(vapply(gioco[[zona]], function(carta) isTRUE(carta$token) || isTRUE(carta$copia), logical(1)))
      if (length(carte_fuori) > 0) {
        gioco[[zona]] <- gioco[[zona]][-carte_fuori]
        azioni_applicate <- TRUE
        gioco <- aggiungi_log(gioco, "Pedine o copie fuori dal campo cessano di esistere.")
      }
    }
    
    # 5. Creature con costituzione <= 0 muoiono
    for (i in seq_along(gioco$campo)) {
      carta <- gioco$campo[[i]]
      if (carta$tipo == "creatura" && carta$costituzione <= 0) {
        proprietario <- carta$proprietario
        gioco <- aggiungi_log(gioco, paste0(carta$nome, " ha costituzione <= 0 e viene messa nel cimitero di ", proprietario, "."))
        gioco <- sposta_carta(gioco, i, da = "campo", a = "cimitero", proprietario = proprietario)
        azioni_applicate <- TRUE
      }
    }
    
    # 6. Creature con danno >= costituzione muoiono
    for (i in seq_along(gioco$campo)) {
      carta <- gioco$campo[[i]]
      if (carta$tipo == "creatura" && carta$danno >= carta$costituzione) {
        proprietario <- carta$proprietario
        gioco <- aggiungi_log(gioco, paste0(carta$nome, " ha subito danno letale e viene distrutta."))
        gioco <- sposta_carta(gioco, i, da = "campo", a = "cimitero", proprietario = proprietario)
        azioni_applicate <- TRUE
      }
    }
    
    # 7. Planeswalker con fedeltà 0 muoiono
    for (i in seq_along(gioco$campo)) {
      carta <- gioco$campo[[i]]
      if (carta$tipo == "planeswalker" && carta$fedelta <= 0) {
        proprietario <- carta$proprietario
        gioco <- aggiungi_log(gioco, paste0(carta$nome, " ha fedeltà 0 e viene messa nel cimitero di ", proprietario, "."))
        gioco <- sposta_carta(gioco, i, da = "campo", a = "cimitero", proprietario = proprietario)
        azioni_applicate <- TRUE
      }
    }
    
    # 8. Regola delle leggende (semplificata)
    leggendari <- gioco$campo[vapply(gioco$campo, function(c) isTRUE(c$leggendario), logical(1))]
    if (length(leggendari) > 1) {
      nomi <- unique(vapply(leggendari, `[[`, "", "nome"))
      for (nome in nomi) {
        stesse <- which(vapply(gioco$campo, function(c) c$nome == nome && isTRUE(c$leggendario), logical(1)))
        if (length(stesse) > 1) {
          # Mantiene solo la prima
          gioco <- aggiungi_log(gioco, paste0("Regola delle leggende: vengono mantenute solo una copia di ", nome, "."))
          gioco$campo <- gioco$campo[-stesse[-1]]
          azioni_applicate <- TRUE
        }
      }
    }
    
    # Fine del ciclo se nulla è stato applicato
    if (!azioni_applicate) break
  }
  
  return(gioco)
}
