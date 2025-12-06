# ============================
# 1. CARTE DI TEST
# ============================

crea_carte_test <- function() {
  # Elfo di Llanowar
  elfo <- crea_carta(
    nome          = "Elfo di Llanowar",
    costo         = "{G}",
    tipo          = "Creatura",
    sottotipo     = "Elfo Druido",
    forza         = 1,
    costituzione  = 1,
    colore        = "Verde",
    testo         = "T: Aggiungi {G}."
    # in futuro: abilità di mana come blocco effetti / abilità attivata
  )
  
  # Foresta
  foresta <- crea_carta(
    nome      = "Foresta",
    tipo      = "Terra",
    sottotipo = "Foresta",
    colore    = NULL,
    testo     = "T: Aggiungi {G}."
  )
  
  # Three Visits (per ora solo testo descrittivo)
  three_visits <- crea_carta(
    nome   = "Three Visits",
    costo  = "{G}",
    tipo   = "Stregoneria",
    colore = "Verde",
    testo  = "Cerca nel tuo mazzo una terra base, mettila in gioco tappata, poi mescola il tuo mazzo."
    # in una versione futura: effetti = list( ... search_library_basic_land ... )
  )
  
  # Montagna
  montagna <- crea_carta(
    nome      = "Montagna",
    tipo      = "Terra",
    sottotipo = "Montagna",
    colore    = NULL,
    testo     = "T: Aggiungi {R}."
  )
  
  # Creatura rossa semplice
  goblin <- crea_carta(
    nome         = "Goblin Assaltatore",
    costo        = "{1}{R}",
    tipo         = "Creatura",
    sottotipo    = "Goblin Guerriero",
    forza        = 2,
    costituzione = 2,
    colore       = "Rosso",
    testo        = ""
  )
  
  # Lightning Bolt con DSL effetti
  lightning_bolt <- crea_carta(
    nome    = "Lightning Bolt",
    costo   = "{R}",
    tipo    = "Istantaneo",
    colore  = "Rosso",
    effetti = list(
      list(action = "choose_target", target_filter = "any_target"),
      list(action = "deal_damage", amount = 3, target = "chosen")
    ),
    testo   = "Lightning Bolt infligge 3 danni a qualsiasi bersaglio."
  )
  
  list(
    elfo           = elfo,
    foresta        = foresta,
    three_visits   = three_visits,
    montagna       = montagna,
    goblin         = goblin,
    lightning_bolt = lightning_bolt
  )
}

# ============================
# 2. MAZZI DI TEST
# ============================

crea_mazzo_test_elfi <- function() {
  carte <- crea_carte_test()
  
  # Numeri arbitrari ma piccoli, così la simulazione è veloce
  lista_carte <- c(
    replicate(12, carte$elfo,         simplify = FALSE),
    replicate(10, carte$foresta,      simplify = FALSE),
    replicate(4,  carte$three_visits, simplify = FALSE)
  )
  
  crea_mazzo(lista_carte, nome = "Test MonoG Elfi")
}

crea_mazzo_test_burn <- function() {
  carte <- crea_carte_test()
  
  lista_carte <- c(
    replicate(12, carte$montagna,       simplify = FALSE),
    replicate(8,  carte$goblin,         simplify = FALSE),
    replicate(6,  carte$lightning_bolt, simplify = FALSE)
  )
  
  crea_mazzo(lista_carte, nome = "Test MonoR Burn")
}

# ============================
# 3. GIOCATORI DI TEST
# ============================

crea_giocatori_test <- function() {
  mazzo_elfi <- crea_mazzo_test_elfi()
  mazzo_burn <- crea_mazzo_test_burn()
  
  player1 <- crea_giocatore(
    mazzo      = mazzo_elfi,
    formato    = "standard",
    comandante = NULL,
    nome       = "Pippo - Elfi"
  )
  player1 <- inizializza_giocatore_in_partita(player1, n_carte_iniziali = 7L)
  
  player2 <- crea_giocatore(
    mazzo      = mazzo_burn,
    formato    = "standard",
    comandante = NULL,
    nome       = "Mario - Burn"
  )
  player2 <- inizializza_giocatore_in_partita(player2, n_carte_iniziali = 7L)
  
  list(
    p1 = player1,
    p2 = player2
  )
}

# ============================
# 4. PARTITA DI TEST
# ============================

crea_partita_test <- function(seed = 123, ordine_casuale = FALSE) {
  set.seed(seed)  # per avere un test riproducibile
  giocatori <- crea_giocatori_test()
  
  # Usa la nuova firma di crea_partita (lista di giocatori)
  gioco <- crea_partita(
    giocatori      = list(giocatori$p1, giocatori$p2),
    ordine_casuale = ordine_casuale
  )
  
  gioco
}

# ============================
# 5. LOOP DI PARTITA DI TEST
# ============================

gioca_partita_test <- function(max_turni = 20,
                               seed      = 123,
                               verbose   = TRUE) {
  gioco <- crea_partita_test(seed = seed)
  
  if (verbose) {
    cat("=== Inizio partita di test: Elfi vs Burn ===\n")
    cat("Giocatore 1:", gioco$giocatori[[1]]$nome, "\n")
    cat("Giocatore 2:", gioco$giocatori[[2]]$nome, "\n\n")
  }
  
  for (turno in seq_len(max_turni)) {
    if (verbose) cat("\n----- Turno", turno, "-----\n")
    
    # Assumo che tu abbia una funzione esegui_turno(gioco, verbose = TRUE/FALSE)
    gioco <- esegui_turno(gioco)
    
    # Dopo il turno, per sicurezza, riallineo SBA
    gioco <- controllo_azioni_stato(gioco)
    
    # Controllo se qualcuno ha perso (usando ha_perso)
    stati_persi <- vapply(
      gioco$giocatori,
      function(g) isTRUE(g$ha_perso),
      logical(1)
    )
    
    if (any(stati_persi)) {
      # Giocatori che NON hanno perso
      vivi_idx <- which(!stati_persi)
      
      if (length(vivi_idx) == 1L) {
        vincitore_nome <- gioco$giocatori[[vivi_idx]]$nome
      } else {
        vincitore_nome <- "pareggio / multipli superstiti"
      }
      
      gioco$vincitore <- vincitore_nome
      gioco$stato     <- "conclusa"
      
      if (verbose) {
        cat("\nPartita terminata al turno", turno, "\n")
        cat("Vincitore:", vincitore_nome, "\n")
      }
      return(gioco)
    }
  }
  
  if (verbose) {
    cat("\nRaggiunto max_turni senza vincitore.\n")
    cat("P1 vita:", gioco$giocatori[[1]]$punti_vita,
        " - P2 vita:", gioco$giocatori[[2]]$punti_vita, "\n")
  }
  
  gioco
}
