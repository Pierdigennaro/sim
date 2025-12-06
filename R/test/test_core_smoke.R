

# Uso le carte di test già definite in test_1.R
# Se vuoi renderlo autonomo, puoi copiare qui anche crea_carte_test / crea_mazzo_test_*.


# ----------------------------
# Test 1: oggetti base
# ----------------------------
test_oggetti_base <- function() {
  if (!exists("crea_carte_test")) {
    stop("crea_carte_test non è definita (carica test_1.R o spostala in un file comune).")
  }
  
  carte <- crea_carte_test()
  
  # Mazzi di test
  mazzo_elfi <- crea_mazzo_test_elfi()
  mazzo_burn <- crea_mazzo_test_burn()
  
  # 1) Creo i giocatori (solo struttura)
  p1 <- crea_giocatore(mazzo = mazzo_elfi, formato = "standard", nome = "Tester Elfi")
  p2 <- crea_giocatore(mazzo = mazzo_burn, formato = "standard", nome = "Tester Burn")
  
  # 2) Inizializzo la partita per loro (mischia + mano iniziale)
  p1 <- inizializza_giocatore_in_partita(p1, n_carte_iniziali = 7L)
  p2 <- inizializza_giocatore_in_partita(p2, n_carte_iniziali = 7L)
  
  # Ora ha senso aspettarsi le 7 carte in mano
  stopifnot(length(p1$mano) == 7L)
  stopifnot(length(p2$mano) == 7L)
  
  # 3) Creo la partita a partire dai giocatori già inizializzati
  gioco <- crea_partita_2_4(p1, p2, ordine_casuale = FALSE)
  
  stopifnot(length(gioco$giocatori) == 2L)
  stopifnot(gioco$turno_corrente == 1L)
  stopifnot(gioco$indice_giocatore_attivo == 1L)
  stopifnot(identical(gioco$campo, list()))
  stopifnot(identical(gioco$pila, list()))
  
  cat("[OK] test_oggetti_base\n")
  invisible(gioco)
}


# ----------------------------
# Test 2: SBA – creatura con danno letale
# ----------------------------
test_sba_danno_letale <- function() {
  carte <- crea_carte_test()
  
  # Mazzo vuoto giusto per avere un oggetto mazzo valido
  mazzo_vuoto <- crea_mazzo(list(), nome = "Mazzo vuoto")
  p1 <- crea_giocatore(mazzo = mazzo_vuoto, formato = "standard", nome = "P1")
  p2 <- crea_giocatore(mazzo = mazzo_vuoto, formato = "standard", nome = "P2")
  
  gioco <- crea_partita_2_4(p1, p2, ordine_casuale = FALSE)
  
  # Metto un Elfo sul campo di P1 e gli assegno danno letale
  elfo <- carte$elfo
  elfo$owner      <- gioco$giocatori[[1]]$id
  elfo$controller <- gioco$giocatori[[1]]$id
  elfo$zona       <- "campo"
  elfo$danno      <- elfo$costituzione  # danno letale
  
  campo <- get_zona(gioco, "campo")
  campo[[length(campo) + 1L]] <- elfo
  gioco <- set_zona(gioco, "campo", campo)
  
  # Applico SBA
  gioco <- controllo_azioni_stato(gioco)
  
  campo2 <- get_zona(gioco, "campo")
  cim1   <- get_zona(gioco, "cimitero", 1)
  
  stopifnot(length(campo2) == 0L)         # nessuna creatura in campo
  stopifnot(length(cim1)   == 1L)         # elfo nel cimitero
  stopifnot(cim1[[1]]$nome == "Elfo di Llanowar")
  
  cat("[OK] test_sba_danno_letale\n")
  invisible(gioco)
}


# ----------------------------
# Test 3: Stack – Lightning Bolt su giocatore
# ----------------------------
test_stack_lightning_bolt_su_giocatore <- function() {
  carte <- crea_carte_test()
  lb    <- carte$lightning_bolt
  
  mazzo_vuoto <- crea_mazzo(list(), nome = "Mazzo vuoto")
  p1 <- crea_giocatore(mazzo = mazzo_vuoto, formato = "standard", nome = "P1")
  p2 <- crea_giocatore(mazzo = mazzo_vuoto, formato = "standard", nome = "P2")
  
  gioco <- crea_partita_2_4(p1, p2, ordine_casuale = FALSE)
  
  # Do a P1 una Lightning Bolt in mano
  g1 <- gioco$giocatori[[1]]
  g1$mano <- c(g1$mano, list(lb))
  gioco$giocatori[[1]] <- g1
  
  vita_iniziale <- gioco$giocatori[[2]]$punti_vita
  target_id     <- gioco$giocatori[[2]]$id
  
  # Lancio la Lightning Bolt dall'ultima posizione della mano
  idx_carta <- length(gioco$giocatori[[1]]$mano)
  gioco <- lancia_magia_dalla_mano(
    gioco            = gioco,
    indice_giocatore = 1,
    indice_carta_mano = idx_carta,
    targets          = list(target_id)  # imposto direttamente il bersaglio
  )
  
  # Risolvo la pila
  gioco <- risolvi_pila(gioco)
  
  vita_finale <- gioco$giocatori[[2]]$punti_vita
  
  stopifnot(vita_iniziale - vita_finale == 3L)
  stopifnot(length(gioco$pila) == 0L)
  
  cat("[OK] test_stack_lightning_bolt_su_giocatore\n")
  invisible(gioco)
}


# ----------------------------
# Runner
# ----------------------------
run_core_smoke_tests <- function() {
  gioco1 <- test_oggetti_base()
  gioco2 <- test_sba_danno_letale()
  gioco3 <- test_stack_lightning_bolt_su_giocatore()
  cat("\nTUTTI I TEST DI BASE SONO PASSATI.\n")
  
  invisible(list(gioco1 = gioco1, gioco2 = gioco2, gioco3 = gioco3))
}
