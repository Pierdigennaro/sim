crea_giocatore <- function(formato="commander",mazzo,comandante,nome=NA) {
  mazzo_mischiato <- sample(mazzo, replace = FALSE)
  mano_iniziale <- mazzo_mischiato[1:7]
  if (is.na(nome)) {nome <- paste0("Giocatore_", as.integer(runif(1, 1, 1e6)))}
  player <- list(
    punti_vita = ifelse(formato=="commander",40,20),
    comandante = comandante,
    comandante_tassa = 0,
    comandante_posizione = "zona_comando",
    mazzo = mazzo_mischiato,
    mano_iniziale= mano_iniziale,
    mano = mano_iniziale,
    cimitero = list(),
    esilio = list(),
    turni_iniziati = 0,
    campo = list(),
    terre_giocate_turno = 0,       # per rispettare il limite 1 terra/turno
    terre_in_campo = list(),
    mana_pool = list(),
    nome = nome,
    id = paste0("player_", as.integer(runif(1, 1, 1e6)))
  )
  return(player)
}

stampa_giocatore <- function(giocatore) {
  cat("───────────────────────────────\n")
  cat("Nome: ", giocatore$nome, "\n", sep = "")
  cat("ID: ", giocatore$id, "\n", sep = "")
  cat("Punti vita: ", giocatore$punti_vita, "\n", sep = "")
  cat("Formato: ", ifelse(giocatore$punti_vita == 40, "Commander", "Standard"), "\n", sep = "")
  
  cat("Comandante: ", ifelse(!is.null(giocatore$comandante),
                             giocatore$comandante$nome,
                             "Nessuno"), "\n", sep = "")
  
  cat("Carte in mano: ", length(giocatore$mano), "\n", sep = "")
  cat("Carte nel mazzo: ", length(giocatore$mazzo), "\n", sep = "")
  cat("Carte nel cimitero: ", length(giocatore$cimitero), "\n", sep = "")
  cat("Carte in esilio: ", length(giocatore$esilio), "\n", sep = "")
  cat("Carte sul campo: ", length(giocatore$campo), "\n", sep = "")
  cat("Terre giocate questo turno: ", giocatore$terre_giocate_turno, "\n", sep = "")
  #cat("Mana pool: ", ifelse(length(giocatore$mana_pool) == 0, "vuoto", paste(giocatore$mana_pool, collapse = ", ")), "\n", sep = "")
  cat("───────────────────────────────\n")
  
  if (length(giocatore$mano) > 0) {
    cat("Carte in mano:\n")
    for (carta in giocatore$mano) {
      cat("  - ", carta$nome, "\n", sep = "")
    }
    cat("───────────────────────────────\n")
  }
}

