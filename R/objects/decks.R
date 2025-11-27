crea_mazzo <- function(lista_carte, nome = "Mazzo senza nome") {
  list(
    nome = nome,
    carte = lista_carte,
    n_carte = length(lista_carte),
    id = paste0("deck_", as.integer(runif(1, 1, 1e6)))
  )
}

mostra_mazzo <- function(giocatore, n = 5) {
  cat("Prime", n, "carte del mazzo di",giocatore$nome,":",giocatore$mazzo$nome, ":\n")
  for (i in seq_len(min(n, length(giocatore$mazzo$carte)))) {
    cat(" -", giocatore$mazzo$carte[[i]]$nome, "\n")
  }
}
