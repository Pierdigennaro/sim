# Funzione per creare una carta
crea_carta <- function(
    nome, 
    immagine=NULL,
    costo=NULL, 
    tipo, 
    #    supertipo=NULL,
    sottotipo=NULL,
    leggendaria=FALSE, 
    forza=NULL, 
    costituzione=NULL,
    abilita_statica=NULL, 
    abilita_attivata=NULL,
    abilita_di_mana=NULL, 
    tappata=FALSE, 
    owner="player1", 
    controller="player1", 
    zona="mazzo",
    testo = NULL,
    colore = NULL,
    counter = list(),
    meccanica = list(),
    phase_out = FALSE) {
  if (is.null(testo)) testo <- ""
  if (is.null(colore)) colore <- "Incolore"
  return(list(
    nome = nome,
    immagine=immagine,
    costo = costo,
    tipo = tipo,
    sottotipo = sottotipo,
    forza = forza,
    leggendaria=leggendaria,
    costituzione = costituzione,
    abilita_statica = abilita_statica,
    abilita_attivata = abilita_attivata,
    abilita_di_mana = abilita_di_mana,
    tappata=tappata,
    puo_stappare=TRUE,
    owner = owner,
    controller = controller,
    zona = zona,
    testo = testo,
    colore = colore,
    counter = counter,
    id = paste0(nome, "_", as.integer(runif(1, 1, 1e6))),
    meccanica = meccanica,
    phase_out = phase_out
  ))}

stampa_carta <- function(carta) {
  cat("───────────────────────────────\n")
  # Nome + Leggendaria
  nome_line <- carta$nome
  cat(nome_line, "\n")
  
  # Tipo e sottotipo
  tipo_line <- carta$tipo
  if (!is.null(carta$leggendaria) && carta$leggendaria == TRUE) {
    tipo_line <- paste(tipo_line, "Legendaria")
  }
  if (!is.null(carta$sottotipo)) {
    tipo_line <- paste0(tipo_line, " — ", carta$sottotipo)
  }
  cat(tipo_line, "\n")
  
  # Costo di mana
  if (!is.null(carta$costo) && carta$costo != "") {
    cat("Costo:", carta$costo, "\n")
  }
  
  # Colore
  if (!is.null(carta$colore)) {
    cat("Colore:", carta$colore, "\n")
  }
  
  # Abilità e testo
  if (!is.null(carta$testo) && carta$testo != "") {
    cat("Testo:\n", carta$testo, "\n")
  }
  if (!is.null(carta$abilita_statica)) {
    cat("Abilità statica:", carta$abilita_statica, "\n")
  }
  if (!is.null(carta$abilita_attivata)) {
    cat("Abilità attivata:", carta$abilita_attivata, "\n")
  }
  if (!is.null(carta$abilita_di_mana)) {
    cat("Abilità di mana:", carta$abilita_di_mana, "\n")
  }
  # Creature: forza/costituzione
  if (!is.null(carta$forza) && !is.null(carta$costituzione)) {
    cat("Forza/Costituzione:", carta$forza, "/", carta$costituzione, "\n")
  }
  # Stato
  cat("Zona:", carta$zona, "\n")
  cat("Tappata:", ifelse(carta$tappata, "Sì", "No"), "\n")
  # Proprietà
  cat("Owner:", carta$owner, " | Controller:", carta$controller, "\n")
  # ID (utile per debugging)
  cat("ID:", carta$id, "\n")
  cat("───────────────────────────────\n")
}
