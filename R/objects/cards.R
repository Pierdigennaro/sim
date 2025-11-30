# Costruttore "v2" per una carta
crea_carta <- function(
    # --- dati statici della carta (come stampata) ---
  nome,
  immagine      = NULL,
  costo         = NULL,          # es: "1G", "3UU"
  tipo,                           # es: "Creatura", "Istantaneo", "Terra"
  sottotipo     = NULL,          # es: "Elfo Druido" oppure c("Elfo", "Druido")
  leggendaria   = FALSE,         # TRUE se è leggendaria
  forza         = NULL,          # solo per creature
  costituzione  = NULL,          # solo per creature
  abilita_statica   = NULL,      # testo o struttura DSL
  abilita_attivata  = NULL,
  abilita_di_mana   = NULL,
  testo         = NULL,          # rules text completo
  colore        = NULL,          # character vector, es: c("Verde"), c("Blu","Nero")
  meccanica     = list(),        # keyword tipo "Volare", "Travolgere" (strutturata più avanti)
  effetti       = NULL,          # lista di "blocchi di effetto" per il motore
  
  # --- stato di gioco (permanente/oggetto in partita) ---
  tappata       = FALSE,
  puo_stappare  = TRUE,
  owner         = NULL,          # id giocatore proprietario
  controller    = owner,         # per default chi controlla è il proprietario
  zona          = "mazzo",       # "mazzo", "mano", "campo", "cimitero", "esilio", "pila"
  counter       = list(),        # es: list("+1/+1" = 2, "veleno" = 1)
  phase_out     = FALSE,         # phasing
  token         = FALSE,         # TRUE se è pedina
  copia         = FALSE,         # TRUE se è una copia
  id            = NULL           # opzionale: se NULL, generato automaticamente
) {
  # default "sicuri"
  if (is.null(testo))  testo  <- ""
  if (is.null(colore)) colore <- character(0)   # pronto per multicolore
  
  # controllore = owner se non specificato
  if (is.null(controller)) controller <- owner
  
  # genera un id unico se non fornito
  if (is.null(id)) {
    # sostituisce spazi nel nome giusto per leggibilità
    base <- gsub("\\s+", "_", nome)
    id   <- paste0(base, "_", as.integer(runif(1, 1, 1e9)))
  }
  
  # normalizzazione semplice di tipo/sottotipo: accetto sia stringa unica che vettore
  if (length(tipo) > 1) {
    # se ti arriva un vettore, uniscilo con spazio tipo "Creatura Artefatto"
    tipo <- paste(tipo, collapse = " ")
  }
  if (length(sottotipo) > 1) {
    sottotipo <- paste(sottotipo, collapse = " ")
  }
  
  # ritorno l'oggetto carta
  return(list(
    # --- dati statici ---
    nome        = nome,
    immagine    = immagine,
    costo       = costo,
    tipo        = tipo,
    sottotipo   = sottotipo,
    
    # doppio campo per compatibilità con codice già scritto
    leggendaria = isTRUE(leggendaria),
    leggendario = isTRUE(leggendaria),
    
    forza       = forza,
    costituzione= costituzione,
    abilita_statica  = abilita_statica,
    abilita_attivata = abilita_attivata,
    abilita_di_mana  = abilita_di_mana,
    testo       = testo,
    colore      = colore,
    meccanica   = meccanica,
    effetti     = effetti,
    
    # --- stato di gioco ---
    tappata     = tappata,
    puo_stappare= puo_stappare,
    owner       = owner,
    controller  = controller,
    zona        = zona,
    counter     = counter,
    phase_out   = phase_out,
    token       = token,
    copia       = copia,
    
    # campo usato dalle SBA per il danno accumulato (default 0)
    danno       = 0L,
    
    # id unico
    id          = id
  ))
}


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
