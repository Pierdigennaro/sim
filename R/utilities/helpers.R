`%||%` <- function(x, y) if (is.null(x)) y else x

is_instant_or_sorcery <- function(carta) {
  tipo_line <- tolower(carta$tipo %||% "")
  grepl("istantaneo", tipo_line) ||
    grepl("instant", tipo_line)   ||
    grepl("stregoneria", tipo_line) ||
    grepl("sorcery", tipo_line)
}

is_permanent_type <- function(carta) {
  tipo_line <- tolower(carta$tipo %||% "")
  if (tipo_line == "") return(FALSE)
  
  # Se è istantaneo o stregoneria → NON è permanente
  if (is_instant_or_sorcery(carta)) return(FALSE)
  
  # Tutto il resto lo consideriamo "permanente" (Creatura, Artefatto, Incantesimo, Terra, Planeswalker, ecc.)
  TRUE
}

trova_indice_giocatore <- function(gioco, ref) {
  # ref può essere indice, id, nome
  if (is.null(ref)) return(NULL)
  if (is.numeric(ref)) return(as.integer(ref))
  
  # prova a confrontare con id
  idx <- which(vapply(
    gioco$giocatori,
    function(g) g$id %||% NA_character_,
    character(1)
  ) == ref)
  if (length(idx) == 1L) return(idx)
  
  # prova con il nome
  idx <- which(vapply(
    gioco$giocatori,
    function(g) g$nome %||% NA_character_,
    character(1)
  ) == ref)
  if (length(idx) == 1L) return(idx)
  
  stop("trova_indice_giocatore: nessun giocatore trovato per ref = ", ref)
}

# Restituisce il giocatore attivo
get_giocatore_attivo <- function(gioco) {
  gioco$giocatori[[gioco$indice_giocatore_attivo]]
}

get_giocatori_non_attivi <- function(gioco) {
  idx <- seq_along(gioco$giocatori)
  gioco$giocatori[idx != gioco$indice_giocatore_attivo]
}

imposta_prossimo_giocatore_attivo <- function(gioco) {
  n <- length(gioco$giocatori)
  gioco$indice_giocatore_attivo <- (gioco$indice_giocatore_attivo %% n) + 1L
  return(gioco)
}

incrementa_turno <- function(gioco) {
  gioco$turno_corrente <- gioco$turno_corrente + 1L
  # qui potresti anche shiftare magie_lanciate_turno_* fra corrente e precedente
  return(gioco)
}

aggiungi_carta <- function(giocatore, carta, posizione="in_fondo") {
  if (posizione=="in_fondo") {
    giocatore$mazzo$carte <- append(giocatore$mazzo$carte, carta)
  }else if(posizione=="in_cima") {
    giocatore$mazzo$carte <- append(carta,giocatore$mazzo$carte)
  }
  return(giocatore)
}


gestisci_inneschi_pescata <- function(gioco, indice_giocatore, carta_pescata) {
  giocatore <- gioco$giocatori[[indice_giocatore]]
  
  for (permanente in giocatore$campo) {
    if (!is.null(permanente$abilita_innescate)) {
      for (abilita in permanente$abilita_innescate) {
        if (abilita$tipo_evento == "pesca_carta") {
          gioco$pila <- append(gioco$pila, list(list(
            fonte = permanente$nome,
            abilita = abilita,
            controllore = giocatore$nome
          )))
          messaggio <- paste("Si innesca l'abilità di", permanente$nome, ":", abilita$descrizione)
          gioco <- aggiungi_log(gioco, messaggio, tipo = "innesco")
        }
      }
    }
  }
  
  return(gioco)
}

pesca_carta <- function(gioco, indice_giocatore, n = 1, motivo = "generico") {
  giocatore <- gioco$giocatori[[indice_giocatore]]
  
  # eventi di pescata che comunicheremo al sistema di trigger
  eventi_pescata <- list()
  
  for (i in seq_len(n)) {
    risultato <- pesca_carta_mazzo(giocatore$mazzo)
    giocatore$mazzo <- risultato$mazzo
    
    if (is.null(risultato$carta)) {
      # Tentativo di pescare da grimorio vuoto
      # → regole 121.4 e 704.5b: perderà alla prossima verifica SBA.
      gioco <- aggiungi_log(
        gioco,
        paste(giocatore$nome, "prova a pescare ma il grimorio è vuoto."),
        tipo = "warning"
      )
      
      # Flag sul giocatore: ha tentato di pescare da grimorio vuoto.
      # Sarà controllo_azioni_stato() a trasformarlo in "perdi la partita".
      giocatore$ha_tentato_pescata_grimorio_vuoto <- TRUE
      
      # Stoppo ulteriori pescate richieste da questa chiamata
      break
    } else {
      # Spostamento standard: grimorio -> mano
      giocatore$mano <- append(giocatore$mano, list(risultato$carta))
      
      gioco <- aggiungi_log(
        gioco,
        paste(giocatore$nome, "pesca", risultato$carta$nome),
        tipo = "azione"
      )
      
      # Registro l'evento di pescata, ma NON risolvo ancora i trigger
      eventi_pescata[[length(eventi_pescata) + 1]] <- list(
        tipo      = "pescata",
        giocatore = indice_giocatore,
        carta     = risultato$carta,
        motivo    = motivo
      )
    }
  }
  
  # Salvo il giocatore aggiornato
  gioco$giocatori[[indice_giocatore]] <- giocatore
  
  # Accodo gli eventi alla coda eventi globale del gioco
  if (is.null(gioco$eventi_in_coda)) {
    gioco$eventi_in_coda <- eventi_pescata
  } else if (length(eventi_pescata) > 0) {
    gioco$eventi_in_coda <- c(gioco$eventi_in_coda, eventi_pescata)
  }
  
  return(gioco)
}
