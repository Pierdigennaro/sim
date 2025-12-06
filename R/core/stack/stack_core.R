crea_stack_object <- function(
    tipo,                 # "spell" | "activated" | "triggered"
    carta = NULL,         # lista carta (può essere NULL per abilità “pure”)
    sorgente_id = NULL,   # id permanente o carta che lo genera
    controller_id,        # id del controllore (giocatore)
    targets = list(),     # lista di id bersagli scelti
    effetti = list(),     # lista DSL effetti
    costo_pagato = NULL,
    mode = NULL,
    priority_owner_hint = NULL
) {
  list(
    tipo              = tipo,
    carta             = carta,
    sorgente_id       = sorgente_id %||% if (!is.null(carta)) carta$id else NULL,
    controller_id     = controller_id,
    targets           = targets,
    costo_pagato      = costo_pagato,
    effetti           = effetti,
    priority_owner_hint = priority_owner_hint,
    mode              = mode,
    id                = paste0("stack_", as.integer(runif(1, 1, 1e6))),
    timestamp         = Sys.time()
  )
}

trova_bersagli_validi <- function(gioco, filtro, controller_id) {
  # Ritorna una lista di descrittori:
  # list(id = ..., kind = "player"|"permanent"|"card", zone = ..., player_index = ..., card = ...)
  
  res <- list()
  
  if (filtro == "any_target") {
    # Giocatori
    for (i in seq_along(gioco$giocatori)) {
      g <- gioco$giocatori[[i]]
      res[[length(res) + 1L]] <- list(
        id           = g$id,   # usa sempre id del giocatore
        kind         = "player",
        zone         = "player",
        player_index = i
      )
    }
    
    # Permanenti sul campo
    campo <- get_zona(gioco, "campo")
    if (!is.null(campo) && length(campo) > 0L) {
      for (idx in seq_along(campo)) {
        c <- campo[[idx]]
        res[[length(res) + 1L]] <- list(
          id             = c$id,
          kind           = "permanent",
          zone           = "campo",
          index          = idx,
          card           = c,
          owner_ref      = c$owner,
          controller_ref = c$controller
        )
      }
    }
    
  } else if (filtro == "creature_in_any_graveyard") {
    # Qualsiasi creatura in qualsiasi cimitero
    for (i in seq_along(gioco$giocatori)) {
      cim <- get_zona(gioco, "cimitero", i)
      if (length(cim) == 0L) next
      
      for (idx in seq_along(cim)) {
        c <- cim[[idx]]
        tipo_line <- tolower(c$tipo %||% "")
        if (grepl("creatura", tipo_line)) {
          res[[length(res) + 1L]] <- list(
            id             = c$id,
            kind           = "card",
            zone           = "cimitero",
            player_index   = i,
            index          = idx,
            card           = c,
            owner_ref      = c$owner %||% gioco$giocatori[[i]]$id
          )
        }
      }
    }
  }
  
  # altri filtri in futuro: "creature_you_control", "opponent_creature", ecc.
  res
}

trova_oggetto_per_id <- function(gioco, id) {
  # 1) Giocatori
  for (i in seq_along(gioco$giocatori)) {
    g <- gioco$giocatori[[i]]
    if (!is.null(g$id) && g$id == id) {
      return(list(
        kind         = "player",
        zone         = "player",
        player_index = i,
        id           = id
      ))
    }
  }
  
  # 2) Oggetti sulla pila (stack object)
  if (!is.null(gioco$pila) && length(gioco$pila) > 0L) {
    for (i in seq_along(gioco$pila)) {
      obj <- gioco$pila[[i]]
      if (obj$id == id) {
        return(list(
          kind    = "stack_object",
          zone    = "pila",
          index   = i,
          object  = obj,
          id      = id
        ))
      }
    }
  }
  
  # 3) Permanenti sul campo
  campo <- get_zona(gioco, "campo")
  if (!is.null(campo) && length(campo) > 0L) {
    for (i in seq_along(campo)) {
      c <- campo[[i]]
      if (!is.null(c$id) && c$id == id) {
        return(list(
          kind           = "permanent",
          zone           = "campo",
          index          = i,
          card           = c,
          owner_ref      = c$owner,
          controller_ref = c$controller,
          id             = id
        ))
      }
    }
  }
  
  # 4) Zone personali: mano, cimitero, mazzo
  for (g_idx in seq_along(gioco$giocatori)) {
    for (zona in c("mano", "cimitero", "mazzo")) {
      lista <- get_zona(gioco, zona, g_idx)
      if (is.null(lista) || length(lista) == 0L) next
      
      for (i in seq_along(lista)) {
        c <- lista[[i]]
        if (!is.null(c$id) && c$id == id) {
          return(list(
            kind         = "card",
            zone         = zona,
            player_index = g_idx,
            index        = i,
            card         = c,
            owner_ref    = c$owner %||% gioco$giocatori[[g_idx]]$id,
            id           = id
          ))
        }
      }
    }
  }
  
  # non trovato
  return(NULL)
}

# Alias per compatibilità col vecchio nome (tippo 'objetto')
trova_objetto_per_id <- trova_oggetto_per_id

sposta_oggetto_per_id <- function(gioco, target_id, a_zona, a_giocatore = NULL) {
  a_zona <- tolower(a_zona)
  
  loc <- trova_oggetto_per_id(gioco, target_id)
  if (is.null(loc)) {
    gioco <- aggiungi_log(
      gioco,
      paste0("sposta_oggetto_per_id: oggetto con id ", target_id, " non trovato.")
    )
    return(gioco)
  }
  
  # I giocatori non "si spostano" di zona
  if (loc$kind == "player") {
    gioco <- aggiungi_log(
      gioco,
      paste0("sposta_oggetto_per_id: impossibile spostare un giocatore (id ", target_id, ").")
    )
    return(gioco)
  }
  
  # Determino indice giocatore sorgente (se rilevante)
  da_giocatore <- if (!is.null(loc$player_index)) loc$player_index else NULL
  
  # Determino giocatore di destinazione di default (owner_ref), se non specificato
  if (is.null(a_giocatore) && !is.null(loc$owner_ref)) {
    a_giocatore <- loc$owner_ref
  }
  
  gioco <- sposta_carta(
    gioco,
    da_zona      = loc$zone,
    a_zona       = a_zona,
    indice_carta = loc$index,
    da_giocatore = da_giocatore,
    a_giocatore  = a_giocatore
  )
  
  return(gioco)
}

action_choose_target <- function(gioco, eff, stack_obj) {
  # Se i target sono già stati scelti (da UI/AI), non fare nulla
  if (!is.null(stack_obj$targets) && length(stack_obj$targets) > 0L) {
    return(gioco)
  }
  
  candidates <- trova_bersagli_validi(
    gioco, 
    eff$target_filter, 
    stack_obj$controller_id
  )
  
  if (length(candidates) == 0L) {
    # Nessun bersaglio disponibile: lasciamo targets vuoto
    # Sarà validate_targets a far "fizzle" la spell/abilità
    return(gioco)
  }
  
  # TODO: sostituire con AI / input giocatore
  scelta_id <- candidates[[1]]$id
  stack_obj$targets <- list(scelta_id)
  
  gioco <- aggiorna_stack_object_in_gioco(gioco, stack_obj)
  return(gioco)
}

action_deal_damage <- function(gioco, eff, stack_obj) {
  target_id <- if (identical(eff$target, "chosen")) {
    if (length(stack_obj$targets) == 0L) return(gioco)
    stack_obj$targets[[1]]
  } else {
    eff$target
  }
  
  if (is.null(target_id)) return(gioco)
  
  loc <- trova_oggetto_per_id(gioco, target_id)
  if (is.null(loc)) {
    gioco <- aggiungi_log(
      gioco,
      paste0("Bersaglio non trovato (", target_id, "), danno annullato."),
      tipo = "warn"
    )
    return(gioco)
  }
  
  amount <- eff$amount %||% 0L
  
  if (loc$kind == "player") {
    gioco <- applica_danno_a_giocatore(gioco, loc$player_index, amount)
    
  } else if (loc$kind %in% c("permanent", "card") &&
             loc$zone == "campo") {
    # assumiamo che applica_danno_a_creatura sappia usare loc$card e loc$index
    gioco <- applica_danno_a_creatura(gioco, loc, amount)
  }
  
  return(gioco)
}

action_move_zone <- function(gioco, eff, stack_obj) {
  # eff$from (opzionale), eff$to (es. "cimitero"), eff$target (id o "chosen")
  target_id <- if (identical(eff$target, "chosen")) {
    if (length(stack_obj$targets) == 0L) return(gioco)
    stack_obj$targets[[1]]
  } else {
    eff$target
  }
  
  if (is.null(target_id)) return(gioco)
  
  to_zone      <- eff$to
  to_controller <- eff$to_controller %||% NULL  # opzionale
  
  gioco <- sposta_oggetto_per_id(gioco, target_id, to_zone, to_controller)
  return(gioco)
}

is_target_still_legal <- function(target_id, gioco) {
  loc <- trova_oggetto_per_id(gioco, target_id)
  if (is.null(loc)) return(FALSE)
  # TODO: controllare shroud, hexproof, cambi di tipo ecc.
  TRUE
}

resolve_top_of_stack <- function(gioco) {
  popped <- pop_pila(gioco)
  gioco <- popped$gioco
  obj   <- popped$oggetto
  
  if (is.null(obj)) return(gioco)  # pila vuota
  
  nome <- if (!is.null(obj$carta)) obj$carta$nome else obj$tipo
  
  # 1) Validazione bersagli (608.2b)
  validation <- validate_targets(obj, gioco)
  
  if (validation$all_illegal) {
    gioco <- aggiungi_log(
      gioco,
      paste0(
        "La spell/abilità ", nome, " (ID ", obj$id,
        ") non ha più alcun bersaglio legale → neutralizzata."
      )
    )
    # CR: spell/ability è neutralizzata e va comunque nella zona appropriata
    gioco <- gestisci_post_risoluzione_zona(gioco, obj, countered = TRUE)
    
    # Azioni basate sullo stato dopo la “non-risoluzione”
    gioco <- controllo_azioni_stato(gioco)
    # TODO: inneschi post-SBA, se usi eventi
    return(gioco)
  }
  
  # 2) Applica gli effetti DSL
  gioco <- applica_effetti(gioco, obj$effetti, obj)
  
  # 3) Gestione zona sorgente (spell che va al cimitero, abilità che sparisce)
  gioco <- gestisci_post_risoluzione_zona(gioco, obj, countered = FALSE)
  
  # 4) Azioni basate sullo stato post-risoluzione
  gioco <- controllo_azioni_stato(gioco)
  # TODO: gestisci_inneschi_da_eventi(gioco) quando avrai il sistema di eventi
  
  return(gioco)
}

risolvi_pila <- function(gioco) {
  while (!is.null(gioco$pila) && length(gioco$pila) > 0L) {
    gioco <- resolve_top_of_stack(gioco)
    # CR: tra una risoluzione e l'altra SBA + trigger sono già stati gestiti dentro resolve_top_of_stack
  }
  gioco
}

gestisci_post_risoluzione_zona <- function(gioco, stack_obj, countered = FALSE) {
  # Solo magie vere (non abilità) con una carta associata
  if (stack_obj$tipo != "spell" || is.null(stack_obj$carta)) {
    return(gioco)
  }
  
  carta <- stack_obj$carta
  
  # Chi è il proprietario? (ref può essere id o nome, come abbiamo deciso)
  owner_ref <- carta$owner %||% carta$proprietario
  owner_idx <- trova_indice_giocatore(gioco, owner_ref)
  
  if (is.null(owner_idx)) {
    # situazione patologica: non trovo il proprietario
    gioco <- aggiungi_log(
      gioco,
      paste0("gestisci_post_risoluzione_zona: impossibile trovare il proprietario di ", carta$nome),
      tipo = "warn"
    )
    return(gioco)
  }
  
  # Ist/Str: vanno sempre nel cimitero del proprietario
  if (is_instant_or_sorcery(carta)) {
    carta$zona <- "cimitero"
    
    cim <- get_zona(gioco, "cimitero", owner_idx)
    cim[[length(cim) + 1L]] <- carta
    gioco <- set_zona(gioco, "cimitero", cim, owner_idx)
    
    gioco <- aggiungi_log(
      gioco,
      paste0(carta$nome, " viene messa nel cimitero di ", gioco$giocatori[[owner_idx]]$nome, ".")
    )
    
    return(gioco)
  }
  
  # Da qui in giù: magie permanenti (creature, artefatti, planeswalker, enchantment permanenti, terre speciali, ecc.)
  
  if (countered) {
    # Magia permanente neutralizzata → cimitero del proprietario
    carta$zona <- "cimitero"
    cim <- get_zona(gioco, "cimitero", owner_idx)
    cim[[length(cim) + 1L]] <- carta
    gioco <- set_zona(gioco, "cimitero", cim, owner_idx)
    
    gioco <- aggiungi_log(
      gioco,
      paste0("La magia permanente ", carta$nome,
             " viene neutralizzata e messa nel cimitero di ",
             gioco$giocatori[[owner_idx]]$nome, ".")
    )
    
    return(gioco)
  } else {
    # Magia permanente risolta → entra come permanente sul campo di battaglia
    carta$zona <- "campo"
    
    # Decidiamo il controllore: chi ha lanciato la magia
    ctrl_ref  <- stack_obj$controller_id %||% owner_ref
    ctrl_idx  <- trova_indice_giocatore(gioco, ctrl_ref)
    
    if (!is.null(ctrl_idx)) {
      carta$controller <- gioco$giocatori[[ctrl_idx]]$id %||% gioco$giocatori[[ctrl_idx]]$nome
    }
    
    campo <- get_zona(gioco, "campo")
    campo[[length(campo) + 1L]] <- carta
    gioco <- set_zona(gioco, "campo", campo)
    
    gioco <- aggiungi_log(
      gioco,
      paste0("La magia ", carta$nome,
             " si risolve ed entra sul campo di battaglia controllata da ",
             ctrl_ref, ".")
    )
    
    return(gioco)
  }
}

lancia_magia_dalla_mano <- function(gioco,
                                    indice_giocatore,
                                    indice_carta_mano,
                                    targets = list(),
                                    effetti_override = NULL) {
  giocatore <- gioco$giocatori[[indice_giocatore]]
  
  if (indice_carta_mano < 1L || indice_carta_mano > length(giocatore$mano)) {
    stop("lancia_magia_dalla_mano: indice_carta_mano fuori range.")
  }
  
  carta <- giocatore$mano[[indice_carta_mano]]
  tipo_line <- tolower(carta$tipo %||% "")
  
  # Terre non sono magie: vanno giocate con un'altra funzione
  if (grepl("terra", tipo_line) || grepl("land", tipo_line)) {
    stop("lancia_magia_dalla_mano: questa funzione non gioca terre. Usa una funzione dedicata, es. gioca_terra().")
  }
  
  # TODO: qui in futuro controlli di costo di mana, timing, ecc.
  
  # Rimuovi la carta dalla mano del giocatore
  giocatore$mano[[indice_carta_mano]] <- NULL
  gioco$giocatori[[indice_giocatore]] <- giocatore
  
  # Aggiorna zona e controller nella carta
  carta$zona       <- "pila"
  carta$controller <- giocatore$id %||% giocatore$nome
  
  # Effetti DSL: se non passati esplicitamente, usa quelli definiti nella carta
  effetti <- effetti_override %||% carta$effetti
  
  # Crea l'oggetto di pila
  stack_obj <- crea_stack_object(
    tipo           = "spell",
    carta          = carta,
    controller_id  = carta$controller,
    targets        = targets,
    effetti        = effetti
  )
  
  # Metti sulla pila
  gioco <- metti_su_pila(gioco, stack_obj)
  
  return(gioco)
}

applica_effetti <- function(gioco, effetti, stack_obj) {
  for (eff in effetti) {
    # Permetti ad una action di fallire/essere annullata:
    # per es. action_choose_target può impostare eff$skipped <- TRUE
    if (!is.null(eff$skipped) && eff$skipped) next
    gioco <- applica_singolo_effetto(gioco, eff, stack_obj)
  }
  return(gioco)
}

applica_singolo_effetto <- function(gioco, eff, stack_obj) {
  # eff: lista che contiene almeno action (string)
  action <- eff$action
  
  switch(action,
         
         "choose_target" = {
           gioco <- action_choose_target(gioco, eff, stack_obj)
         },
         
         "deal_damage" = {
           gioco <- action_deal_damage(gioco, eff, stack_obj)
         },
         
         "draw_cards" = {
           gioco <- action_draw_cards(gioco, eff, stack_obj)
         },
         
         "create_token" = {
           gioco <- action_create_token(gioco, eff, stack_obj)
         },
         
         "move_zone" = {
           gioco <- action_move_zone(gioco, eff, stack_obj)
         },
         
         "return_to_battlefield_under_owner_control" = {
           gioco <- action_return_to_battlefield(gioco, eff, stack_obj)
         },
         
         # aggiungi qui nuovi handler...
         
         stop(paste0("Action non gestita: ", action))
  )
  
  return(gioco)
}

# Aggiorna oggetto sulla pila (sostituisce lo stack_obj con stesso id)
aggiorna_stack_object_in_gioco <- function(gioco, new_obj) {
  if (length(gioco$pila) == 0) return(gioco)
  for (i in seq_along(gioco$pila)) {
    if (gioco$pila[[i]]$id == new_obj$id) {
      gioco$pila[[i]] <- new_obj
      return(gioco)
    }
  }
  # se non trovato, append (safety)
  gioco$pila <- append(gioco$pila, list(new_obj))
  return(gioco)
}

# Trova oggetto per id (semplice)

metti_su_pila <- function(gioco, stack_obj) {
  if (is.null(gioco$pila)) gioco$pila <- list()
  
  gioco$pila <- append(gioco$pila, list(stack_obj))
  
  nome <- if (!is.null(stack_obj$carta)) stack_obj$carta$nome else stack_obj$tipo
  
  gioco <- aggiungi_log(
    gioco,
    paste0(
      "→ Oggetto messo sulla pila: ", nome,
      " (ID ", stack_obj$id, 
      ", tipo: ", stack_obj$tipo, 
      ", controller: ", stack_obj$controller_id, ")"
    )
  )
  if (!is.null(stack_obj$carta)) {
    stack_obj$carta$zona <- "pila"
  }
  return(gioco)
}

peek_pila <- function(gioco) {
  if (is.null(gioco$pila) || length(gioco$pila) == 0) return(NULL)
  gioco$pila[[ length(gioco$pila) ]]
}

pop_pila <- function(gioco) {
  if (is.null(gioco$pila) || length(gioco$pila) == 0) {
    return(list(gioco = gioco, oggetto = NULL))
  }
  
  idx <- length(gioco$pila)
  oggetto <- gioco$pila[[idx]]
  gioco$pila <- gioco$pila[-idx]
  
  nome <- if (!is.null(oggetto$carta)) oggetto$carta$nome else oggetto$tipo
  
  gioco <- aggiungi_log(
    gioco,
    paste0("↓ Oggetto rimosso dalla pila: ", nome, " (ID ", oggetto$id, ")")
  )
  
  list(gioco = gioco, oggetto = oggetto)
}

validate_targets <- function(stack_obj, gioco) {
  if (is.null(stack_obj$targets) || length(stack_obj$targets) == 0) {
    return(list(all_illegal = FALSE, legality = list()))
  }
  
  legality <- list()
  any_legal <- FALSE
  
  for (target_id in stack_obj$targets) {
    is_legal <- is_target_still_legal(target_id, gioco)
    legality[[target_id]] <- is_legal
    if (is_legal) any_legal <- TRUE
  }
  
  list(
    all_illegal = !any_legal,
    legality = legality
  )
}