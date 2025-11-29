`%||%` <- function(a, b) if (!is.null(a)) a else b

crea_stack_object <- function(
    tipo,                 # "spell" | "activated" | "triggered"
    carta = NULL,         # lista carta (può essere NULL per abilità “pure”)
    sorgente_id = NULL,   # id permanente o carta che lo genera
    controller_id,        # id del controllore
    targets = list(),     # lista di id bersagli scelti
    effetti = list(),     # lista DSL effetti
    costo_pagato = NULL,
    mode = NULL,
    priority_owner_hint = NULL
) {
  list(
    tipo = tipo,
    carta = carta,
    sorgente_id = sorgente_id %||% if (!is.null(carta)) carta$id else NULL,
    controller_id = controller_id,
    targets = targets,
    costo_pagato = costo_pagato,
    effetti = effetti,
    priority_owner_hint = priority_owner_hint,
    mode = mode,
    id = paste0("stack_", as.integer(runif(1, 1, 1e6))),
    timestamp = Sys.time()
  )
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

action_choose_target <- function(gioco, eff, stack_obj) {
  # Se i target sono già stati scelti (da UI/AI), non fare nulla
  if (!is.null(stack_obj$targets) && length(stack_obj$targets) > 0) {
    return(gioco)
  }
  
  candidates <- trova_bersagli_validi(
    gioco, 
    eff$target_filter, 
    stack_obj$controller_id
  )
  
  if (length(candidates) == 0) {
    # Nessun bersaglio disponibile: lasciamo targets vuoto
    # Sarà validate_targets a far "fizzle" la spell/abilità
    return(gioco)
  }
  
  scelta <- candidates[[1]]$id   # TODO: sostituire con AI/giocatore
  stack_obj$targets <- list(scelta)
  
  gioco <- aggiorna_stack_object_in_gioco(gioco, stack_obj)
  return(gioco)
}

action_deal_damage <- function(gioco, eff, stack_obj) {
  target_id <- if (identical(eff$target, "chosen")) {
    if (length(stack_obj$targets) == 0) return(gioco)
    stack_obj$targets[[1]]
  } else {
    eff$target
  }
  
  if (is.null(target_id)) return(gioco)
  
  loc <- trova_objetto_per_id(gioco, target_id)
  if (is.null(loc)) {
    gioco <- aggiungi_log(gioco, paste0("Bersaglio non trovato (", target_id, "), danno annullato"), "warn")
    return(gioco)
  }
  
  if (loc$zone == "battlefield" && loc$type == "creature") {
    gioco <- applica_danno_a_creatura(gioco, loc, eff$amount)
  } else if (loc$zone == "player") {
    gioco <- applica_danno_a_giocatore(gioco, loc$player_index, eff$amount)
  }
  
  return(gioco)
}

action_move_zone <- function(gioco, eff, stack_obj) {
  # eff$from, eff$to, eff$target: id
  target_id <- ifelse(eff$target == "chosen", stack_obj$targets[[1]], eff$target)
  gioco <- sposta_objetto(gioco, target_id, eff$to)
  return(gioco)
}

# Cerca bersagli validi in base a filtro semplificato
trova_bersagli_validi <- function(gioco, filtro, controller) {
  # filtro es: "any_target", "creature_in_any_graveyard", "opponent_creature", "creature_you_control"
  res <- list()
  if (filtro == "any_target") {
    # ritorna giocatori e permanenti
    for (i in seq_along(gioco$giocatori)) {
      res <- append(res, list(list(id = paste0("player_", i), zone = "player", type = "player", player_index = i)))
    }
    for (i in seq_along(gioco$giocatori)) {
      for (c in gioco$giocatori[[i]]$campo) {
        res <- append(res, list(list(id = c$id, zone = "battlefield", type = c$tipo, owner = c$owner, controller = c$controller)))
      }
    }
  } else if (filtro == "creature_in_any_graveyard") {
    for (i in seq_along(gioco$giocatori)) {
      for (c in gioco$giocatori[[i]]$cimitero) {
        if (c$tipo == "Creature") res <- append(res, list(list(id=c$id, zone="graveyard", card=c)))
      }
    }
  }
  return(res)
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
trova_objetto_per_id <- function(gioco, id) {
  # cerca giocatori
  if (grepl("^player_", id)) {
    idx <- as.integer(sub("player_", "", id))
    if (!is.na(idx) && idx <= length(gioco$giocatori)) {
      return(list(zone="player", type="player", player_index = idx))
    }
  }
  # cerca permanenti sul campo
  for (i in seq_along(gioco$giocatori)) {
    for (c in gioco$giocatori[[i]]$campo) {
      if (c$id == id) return(list(zone="battlefield", type=c$tipo, card=c, owner=i, controller=c$controller))
    }
  }
  # cerca cimiteri ecc...
  return(NULL)
}


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

is_target_still_legal <- function(target_id, gioco) {
  loc <- trova_objetto_per_id(gioco, target_id)
  # Regola base: target esiste e rispetta le condizioni generiche minime
  if (is.null(loc)) return(FALSE)
  
  # Possibili controlli estesi (da espandere):
  # - se la carta ora ha shroud/hexproof
  # - se il giocatore è illegale (es. scooped)
  # - se un permanente ha cambiato zona
  
  return(TRUE)
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
      paste0("La spell/abilità ", nome, " (ID ", obj$id,
             ") non ha più alcun bersaglio legale → neutralizzata.")
    )
    # CR: spell/ability è neutralizzata e va comunque nella zona appropriata
    gioco <- gestisci_post_risoluzione_zona(gioco, obj, countered = TRUE)
    return(gioco)
  }
  
  # 2) Applica gli effetti DSL
  gioco <- applica_effetti(gioco, obj$effetti, obj)
  
  # 3) Gestione zona sorgente (spell che va al cimitero, abilità che sparisce)
  gioco <- gestisci_post_risoluzione_zona(gioco, obj, countered = FALSE)
  
  # 4) TODO: SBA + trigger post-risoluzione
  # gioco <- applica_state_based_actions(gioco)
  # gioco <- metti_trigger_in_pila_dopo_eventi(gioco)
  
  return(gioco)
}

risolvi_pila <- function(gioco) {
  while (!is.null(gioco$pila) && length(gioco$pila) > 0) {
    gioco <- resolve_top_of_stack(gioco)
    # In futuro: SBA + trigger tra una risoluzione e l’altra
  }
  gioco
}


gestisci_post_risoluzione_zona <- function(gioco, stack_obj, countered = FALSE) {
  # Qui decidi cosa succede alla carta/abilità dopo la risoluzione o il fizzle.
  # Esempio: se è una spell non permanente → va al cimitero.
  # Se è un istantaneo/s stregoneria: sposta carta dalla pila al cimitero.
  # Se è un’abilità attivata/innescata: non fa nulla (non è un oggetto di zona).
  
  # Pseudo:
  # if (!is.null(stack_obj$carta) && stack_obj$carta$tipo_carta %in% c("Instant", "Sorcery")) {
  #   gioco <- sposta_objetto(gioco, stack_obj$sorgente_id, "graveyard")
  # }
  gioco
}
