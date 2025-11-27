crea_stack_object <- function(carta, controller, scelta_bersagli = list(), mode = NULL) {
  list(
    tipo = character(),               # "spell" | "activated" | "triggered"
    carta = NULL,                     # la carta sorgente (o NULL per abilità senza carta)
    sorgente_id = character(),        # id permanente o carta che lo genera
    controller_id = character(),      # id del controllore
    targets = list(),                 # lista di target scelti
    costo_pagato = NULL,              # dettagli del costo pagato
    effetti = list(),                 # lista della DSL degli effetti
    timestamp = numeric(),            # per APNAP o layer temporali
    priority_owner_hint = character(),# utile alla AI / replay / debugging
    mode = mode,                      # per modal spells (opzionale)
    id = paste0("stack_", as.integer(runif(1,1,1e6))),
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
  # eff$target_filter: string che descrive il filtro
  # semplificazione: se stack_obj$scelta_bersagli già presente -> usala
  if (!is.null(stack_obj$scelta_bersagli) && length(stack_obj$scelta_bersagli) > 0) {
    return(gioco)
  }
  # Implementazione minimale: scegli il primo id valido (meglio collegare UI o AI)
  candidates <- trova_bersagli_validi(gioco, eff$target_filter, stack_obj$controller)
  if (length(candidates) == 0) {
    # nessun bersaglio valido -> segnala per saltare effetti che dipendono da target
    eff$skipped <- TRUE
    return(gioco)
  }
  scelta <- candidates[[1]]$id
  stack_obj$scelta_bersagli <- list(scelta)
  # Nota: dobbiamo che la funzione che chiama applichi la modifica allo stack_obj in gioco
  # R non usa reference semantics per liste, quindi ritorniamo lo stack modifica come attributo
  # soluzione: memorizzare modifiche nello stato globale del gioco (sostituisci l'oggetto sulla pila)
  gioco <- aggiorna_stack_object_in_gioco(gioco, stack_obj)
  return(gioco)
}

action_deal_damage <- function(gioco, eff, stack_obj) {
  # eff$amount ; target può essere "chosen" o un id diretto
  target_id <- ifelse(eff$target == "chosen", stack_obj$scelta_bersagli[[1]], eff$target)
  if (is.null(target_id)) return(gioco) # nulla da fare
  
  # Determina se target è giocatore o creatura
  loc <- trova_objetto_per_id(gioco, target_id)
  if (is.null(loc)) {
    aggiungi_log(gioco, paste0("Bersaglio non trovato (", target_id, "), danno annullato"), "warn")
    return(gioco)
  }
  # Applica danno (semplificato)
  if (loc$zone == "battlefield" && loc$type == "creature") {
    # trova carta in campo e aggiungi danno (o riduci toughness)
    gioco <- applica_danno_a_creatura(gioco, loc, eff$amount)
  } else if (loc$zone == "player") {
    gioco <- applica_danno_a_giocatore(gioco, loc$player_index, eff$amount)
  }
  return(gioco)
}

action_move_zone <- function(gioco, eff, stack_obj) {
  # eff$from, eff$to, eff$target: id
  target_id <- ifelse(eff$target == "chosen", stack_obj$scelta_bersagli[[1]], eff$target)
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


metti_su_pila <- function(gioco, oggetto) {
  # Append LIFO
  gioco$pila <- append(gioco$pila, list(oggetto))
  
  # Log
  nome <- if (!is.null(oggetto$carta)) oggetto$carta$nome else oggetto$tipo
  gioco <- aggiungi_log(
    gioco,
    paste0("→ Oggetto messo sulla pila: ", nome,
           " (ID ", oggetto$id, 
           ", tipo: ", oggetto$tipo, 
           ", controller: ", oggetto$controller_id, ")")
  )
  
  return(gioco)
}

metti_su_pila <- function(gioco, stack_obj) {
  gioco$pila <- append(gioco$pila, list(stack_obj))
  gioco <- aggiungi_log(gioco, 
                        paste0("Messo in pila: ", stack_obj$carta$nome,
                               " (ID:", stack_obj$id, ")",
                               ", tipo: ", stack_obj$tipo,
                               ", controller: ", oggetto$controller_id, ")"))
  return(gioco)
}

peek_pila <- function(gioco) {
  if (length(gioco$pila) == 0) return(NULL)
  return(gioco$pila[[ length(gioco$pila) ]])
}

pop_pila <- function(gioco) {
  if (length(gioco$pila) == 0) {
    return(list(gioco = gioco, oggetto = NULL))
  }
  
  idx <- length(gioco$pila)
  oggetto <- gioco$pila[[idx]]
  
  gioco$pila <- gioco$pila[-idx]
  
  # Log
  nome <- if (!is.null(oggetto$carta)) oggetto$carta$nome else oggetto$tipo
  gioco <- aggiungi_log(
    gioco,
    paste0("↓ Oggetto rimosso dalla pila: ", nome, " (ID ", oggetto$id, ")")
  )
  
  return(list(gioco = gioco, oggetto = oggetto))
}

validate_targets <- function(stack_obj, gioco) {
  # niente target = automaticamente valido
  if (is.null(stack_obj$targets) || length(stack_obj$targets) == 0) {
    return(list(
      all_illegal = FALSE,
      legality = list()
    ))
  }
  
  legality <- list()
  any_legal <- FALSE
  
  for (t in stack_obj$targets) {
    is_legal <- is_target_still_legal(t, gioco)
    legality[[t]] <- is_legal
    if (is_legal) any_legal <- TRUE
  }
  
  all_illegal <- !any_legal
  
  return(list(
    all_illegal = all_illegal,
    legality = legality
  ))
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
  
  # pop dall’alto
  popped <- pop_pila(gioco)
  gioco <- popped$gioco
  obj <- popped$oggetto
  
  if (is.null(obj)) {
    return(gioco)  # pila vuota
  }
  
  nome <- if (!is.null(obj$carta)) obj$carta$nome else obj$tipo
  
  # 1) Validazione bersagli CR 608.2b
  validation <- validate_targets(obj, gioco)
  
  if (validation$all_illegal) {
    gioco <- aggiungi_log(
      gioco,
      paste0("La spell/abilità ", nome, " (ID ", obj$id,
             ") non ha più alcun bersaglio legale → neutralizzata.")
    )
    return(gioco)
  }
  
  # 2) Applica effetti (DSL engine)
  gioco <- applica_effetti(gioco, obj$effetti, obj)
  
  # 3) Trigger dopo risoluzione
  # (qui aggiungeremo i triggered abilities)
  
  return(gioco)
}

risolvi_pila <- function(gioco) {
  while (length(gioco$pila) > 0) {
    gioco <- resolve_top_of_stack(gioco)
  }
  return(gioco)
}


