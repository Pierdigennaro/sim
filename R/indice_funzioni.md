# Indice delle funzioni R
Questo documento elenca tutte le funzioni trovate nel progetto, raggruppate per file.
Per ogni funzione sono indicate:
- **Funzione**: nome della funzione
- **Funzioni del progetto chiamate**: altre funzioni definite nel progetto e chiamate al suo interno
- **Tutte le funzioni chiamate**: tutte le chiamate a funzione rilevate (incluse funzioni base R o di package)

## File: `R/ai/euristic.R`

| Funzione | Funzioni del progetto chiamate | Tutte le funzioni chiamate |
|----------|---------------------------------|-----------------------------|
| `gioca_terra` |  | append;list |

## File: `R/carica_tutto.R`

| Funzione | Funzioni del progetto chiamate | Tutte le funzioni chiamate |
|----------|---------------------------------|-----------------------------|
| `carica_tutto` |  | grepl;invisible;list.files;message;sort;source |

## File: `R/core/Play.R`

| Funzione | Funzioni del progetto chiamate | Tutte le funzioni chiamate |
|----------|---------------------------------|-----------------------------|
| `controlla_vittoria` | aggiungi_log | aggiungi_log;isTRUE;length;logical;paste;paste0;perso;serve;sum;vapply;which |
| `esegui_turno` | aggiungi_log;controlla_azioni_stato;fase_combattimento;fase_finale;fase_iniziale;fase_principale2;fase_principale_1;get_giocatore_attivo;passa_al_prossimo_giocatore;reset_variabili_turno | aggiungi_log;controlla_azioni_stato;fase_combattimento;fase_finale;fase_iniziale;fase_principale2;fase_principale_1;finale;get_giocatore_attivo;iniziale;is.function;is.null;passa_al_prossimo_giocatore;paste;paste0;reset_variabili_turno;turno |
| `gioca_partita` | aggiungi_log;controlla_azioni_stato;controlla_vittoria;esegui_turno | aggiungi_log;controlla_azioni_stato;controlla_vittoria;esegui_turno;identical;is.null;pareggio;paste;paste0 |

## File: `R/core/game_state.R`

| Funzione | Funzioni del progetto chiamate | Tutte le funzioni chiamate |
|----------|---------------------------------|-----------------------------|
| `controlla_azioni_stato` | aggiungi_log;get_zona;set_zona;sposta_carta | aggiungi_log;c;character;cimitero;distrutta;esistere;get_zona;globali;grepl;integer;is.null;isTRUE;length;list;logical;names;paste;paste0;perdono;personali;prima;seq_along;set_zona;sort;split;sposta_carta;tolower;unique;vapply;vuoto;which |

## File: `R/core/stack/stack_core.R`

| Funzione | Funzioni del progetto chiamate | Tutte le funzioni chiamate |
|----------|---------------------------------|-----------------------------|
| `action_choose_target` | aggiorna_stack_object_in_gioco;trova_bersagli_validi | aggiorna_stack_object_in_gioco;is.null;length;list;scelti;trova_bersagli_validi |
| `action_deal_damage` | aggiungi_log;applica_danno_a_creatura;applica_danno_a_giocatore;trova_oggetto_per_id | aggiungi_log;applica_danno_a_creatura;applica_danno_a_giocatore;c;identical;is.null;length;paste0;trova_oggetto_per_id;trovato |
| `action_move_zone` | sposta_oggetto_per_id | from;identical;is.null;length;sposta_oggetto_per_id;target;to |
| `aggiorna_stack_object_in_gioco` |  | append;length;list;seq_along |
| `applica_effetti` | applica_singolo_effetto | applica_singolo_effetto;is.null |
| `applica_singolo_effetto` | action_choose_target;action_deal_damage;action_move_zone | action;action_choose_target;action_create_token;action_deal_damage;action_draw_cards;action_move_zone;action_return_to_battlefield;paste0;stop |
| `crea_stack_object` |  | Sys.time;as.integer;is.null;list;paste0;runif |
| `gestisci_post_risoluzione_zona` | aggiungi_log;get_zona;is_instant_or_sorcery;set_zona;trova_indice_giocatore | aggiungi_log;get_zona;is.null;is_instant_or_sorcery;length;paste0;permanenti;set_zona;trova_indice_giocatore;vere |
| `is_target_still_legal` | trova_oggetto_per_id | is.null;trova_oggetto_per_id |
| `lancia_magia_dalla_mano` | crea_stack_object;gioca_terra;metti_su_pila | crea_stack_object;gioca_terra;grepl;length;metti_su_pila;stop;tolower |
| `metti_su_pila` | aggiungi_log | aggiungi_log;append;is.null;list;paste0 |
| `peek_pila` |  | is.null;length |
| `pop_pila` | aggiungi_log | aggiungi_log;is.null;length;list;paste0 |
| `resolve_top_of_stack` | aggiungi_log;applica_effetti;controlla_azioni_stato;gestisci_post_risoluzione_zona;pop_pila;validate_targets | aggiungi_log;applica_effetti;bersagli;controlla_azioni_stato;gestisci_inneschi_da_eventi;gestisci_post_risoluzione_zona;is.null;paste0;pop_pila;sorgente;validate_targets |
| `risolvi_pila` | resolve_top_of_stack | is.null;length;resolve_top_of_stack |
| `sposta_oggetto_per_id` | aggiungi_log;sposta_carta;trova_oggetto_per_id | aggiungi_log;default;giocatore;is.null;paste0;sorgente;sposta_carta;tolower;trova_oggetto_per_id |
| `trova_bersagli_validi` | get_zona | get_zona;grepl;is.null;length;list;seq_along;tolower |
| `trova_oggetto_per_id` | get_zona | c;get_zona;is.null;length;list;pila;seq_along |
| `validate_targets` | is_target_still_legal | is.null;is_target_still_legal;length;list |

## File: `R/core/state/damage.R`

| Funzione | Funzioni del progetto chiamate | Tutte le funzioni chiamate |
|----------|---------------------------------|-----------------------------|
| `applica_danno_a_creatura` | aggiungi_log;get_zona;set_zona | aggiungi_log;as.integer;danni;get_zona;is.na;paste0;set_zona |
| `applica_danno_a_giocatore` | aggiungi_log | aggiungi_log;as.integer;danni;is.na;paste0 |

## File: `R/io/logging.R`

| Funzione | Funzioni del progetto chiamate | Tutte le funzioni chiamate |
|----------|---------------------------------|-----------------------------|
| `aggiungi_log` |  | Sys.time;append;character;format;invisible;is.null;message;paste0 |

## File: `R/io/snapshot.R`

| Funzione | Funzioni del progetto chiamate | Tutte le funzioni chiamate |
|----------|---------------------------------|-----------------------------|
| `salva_snapshot` |  | Sys.time;append;list |

## File: `R/objects/cards.R`

| Funzione | Funzioni del progetto chiamate | Tutte le funzioni chiamate |
|----------|---------------------------------|-----------------------------|
| `crea_carta` |  | accumulato;as.integer;character;gsub;is.null;isTRUE;length;list;paste;paste0;runif |
| `stampa_carta` |  | ID;ifelse;is.null;paste;paste0 |

## File: `R/objects/decks.R`

| Funzione | Funzioni del progetto chiamate | Tutte le funzioni chiamate |
|----------|---------------------------------|-----------------------------|
| `conta_carte_mazzo` |  | length |
| `crea_mazzo` |  | as.integer;isTRUE;lapply;list;mazzo;paste0;runif |
| `mischia_mazzo` |  | length;sample |
| `mischia_mazzo_giocatore` | mischia_mazzo | is.null;mischia_mazzo;warning |
| `pesca_carta_mazzo` |  | gioco;is.null;length;list;stop;zona |
| `riepilogo_mazzo` |  | invisible;length;names;sort;sprintf;table;vapply |
| `stampa_mazzo` | conta_carte_mazzo | conta_carte_mazzo;invisible;is.null;min;seq_len;sprintf |
| `stampa_mazzo_giocatore` | stampa_mazzo | invisible;is.null;paste;stampa_mazzo |

## File: `R/objects/partita.R`

| Funzione | Funzioni del progetto chiamate | Tutte le funzioni chiamate |
|----------|---------------------------------|-----------------------------|
| `crea_partita` | aggiungi_log | Determino;aggiungi_log;as.integer;character;globali;integer;is.null;lanciate;length;list;paste;paste0;runif;sample;seq_along;stop;vapply |
| `crea_partita_2_4` | crea_partita | c;crea_partita;is.null;list |

## File: `R/objects/players.R`

| Funzione | Funzioni del progetto chiamate | Tutte le funzioni chiamate |
|----------|---------------------------------|-----------------------------|
| `crea_giocatore` | crea_mazzo | as.integer;crea_mazzo;is.null;list;mazzo;paste0;runif;tolower |
| `inizializza_giocatore_in_partita` | mischia_mazzo;pesca_carta_mazzo | is.null;length;list;mischia_mazzo;pesca_carta_mazzo;seq_len |
| `stampa_giocatore` |  | is.null;isTRUE;length;seq_len;sprintf |

## File: `R/objects/zone.R`

| Funzione | Funzioni del progetto chiamate | Tutte le funzioni chiamate |
|----------|---------------------------------|-----------------------------|
| `get_zona` |  | c;is.null;stop;tolower |
| `set_zona` |  | c;is.null;stop;tolower |
| `sposta_carta` | get_zona;set_zona;trova_indice_giocatore | carta;get_zona;giocatori;is.null;is.numeric;length;set_zona;stop;tolower;trova_indice_giocatore |

## File: `R/phases/beginning/Draw_utility.R`

| Funzione | Funzioni del progetto chiamate | Tutte le funzioni chiamate |
|----------|---------------------------------|-----------------------------|
| `gestisci_inneschi_draw` | aggiungi_log | aggiungi_log;append;is.null;length;list;paste0;seq_along |

## File: `R/phases/beginning/Untap_utility.R`

| Funzione | Funzioni del progetto chiamate | Tutte le funzioni chiamate |
|----------|---------------------------------|-----------------------------|
| `aggiorna_ciclo_giorno_notte` | aggiungi_log | aggiungi_log;giorno;notte |
| `phase_in_giocatoreattivo` | get_giocatore_attivo | aggiorna_campi_giocatori;append;get_giocatore_attivo;isTRUE;paste0;seq_along |
| `stappa_giocatoreattivo` | get_giocatore_attivo | aggiorna_campi_giocatori;append;get_giocatore_attivo;isTRUE;lapply;paste0;seq_along |

## File: `R/phases/beginning/Upkeep_utility.R`

| Funzione | Funzioni del progetto chiamate | Tutte le funzioni chiamate |
|----------|---------------------------------|-----------------------------|
| `gestisci_inneschi_upkeep` | aggiungi_log | aggiungi_log;append;is.null;length;list;paste0;seq_along |

## File: `R/phases/beginning/beginning.R`

| Funzione | Funzioni del progetto chiamate | Tutte le funzioni chiamate |
|----------|---------------------------------|-----------------------------|
| `fase_iniziale` | aggiungi_log;sottofase_draw;sottofase_untap;sottofase_upkeep | aggiungi_log;paste;sottofase_draw;sottofase_untap;sottofase_upkeep |
| `sottofase_draw` | aggiungi_log;assegna_priorita_giocatore_attivo;controlla_azioni_stato;gestisci_inneschi_draw;gestisci_inneschi_pescata;pesca_carta | Acquisizione;aggiungi_log;assegna_priorita_giocatore_attivo;controlla_azioni_stato;gestisci_inneschi_draw;gestisci_inneschi_pescata;pesca_carta |
| `sottofase_untap` | aggiorna_ciclo_giorno_notte;aggiungi_log;phase_in_giocatoreattivo;stappa_giocatoreattivo | STAP;aggiorna_ciclo_giorno_notte;aggiungi_log;paste;phase_in_giocatoreattivo;stappa_giocatoreattivo |
| `sottofase_upkeep` | aggiungi_log;assegna_priorita_giocatore_attivo;controlla_azioni_stato;gestisci_inneschi_upkeep | Mantenimento;aggiungi_log;assegna_priorita_giocatore_attivo;controlla_azioni_stato;gestisci_inneschi_upkeep;paste |

## File: `R/phases/combat/combat.R`

| Funzione | Funzioni del progetto chiamate | Tutte le funzioni chiamate |
|----------|---------------------------------|-----------------------------|
| `fase_combattimento` | aggiungi_log;sottofase_dichiarazione_attaccanti;sottofase_dichiarazione_bloccanti;sottofase_fine_combattimento;sottofase_inizio_combattimento | aggiungi_log;length;sottofase_danno_da_combattimento;sottofase_dichiarazione_attaccanti;sottofase_dichiarazione_bloccanti;sottofase_fine_combattimento;sottofase_inizio_combattimento |
| `sottofase_danno_combattimento` | aggiungi_log | abilita_post_danno;aggiungi_log;assegna_priorita;controlla_morti_e_azioni_stato;prima_stretta_danno;seconda_stretta_danno;stretta |
| `sottofase_dichiarazione_attaccanti` | aggiungi_log;assegna_priorita_giocatore_attivo | aggiungi_log;assegna_priorita_giocatore_attivo;dichiara_attaccanti;innesca_abilita_attacco |
| `sottofase_dichiarazione_bloccanti` | aggiungi_log | aggiungi_log;assegna_priorita_giocatore_in_difesa;dichiara_bloccanti;innesca_abilita_blocco |
| `sottofase_fine_combattimento` | aggiungi_log;assegna_priorita_giocatore_attivo | aggiungi_log;assegna_priorita_giocatore_attivo;gestisci_inneschi_fine_combattimento |
| `sottofase_inizio_combattimento` | aggiungi_log;assegna_priorita_giocatore_attivo | aggiungi_log;assegna_priorita_giocatore_attivo;gestisci_inneschi_inizio_combattimento |

## File: `R/phases/ending/End_utility.R`

| Funzione | Funzioni del progetto chiamate | Tutte le funzioni chiamate |
|----------|---------------------------------|-----------------------------|
| `passa_al_prossimo_giocatore` | aggiungi_log | aggiungi_log;length;paste |
| `reset_variabili_turno` |  | is.null;list |

## File: `R/phases/ending/ending.R`

| Funzione | Funzioni del progetto chiamate | Tutte le funzioni chiamate |
|----------|---------------------------------|-----------------------------|
| `fase_finale` | aggiungi_log;sottofase_cleanup;sottofase_end_step | aggiungi_log;sottofase_cleanup;sottofase_end_step |
| `sottofase_cleanup` | aggiungi_log;risolvi_pila | Pulizia;aggiungi_log;applica_regola_mano_massima;length;metti_in_pila_trigger;rimuovi_danni_tutti;risolvi_pila;termina_effetti_fine_turno;trova_trigger_cleanup |
| `sottofase_end_step` | aggiungi_log;assegna_priorita_giocatore_attivo | Turno;aggiungi_log;assegna_priorita_giocatore_attivo;gestisci_inneschi_end_step |

## File: `R/phases/main/postcombat_main.R`

| Funzione | Funzioni del progetto chiamate | Tutte le funzioni chiamate |
|----------|---------------------------------|-----------------------------|
| `fase_principale2` | aggiungi_log;assegna_priorita_giocatore_attivo | aggiungi_log;assegna_priorita_giocatore_attivo;controllo_azioni_stato;passa_alla_fase_successiva |

## File: `R/phases/main/precombat_main.R`

| Funzione | Funzioni del progetto chiamate | Tutte le funzioni chiamate |
|----------|---------------------------------|-----------------------------|
| `fase_principale_1` | aggiungi_log;assegna_priorita_giocatore_attivo | aggiungi_log;assegna_priorita_giocatore_attivo;gestisci_azioni_giocatore_attivo;gestisci_inneschi_main_phase |

## File: `R/test/play_test_1.R`

| Funzione | Funzioni del progetto chiamate | Tutte le funzioni chiamate |
|----------|---------------------------------|-----------------------------|
| `crea_carte_test` | crea_carta | Visits;crea_carta;list |
| `crea_giocatori_test` | crea_giocatore;crea_mazzo_test_burn;crea_mazzo_test_elfi;inizializza_giocatore_in_partita | crea_giocatore;crea_mazzo_test_burn;crea_mazzo_test_elfi;inizializza_giocatore_in_partita;list |
| `crea_mazzo_test_burn` | crea_carte_test;crea_mazzo | c;crea_carte_test;crea_mazzo;replicate |
| `crea_mazzo_test_elfi` | crea_carte_test;crea_mazzo | c;crea_carte_test;crea_mazzo;replicate |
| `crea_partita_test` | crea_giocatori_test;crea_partita | crea_giocatori_test;crea_partita;list;set.seed |
| `gioca_partita_test` | controlla_azioni_stato;crea_partita_test;esegui_turno | any;controlla_azioni_stato;crea_partita_test;esegui_turno;isTRUE;length;logical;perso;seq_len;vapply;which |

## File: `R/test/test_1.R`

| Funzione | Funzioni del progetto chiamate | Tutte le funzioni chiamate |
|----------|---------------------------------|-----------------------------|
| `crea_carte_test` | crea_carta | Visits;crea_carta;effetti;list |
| `crea_giocatori_test` | crea_giocatore;crea_mazzo_test_burn;crea_mazzo_test_elfi;inizializza_giocatore_in_partita | crea_giocatore;crea_mazzo_test_burn;crea_mazzo_test_elfi;inizializza_giocatore_in_partita;list |
| `crea_mazzo_test_burn` | crea_carte_test;crea_mazzo | c;crea_carte_test;crea_mazzo;replicate |
| `crea_mazzo_test_elfi` | crea_carte_test;crea_mazzo | c;crea_carte_test;crea_mazzo;replicate |
| `crea_partita_test` | crea_giocatori_test;crea_partita | crea_giocatori_test;crea_partita;set.seed |
| `gioca_partita_test` | crea_partita_test;esegui_turno | crea_partita_test;esegui_turno;is.null;seq_len |

## File: `R/test/test_core_smoke.R`

| Funzione | Funzioni del progetto chiamate | Tutte le funzioni chiamate |
|----------|---------------------------------|-----------------------------|
| `run_core_smoke_tests` | test_oggetti_base;test_sba_danno_letale;test_stack_lightning_bolt_su_giocatore | invisible;list;test_oggetti_base;test_sba_danno_letale;test_stack_lightning_bolt_su_giocatore |
| `test_oggetti_base` | crea_carte_test;crea_giocatore;crea_mazzo_test_burn;crea_mazzo_test_elfi;crea_partita_2_4;inizializza_giocatore_in_partita | crea_carte_test;crea_giocatore;crea_mazzo_test_burn;crea_mazzo_test_elfi;crea_partita_2_4;definita;exists;giocatori;identical;inizializza_giocatore_in_partita;invisible;length;list;loro;stop;stopifnot |
| `test_sba_danno_letale` | controlla_azioni_stato;crea_carte_test;crea_giocatore;crea_mazzo;crea_partita_2_4;get_zona;set_zona | controlla_azioni_stato;crea_carte_test;crea_giocatore;crea_mazzo;crea_partita_2_4;get_zona;invisible;length;list;set_zona;stopifnot |
| `test_stack_lightning_bolt_su_giocatore` | crea_carte_test;crea_giocatore;crea_mazzo;crea_partita_2_4;lancia_magia_dalla_mano;risolvi_pila | c;crea_carte_test;crea_giocatore;crea_mazzo;crea_partita_2_4;invisible;lancia_magia_dalla_mano;length;list;risolvi_pila;stopifnot |

## File: `R/utilities/helpers.R`

| Funzione | Funzioni del progetto chiamate | Tutte le funzioni chiamate |
|----------|---------------------------------|-----------------------------|
| `aggiungi_carta` |  | append |
| `assegna_priorita_giocatore_attivo` | aggiungi_log;get_giocatore_attivo | aggiungi_log;gestisci_azioni_priorita;get_giocatore_attivo;paste0 |
| `gestisci_inneschi_pescata` | aggiungi_log | aggiungi_log;append;is.null;list;paste |
| `get_giocatore_attivo` |  |  |
| `get_giocatori_non_attivi` |  | seq_along |
| `imposta_prossimo_giocatore_attivo` |  | length |
| `incrementa_turno` |  |  |
| `is_instant_or_sorcery` |  | grepl;tolower |
| `is_permanent_type` | is_instant_or_sorcery | is_instant_or_sorcery;tolower |
| `pesca_carta` | aggiungi_log;controlla_azioni_stato;pesca_carta_mazzo | aggiungi_log;append;c;controlla_azioni_stato;is.null;length;list;paste;pesca_carta_mazzo;seq_len |
| `trova_indice_giocatore` |  | as.integer;character;is.null;is.numeric;length;stop;vapply;which |
