fase_combattimento <- function(gioco) {
  messaggio <- "=== Fase di Combattimento ==="
  gioco <- aggiungi_log(gioco, messaggio, tipo = "fase")
  
  gioco <- sottofase_inizio_combattimento(gioco)
  gioco <- sottofase_dichiarazione_attaccanti(gioco)
  # Se nessun attaccante → salta bloccanti e danno da combattimento
  if (length(gioco$attaccanti) > 0) {
    gioco <- sottofase_dichiarazione_bloccanti(gioco)
    gioco <- sottofase_danno_da_combattimento(gioco)
  }
  
  gioco <- sottofase_fine_combattimento(gioco)
  return(gioco)
}

sottofase_inizio_combattimento <- function(gioco) {
  gioco <- aggiungi_log(gioco, "Sottofase di inizio combattimento")
  gioco <- gestisci_inneschi_inizio_combattimento(gioco)
  gioco <- assegna_priorita_giocatore_attivo(gioco)
  return(gioco)
}

### 4.3.2 Dichiarazione Attaccanti ----
sottofase_dichiarazione_attaccanti <- function(gioco) {
  messaggio <- "Sottofase di dichiarazione delle creature attaccanti"
  gioco <- aggiungi_log(gioco, messaggio, tipo = "sottofase")
  
  # 1. Azione generata dal turno: dichiarare attaccanti
  # (Per ora placeholder)
  gioco <- dichiara_attaccanti(gioco)
  
  # 2. Mettere in pila abilità che si innescano "quando attacca"
  gioco <- innesca_abilita_attacco(gioco)
  
  # 3. Il giocatore attivo ottiene priorità
  gioco <- assegna_priorita_giocatore_attivo(gioco)
  
  return(gioco)
}

### 4.3.2 Dichiarazione Bloccanti ----
sottofase_dichiarazione_bloccanti <- function(gioco) {
  messaggio <- "Sottofase di dichiarazione dei bloccanti"
  gioco <- aggiungi_log(gioco, messaggio, tipo = "sottofase")
  
  # 1. Azione generata dal turno: dichiarare i bloccanti
  gioco <- dichiara_bloccanti(gioco)
  
  # 2. Mettere in pila abilità "quando blocca"
  gioco <- innesca_abilita_blocco(gioco)
  
  # 3. Il difensore ottiene priorità
  gioco <- assegna_priorita_giocatore_in_difesa(gioco)
  
  return(gioco)
}

### 4.3.3 Danno da Combattimento ----
sottofase_danno_combattimento <- function(gioco) {
  gioco <- aggiungi_log(gioco, "Sottofase di danno da combattimento", tipo = "sottofase")
  
  # 1) Prima eventuale stretta (first strike / double strike)
  gioco <- prima_stretta_danno(gioco)
  
  # 2) Azioni basate sullo stato dopo la prima stretta
  gioco <- controlla_morti_e_azioni_stato(gioco)
  
  # 3) Seconda stretta (normale)
  gioco <- seconda_stretta_danno(gioco)
  
  # 4) Azioni basate sullo stato dopo la seconda stretta
  gioco <- controlla_morti_e_azioni_stato(gioco)
  
  # 5) Trigger vari e priorità
  gioco <- abilita_post_danno(gioco)
  gioco <- assegna_priorita(gioco, gioco$giocatore_attivo)
  
  return(gioco)
}

### 4.3.4 Fine Combattimento ----
sottofase_fine_combattimento <- function(gioco) {
  gioco <- aggiungi_log(gioco, "Fine combattimento")
  gioco <- gestisci_inneschi_fine_combattimento(gioco)
  gioco <- assegna_priorita_giocatore_attivo(gioco)
  return(gioco)
}
