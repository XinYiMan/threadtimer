# TThreadTimer

`TThreadTimer` è un componente per Lazarus/FPC simile a `TTimer`, ma con esecuzione **in un thread secondario**.  
Questo permette di eseguire logica o operazioni lunghe senza bloccare l'interfaccia grafica.

---

## Caratteristiche principali

- Esegue il codice di `OnTimer` in un **thread separato**.
- Consente di aggiornare la GUI in modo sicuro tramite `OnAfterTimer`.
- È possibile decidere dentro `OnTimer` se chiamare `OnAfterTimer` tramite `TriggerAfter`.
- `Data: TObject` permette di passare oggetti dal thread secondario al main thread.
- `State: Variant` permette di passare al timer uno stato di tipo variant su cui programmare degli eventi.

---

## Installazione

1. Apri Lazarus.
2. Vai su **Package → New Package**.
3. Aggiungi il file `ThreadTimer.pas`.
4. Compila e installa il package.
5. Dopo il riavvio dell’IDE, troverai `TThreadTimer` nella palette dei componenti (gruppo “Threads”).

---

## Proprietà principali

- `Interval: Cardinal` — intervallo in millisecondi tra un’esecuzione e l’altra.
- `Enabled: Boolean` — abilita o disabilita il timer.
- `TriggerAfter: Boolean` — flag interno per decidere se chiamare `OnAfterTimer`.
- `OnTimer: TThreadTimerEvent` — evento eseguito **nel thread secondario**.
- `OnAfterTimer: TThreadTimerAfterEvent` — evento eseguito **nel main thread** (GUI safe).

---

## Esempio d’uso
- Trovaee un codice d'esempio completo nella sottocartella example
