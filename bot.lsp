; To-Do:
; - Menge der übereinstimmungen in Auswahl einbauen
; Datenstrukturen laden

(load "config.lsp")
(load "data.fas")
(load "sio.fas")

(read-data "woerter.lst" "saetze.lst")

; Eingabedaten verarbeiten, in Datenstruktur einfügen und Kriterien für Antwort liefern
(defun handle-phrase (zeile)
  (format t zeile)
  ; Satztyp bestimmen: Aussage oder Frage
  (cond 
    ((search "?" zeile)
      (setq typ "f")
    )
    (t
      (setq typ "a")
    )
  )
  (setq id (+ (car (car lsaetze)) 1))
  ; Alle Satzzeichen entfernen
  (setq zeile2 (string-trim '(#\, #\. #\! #\? #\") zeile)) ;"  
  (setq teile nil)
  ; Zeile zerteilen
  (cut-line-rec " " zeile2)
  ; Satz in Daten einfügen
  ; Wenn erfolgreich Wörter einfuegen
  (new-phrase id zeile typ)
  (cond
    ((not schonda)
      ; Woerter in Daten einfügen
      (add-words-rec id teile)
      ; Daten speichern
      (save-data "woerter.lst" "saetze.lst")
    )
  )
  ; Antwort suchen
  (answer teile)
)

; Passende antworden suchen
(defun answer (teile)
  (setq funde nil)
  ; Vorkommnisse aller Wörter suchen
  (setq funde (find-all-words-rec teile funde))
  (cond 
    ; Wenn keine Antwort vorhanden Standartantwort ausgeben
    ((= (length funde) 1) 
      (return-from answer standart-antwort)
    )
    ; Wenn nur eine Antwort vorhanden diese ausgeben ohne den ganzen Mist durchzumachen
    ((= (length funde) 2)
      (return-from answer (car (get-phrase (fund-id (car funde)))))
    )
    (t
      ; Gunde nach Menge suchen
      (setq funde (sort funde 'my-mengen-test))
      (setq funde (cdr funde))
      ; Auf maximale Länge zuschneiden
      (cond 
        ((>(length funde) max-antworten)
          (nbutlast funde (- (length funde) max-antworten))
        )
      )
      (setq antworten nil)
      ; Satz-Id in Sätze umsetzen
      (setq anzahl-fragen 0)
      (setq anzahl-aussagen 0)
      (do ((ct 0 (+ 1 ct)))
        ((= ct (length funde)))
        (setq antworten (cons (append (get-phrase (fund-id (nth ct funde))) (list (fund-menge (nth ct funde)))) antworten))
        ; Anzahl der Fragen und Aussagen zählen
        (cond
          ((equal (cadr (get-phrase (fund-id (nth ct funde)))) "a")
            (setq anzahl-aussagen (+ 1 anzahl-aussagen))
          )
          (t
            (setq anzahl-fragen (+ 1 anzahl-fragen))      
          )      
        )    
      )
      ; Reihenfolge der antworten an die Reihenfolge der Funde anpassen
      (setq antworten (reverse antworten))    
      ; Antworten nach Typ sortieren
      (setq antworten (sort antworten 'my-typ-test))
      ; Anteil
      (cond 
        ((not (= anzahl-fragen 0))
          (setq anteil-frage (round (/ prozent-fragen anzahl-fragen)))
        )
        (t 
          (setq anteil-frage 0)
        )
      )
      (cond 
        ((not (= anzahl-fragen 0))
          (setq anteil-aussage (round (/ (- 100 prozent-fragen) anzahl-aussagen)))
        )
        (t 
          (setq anteil-frage 0)
        )
      )
      ; Auswahlmöglichkeiten für zufällige Auswahl vorbereiten
      (setq moeglichkeiten nil)
      (do ((ct1 0 (+ ct1 1)))
        ((= ct1 (length antworten)))
          (cond 
            ((< ct1 anzahl-fragen)
              (do ((ct2 0 (+ 1 ct2)))
                ((= ct2 anteil-frage))
                  (setq moeglichkeiten (cons (car (nth ct1 antworten)) moeglichkeiten))
    		  )
            )
            (t
              (do ((ct2 0 (+ 1 ct2)))
                ((= ct2 anteil-aussage))
                  (setq moeglichkeiten (cons (car (nth ct1 antworten)) moeglichkeiten))              
              )
            )
          )
      )
      (setq zufall (+ 1 (random 100 (make-random-state t))))
      (nth zufall moeglichkeiten)
    )
  )
)

; Alle Wörter in einer Liste suchen und Daten zusammenfügen
(defun find-all-words-rec (woerter funde)
  (cond
    ((equal (not woerter) nil)  
      (setq funde (find-word (car woerter) funde))
      (find-all-words-rec (cdr woerter) funde)
    )
    (t funde)
  )
)

; Mein Test zum sortieren nach Menge
(defun my-mengen-test (a b)
  (> (fund-menge a) (fund-menge b))
)

; Mein Test zum sortieren nach Typ
(defun my-typ-test (a b)
  (string> (cadr a) (cadr b))
)

; Zeile an jedem Vorkommnis von Zeichen zerteilen und Liste bilden
(defun cut-line-rec (zeichen zeile)
  (cond 
    ((not (search zeichen zeile))
      (setq teile (cons zeile teile))
    )
    (t
      (setq teile (cons (subseq zeile 0 (search zeichen zeile)) teile))
      (cut-line-rec zeichen (subseq zeile (+ (search zeichen zeile) 1) (length zeile)))
    )
  )
)

; Woerter-Liste in Daten einfügen
(defun add-words-rec (id liste)
  (cond 
    ((equal (not liste) nil)
      (new-word (car liste) id)
      (add-words-rec id (cdr liste))
    )
  )
)

; Die Unterhaltung führen
(defun chat ()
  (do ((ct 0 0))
    (nil)
      (setq zeile (read-line socket)) 
      (cond 
        ((search "privmsg" (string-downcase zeile))
          (cond 
            ((not (null (search "#" zeile)))
              (setq zeile (subseq zeile (search "#" zeile) (length zeile)))
              (setq zeile (subseq zeile (+ 1 (search " " zeile)) (length zeile)))
              (setq zeile (string-trim '(#\:) zeile))
              (setq antw (handle-phrase zeile))
              (format socket "PRIVMSG ~A :~A~%" irc-channel antw)
            )
          )
        )
        ((search "ping" (string-downcase zeile))
          (setq zeile (subseq zeile 5 (length zeile)))
          (format socket "PONG ~A~%" zeile)
        )
      )
  )
)

(irc-connect)
(chat)
