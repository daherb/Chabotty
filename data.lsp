
; Konfiguration laden
(load "config.lsp")

; Struktur für die Suchfunde
(defstruct fund
  id
  menge
)

; Saetze und Wörter aus den Dateien laden
(defun read-data (woerter saetze)
  (setq lsaetze nil)
  (setq lwoerter nil)
  (setq datei (open woerter :direction :input :if-does-not-exist :error))
  (setq lwoerter (read datei))
  (close datei)
  (setq datei (open saetze :direction :input :if-does-not-exist :error))
  (setq lsaetze (read datei))
  (close datei)
)

; Woerter und Saetze speichern
(defun save-data (woerter saetze)
  (setq datei (open woerter :direction :output :if-exists :overwrite :if-does-not-exist :create))
  (print lwoerter datei)
  (close datei)
  ;(save-words-rec datei (reverse lwoerter)) 
  (setq datei (open saetze :direction :output :if-exists :overwrite :if-does-not-exist :create))
  (print lsaetze datei)
  (close datei)
)

; Neuen Satz einfügen
(defun new-phrase (id satz typ)
  (setq schonda nil)
  (do ((ct 0 (+ 1 ct)))
    ((= ct (length lsaetze)))
      (cond 
        ((equal (string-downcase (cadr (nth ct lsaetze))) (string-downcase satz))
          (setq schonda t)
        )
      )
  )
  (cond 
    ((not schonda)
      (setq ltemp nil)
      (setq ltemp (cons id (cons satz (cons typ ltemp))))
      (setq lsaetze (cons ltemp lsaetze))
      t
    )
    (t nil)
  )
)

; Neues Wort einfügen
(defun new-word (wort id)
  (setq schwarz nil)
  ; Überprüfen ob Wort in Blacklist ist
  (do ((i 0 (+ 1 i)))
    ((equal i (length blacklist)))
      (cond 
        ((equal (nth i blacklist) wort)
          (setq schwarz t)
        )
      )     
  )
  (cond 
    ((not schwarz)
      (setq ltemp nil)
      (setq ltemp (cons (string-downcase wort) (cons id ltemp)))
      (setq lwoerter (cons ltemp lwoerter))
    )
  )
)

; Satz anhand der id
(defun get-phrase (id)
  (get-phrase-rec id lsaetze)  
)
; Entsprechende rekursive Funktion
(defun get-phrase-rec (id daten)
  (cond 
    ((not (null daten))      
      (cond 
        ((equal (caar daten) id)
          (return-from get-phrase-rec (cdar daten))
        )
      )
      (get-phrase-rec id (cdr daten))
    )
    (t nil)
  )
)

; Woerter in Daten suchen
(defun find-word (swort funde)
  (find-word-rec swort lwoerter funde)
)

; Entsprechende Rekursions-Funktion
(defun find-word-rec (wort daten funde)
  (cond 
    ((not (null daten))      
      (cond 
        ((equal (caar daten) (string-downcase wort))
          (setq funde (look-if-there-is-the-phrase-and-add-it-if-it-does-not-exist-or-add-one-to-the-amount-of-places-of-discovery-in-a-recursive-way (cadar daten) funde funde nil))
        )
      )
      (find-word-rec wort (cdr daten) funde)
    )
    (t funde)
  )
)

; Funktion mit selbsterklärendem Namen
(defun look-if-there-is-the-phrase-and-add-it-if-it-does-not-exist-or-add-one-to-the-amount-of-places-of-discovery-in-a-recursive-way (id funde alles treffer) 
  (cond 
    ((not (null funde))      
      (cond 
        ((eq (fund-id (car funde)) id)
          (setf (fund-menge (car funde)) (+ (fund-menge (car funde)) 1))
          (setq treffer t)
        )
      )
      (look-if-there-is-the-phrase-and-add-it-if-it-does-not-exist-or-add-one-to-the-amount-of-places-of-discovery-in-a-recursive-way id (cdr funde) alles treffer)
    )
    ((not treffer)
      (setq alles (cons (make-fund :id id :menge 1) alles))
      (setq treffer t)
      (look-if-there-is-the-phrase-and-add-it-if-it-does-not-exist-or-add-one-to-the-amount-of-places-of-discovery-in-a-recursive-way id (cdr funde) alles treffer)
    )
    (t alles)
  )
)