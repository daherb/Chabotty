; To-Do:
; - Menge der übereinstimmungen in Auswahl einbauen
; Datenstrukturen laden

(load "config.lsp")
;(load "data.fas")
;(load "sio.fas")
(load "data.lsp")
(load "sio.lsp")

(read-data "word.lst" "sentence.lst")

; Handle input, add new data and find answer
(defun handle-phrase (line)
  (format t line)
  ; Type of sentence: question or statement
  (cond 
    ((search "?" line)
      (setq typ "q")
    )
    (t
      (setq typ "a")
    )
  )
  (setq id (+ (car (car lphrases)) 1))
  ; Remove special characters
  (setq line2 (string-trim '(#\, #\. #\! #\? #\") line)) ;"  
  (setq parts nil)
  ; Split line
  (cut-line-rec " " line2)
  ; Add sentence
  (new-phrase id line typ)
  (cond
    ; If not already present
    ((not schonda)
      ; Add words
      (add-words-rec id parts)
      ; Save data
      (save-data "word.lst" "sentence.lst")
    )
  )
  ; Find an answer
  (answer parts)
)

; Find suitable answer
(defun answer (parts)
  (setq funde nil)
  ; Find all occurences of words
  (setq funde (find-all-words-rec parts funde))
  (cond 
    ; Give default answer if no answer found
    ((= (length funde) 1) 
      (return-from answer standart-antwort)
    )
    ; Skip rest if only one answer founf
    ((= (length funde) 2)
      (return-from answer (car (get-phrase (fund-id (car funde)))))
    )
    (t
      ; Sort discoveries by set
      (setq funde (sort funde 'my-mengen-test))
      (setq funde (cdr funde))
      ; Cut to maximum length
      (cond 
        ((>(length funde) max-antworten)
          (nbutlast funde (- (length funde) max-antworten))
        )
      )
      (setq antworten nil)
      ; Convert sentence id to sentence
      (setq anzahl-fragen 0)
      (setq anzahl-aussagen 0)
      (do ((ct 0 (+ 1 ct)))
        ((= ct (length funde)))
        (setq antworten (cons (append (get-phrase (fund-id (nth ct funde))) (list (fund-menge (nth ct funde)))) antworten))
        ; Count number of questions and statements
        (cond
          ((equal (cadr (get-phrase (fund-id (nth ct funde)))) "a")
            (setq anzahl-aussagen (+ 1 anzahl-aussagen))
          )
          (t
            (setq anzahl-fragen (+ 1 anzahl-fragen))      
          )      
        )    
      )
      ; Align order of answers to order of discoveries
      (setq antworten (reverse antworten))    
      ; Sort answers by type
      (setq antworten (sort antworten 'my-type-test))
      ; Fraction
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
      ; Select choices for random selection
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

; Find all words in a list and merge data
(defun find-all-words-rec (words funde)
  (cond
    ((equal (not words) nil)  
      (setq funde (find-word (car words) funde))
      (find-all-words-rec (cdr words) funde)
    )
    (t funde)
  )
)

; My test to sort by set
(defun my-mengen-test (a b)
  (> (fund-menge a) (fund-menge b))
)

; My test to sort by type
(defun my-type-test (a b)
  (string> (cadr a) (cadr b))
)

; Cut and tokenize line
(defun cut-line-rec (zeichen line)
  (cond 
    ((not (search zeichen line))
      (setq parts (cons line parts))
    )
    (t
      (setq parts (cons (subseq line 0 (search zeichen line)) parts))
      (cut-line-rec zeichen (subseq line (+ (search zeichen line) 1) (length line)))
    )
  )
)

; Add words to word list
(defun add-words-rec (id list)
  (cond 
    ((equal (not list) nil)
      (new-word (car list) id)
      (add-words-rec id (cdr list))
    )
  )
)

; Do the talk
(defun chat ()
  (do ((ct 0 0))
    (nil)
      (setq line (read-line socket)) 
      (print line)
      (cond 
        ((search "privmsg" (string-downcase line))
          (cond 
            ((not (null (search "#" line)))
              (setq line (subseq line (search "#" line) (length line)))
              (setq line (subseq line (+ 1 (search " " line)) (length line)))
              (setq line (string-trim '(#\:) line))
              (setq antw (handle-phrase line))
              (format socket "PRIVMSG ~A :~A~%" irc-channel antw)
            )
          )
        )
        ((search "ping" (string-downcase line))
          (setq line (subseq line 5 (length line)))
          (format socket "PONG ~A~%" line)
        )
      )
  )
)

(irc-connect)
(chat)
