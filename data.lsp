
; Load the config
(load "config.lsp")

; Strukture for findings
(defstruct finding
  id
  set
)

; Load phrases and words from files
(defun read-data (words phrases)
  (setq lphrases nil)
  (setq lwords nil)
  (setq datei (open words :direction :input :if-does-not-exist :error))
  (setq lwords (read datei))
  (close datei)
  (setq datei (open phrases :direction :input :if-does-not-exist :error))
  (setq lphrases (read datei))
  (close datei)
)

; Save words and phrases
(defun save-data (words phrases)
  (setq datei (open words :direction :output :if-exists :overwrite :if-does-not-exist :create))
  (print lwords datei)
  (close datei)
  ;(save-words-rec datei (reverse lwords)) 
  (setq datei (open phrases :direction :output :if-exists :overwrite :if-does-not-exist :create))
  (print lphrases datei)
  (close datei)
)

; Add new phrase
(defun new-phrase (id phrase typ)
  (setq schonda nil)
  (do ((ct 0 (+ 1 ct)))
    ((= ct (length lphrases)))
      (cond 
        ((equal (string-downcase (cadr (nth ct lphrases))) (string-downcase phrase))
          (setq schonda t)
        )
      )
  )
  (cond 
    ((not schonda)
      (setq ltemp nil)
      (setq ltemp (cons id (cons phrase (cons typ ltemp))))
      (setq lphrases (cons ltemp lphrases))
      t
    )
    (t nil)
  )
)

; Add new word
(defun new-word (word id)
  (setq black nil)
  ; Check if word is blacklisted
  (do ((i 0 (+ 1 i)))
    ((equal i (length blacklist)))
      (cond 
        ((equal (nth i blacklist) word)
          (setq black t)
        )
      )     
  )
  (cond 
    ((not black)
      (setq ltemp nil)
      (setq ltemp (cons (string-downcase word) (cons id ltemp)))
      (setq lwords (cons ltemp lwords))
    )
  )
)

; Get Phrase by id
(defun get-phrase (id)
  (get-phrase-rec id lphrases)  
)
(defun get-phrase-rec (id data)
  (cond 
    ((not (null data))      
      (cond 
        ((equal (caar data) id)
          (return-from get-phrase-rec (cdar data))
        )
      )
      (get-phrase-rec id (cdr data))
    )
    (t nil)
  )
)

; Look for words in data
(defun find-word (sword findings)
  (find-word-rec sword lwords findings)
)
(defun find-word-rec (word data findings)
  (cond 
    ((not (null data))      
      (cond 
        ((equal (caar data) (string-downcase word))
          (setq findings (look-if-there-is-the-phrase-and-add-it-if-it-does-not-exist-or-add-one-to-the-amount-of-places-of-discovery-in-a-recursive-way (cadar data) findings findings nil))
        )
      )
      (find-word-rec word (cdr data) findings)
    )
    (t findings)
  )
)

; Self commentarial name
(defun look-if-there-is-the-phrase-and-add-it-if-it-does-not-exist-or-add-one-to-the-amount-of-places-of-discovery-in-a-recursive-way (id findings all hit) 
  (cond 
    ((not (null findings))      
      (cond 
        ((eq (fund-id (car findings)) id)
          (setf (fund-set (car findings)) (+ (fund-set (car findings)) 1))
          (setq hit t)
        )
      )
      (look-if-there-is-the-phrase-and-add-it-if-it-does-not-exist-or-add-one-to-the-amount-of-places-of-discovery-in-a-recursive-way id (cdr findings) all hit)
    )
    ((not hit)
      (setq all (cons (make-fund :id id :set 1) all))
      (setq hit t)
      (look-if-there-is-the-phrase-and-add-it-if-it-does-not-exist-or-add-one-to-the-amount-of-places-of-discovery-in-a-recursive-way id (cdr findings) all hit)
    )
    (t all)
  )
)