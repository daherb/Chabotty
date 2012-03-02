; To-Do:
; - String-Überprüfung bei Dateinamen
; Grundstruktur der ini-Datei
(defstruct abteilung
  name
  werte
)

(defstruct wert
  name
  wert
)
; Alle Abteilungen
(setq abteilungen (make-array 0 :element-type 'abteilung :adjustable t))

; ini-Datei auslesen
(defun ini-init (dateiname)
  ; Datei öffnen
  (setq inidatei (open dateiname :direction :input :if-does-not-exist :error))
  ; Alle Zeilen auslesen
  (do ((zeile nil (read-line inidatei nil inidatei)))
    ; Bei Dateiende Datei schließen
    ((eq zeile inidatei) (close inidatei))
      ; Überpfüfen ob Zeile Inhalt hat
      (cond 
        ((equal (not zeile) nil)
          ; Überprüfen ob Zeile mit [ beginnt --> Abteilungsname
          (cond ((eq (char zeile 0) #\[)           
            (adjust-array abteilungen (+ (array-total-size abteilungen) 1))
            (setf (aref abteilungen (- (array-total-size abteilungen) 1))
              (make-abteilung
                :name (string-trim '(#\[ #\]) zeile)
                :werte (make-array 0 :element-type 'wert :adjustable t)
              )
            )          
                )
          ; Sonst enthällt die Zeile einen Wert 
                (t
            '(print (array-total-size (abteilung-werte (aref abteilungen (- (array-total-size abteilungen) 1)))))
            (setq kurz (abteilung-werte (aref abteilungen (- (array-total-size abteilungen) 1))))
            (adjust-array kurz (+ (array-total-size (abteilung-werte (aref abteilungen (- (array-total-size abteilungen) 1)))) 1))                    
            (setf (aref kurz (- (length kurz) 1))
              (make-wert 
                :name (subseq zeile 0 (search "=" zeile)) 
                :wert (subseq zeile (+ (search "=" zeile) 1) (length zeile))
              )
            )                        
                )
          )
        )
        (t nil)
      )
  )
)

(defun section-by-name (name)
  (do ((ct 0 (+ 1 ct)))
    ((eq ct (length abteilungen)))
    (cond ((equal name (abteilung-name (aref abteilungen ct)))
      (return (aref abteilungen ct)))
      (t nil)
    )
    (print ct)
    (print (abteilung-name (aref abteilungen ct)))
  )
)

(ini-init "c:\boot.ini")
(print (section-by-name "boot loader"))