; =============================================================================
; GENERAL HELPERS
; =============================================================================

; List member predicate implemented as an extension of 
; the memq method, but returns #t instead of a list
(define (member? elem lst)
  (list? (memq elem lst)))

; Predicate for determining whether an instrument value is valid
; - 'instrument' is valid if its a symbol and is among valid instruments
(define (valid-instrument? instrument)
  (define (get-valid-instruments)
    '(piano organ guitar violin flute trumpet helicopter telephone))
  (and (symbol? instrument)
       (member? instrument (get-valid-instruments))))

; Predicate for determining whether a pitch value is valid
; - 'pitch' is valid if it's an integer in interval [0,127]
(define (valid-pitch? pitch)
  (and (integer? pitch)
       (and (>= pitch 0) (<= pitch 127))))

; Predicate for determining whether a duration valid is valid
; - 'duration' is valid if its an integer > 0
(define (valid-duration? duration)
  (and (integer? duration) (> duration 0)))

; Predicate for determining whether a list is a property-list
; - 'lst' is a property list if every other element is a key of type symbol
;         and there exists a value following each key
; ** plist? uses a helper function plist?-tail, so that plist? could return
;    false in case the initial list is empty. This also makes plist? tail
;    recursive
(define (plist? lst)
  (cond ((not (list? lst)) #f)
        ((null? lst) #f)
	(else (plist?-tail lst))))
(define (plist?-tail lst)
  (cond ((null? lst) #t)
	((and (symbol? (car lst))
	      (not (null? (cdr lst))))
	 (plist?-tail (cddr lst)))
	(else #f)))

; Returns the value of 'key' in 'plist'
; - 'key' a property key in a property list
; - 'plist' property list
; outputs the element following 'key' in 'plist'
(define (get-plist-val key plist)
  (cond ((null? plist) #f)
	((not (symbol? key)) (error "value of 'key' must be a symbol"))
	((not (plist? plist)) (error "value of 'plist' must be a property list"))
	((eq? (car plist) key) (car (cdr plist)))
	(else (get-plist-val key (cdr (cdr plist)))))) 

; =============================================================================
; Note
; =============================================================================

; CONSTRUCTOR 
; In: 'pitch' - integer pitch value
;        'duration' - integer duration value (number of time units)
;        'instrument' - instrument symbol
; Out: note object represented as a property list of type 'note
(define (new-note pitch duration instrument)
  (cond ((not (valid-pitch? pitch)) (error "Invalid pitch value"))
	((not (integer? duration)) (error "Invalid duration value"))
	((not (valid-instrument? instrument)) (error "Invalid instrument value"))
	(else (list 'type 'note 'pitch pitch 'duration duration 'instrument instrument))))

; PREDICATE - NOTE
; In: 'obj' - any value
; Out: #t or #f 
; ** only returns true if 'obj' is a property list that has 'type of 'note
(define (note? obj)
  (if (plist? obj)
    (eq? (get-plist-val 'type obj) 'note)
    #f))

; SELECTOR - PITCH
; In: 'note' - a note element
; Out: pitch of the note
(define (get-pitch note)
  (define (pitch) (get-plist-val 'pitch note))
  (cond ((not (note? note)) (error "Input must be a note"))
	((not (pitch)) (error "pitch attribute not found on note"))
	(else (pitch))))

; SELECTOR - DURATION
; In: 'elem' - either a note or a pause element
; Out: duration of the note/pause
(define (get-duration elem)
  (define (duration) (get-plist-val 'duration elem))
  (cond ((not (or (note? elem) (pause? elem))) (error "Input must be a note or a pause"))
	((not (duration)) (error "duration attribute not found on element"))
	(else (duration))))

; TODO Remove when pause? implemented
(define (pause? obj) #f)
; TODO Remove when pause? implemented


; SELECTOR - INSTRUMENT
; In: 'note' - a note element
; Out: instrument of the note
(define (get-instrument note)
  (define (instrument) (get-plist-val 'instrument note))
  (cond ((not (note? note)) (error "Input must be a note"))
	((not (instrument)) (error "instrument attribute not found on note"))
	(else (instrument))))



    

