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

; Removes the key and value of 'key' in 'plist'
; - 'key' a property key in a property list
; - 'plist' property list
; outputs a new property list without key-value pairs matching 'key'
(define (delete-plist-val key plist)
  (cond ((not (plist? plist)) (error "the list must be a property list"))
	((not (symbol? key)) (error "the key must be a symbol"))
	(else (delete-plist-val-tail key plist `()))))
(define (delete-plist-val-tail key plist res)
  (define (current-key) (car plist))
  (define (current-val) (car (cdr plist)))
  (cond ((null? plist) res)
	((eq? key (car plist))
	 (delete-plist-val-tail key (cddr plist) res))
	(else
	 (delete-plist-val-tail
	   key
	   (cddr plist)
	   (append res (list (current-key) (current-val))))))) 

; Updates the value of 'key' to 'val' in 'plist'
; - 'key' a property key in a property list
; - 'val' any value
; - 'plist' property list
; outputs a new list with all occurences of 'key' being updated with new value
(define (update-plist-val key val plist)
  (cond ((not (plist? plist)) (error "the list must be a property list"))
	((not (symbol? key)) (error "the key must be a symbol"))
	(else (update-plist-val-tail key val plist `()))))
(define (update-plist-val-tail key val plist res)
  (define (current-key) (car plist))
  (define (current-val) (car (cdr plist)))
  (cond ((null? plist) res)
	((eq? key (car plist))
	 (update-plist-val-tail
	   key val
	   (cddr plist)
	   (append res (list (current-key) val))))
	(else
	 (update-plist-val-tail
	   key val
	   (cddr plist)
	   (append res (list (current-key) (current-val))))))) 

; For-all (universal quantifier) implemented as a higher-order function
; - 'pred' predicate to check each element in 'lst' with
; - 'lst' list 
(define (for-all? pred lst)
  (cond ((null? lst) #t)
	((pred (car lst)) (for-all? pred (cdr lst)))
	(else #f)))

; =============================================================================
; Note
; =============================================================================

; CONSTRUCTOR 
; In: 'pitch' - integer pitch value
;     'duration' - integer duration value (number of time units)
;     'instrument' - instrument symbol
; Out: note element represented as a property list starting with 'type 'note
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

; SELECTOR - INSTRUMENT
; In: 'note' - a note element
; Out: instrument of the note
(define (get-instrument note)
  (define (instrument) (get-plist-val 'instrument note))
  (cond ((not (note? note)) (error "Input must be a note"))
	((not (instrument)) (error "instrument attribute not found on note"))
	(else (instrument))))


; TRANSPOSITION - NOTE
; In: 'note' - a note element
;     'delta' - pitch delta to be added to the existing pitch value
; Out: new note with pitch = pitch + delta
(define (transpose-note note delta)
  (define (pitch) (get-plist-val 'pitch note))
  (cond ((not (note? note)) (error "Not a note"))
	((not (integer? delta)) (error "The pitch to be added must be an integer"))
	(else (update-plist-val 'pitch (+ (pitch) delta) note))))




; =============================================================================
; Pause
; =============================================================================

; CONSTRUCTOR 
; In: 'duration' - integer duration value (number of time units)
; Out: pause element represented as a property list with 'type 'pause
(define (new-pause duration)
  (cond ((not (integer? duration)) (error "Invalid duration value"))
	(else (list 'type 'pause 'duration duration))))

; PREDICATE - PAUSE
; In: 'obj' - any value
; Out: #t or #f 
; ** only returns true if 'obj' is a property list that has 'type of 'pause
(define (pause? obj)
  (if (plist? obj)
    (eq? (get-plist-val 'type obj) 'pause)
    #f))

; SELECTOR - DURATION already implemented for both note and pause, line 138



; PREDICATE - MUSIC ELEMENT
; In: 'obj' - any value
; Out: #t or #f 
; ** only returns true if 'obj' is a property list that has 'type of
; any of the four music elements
(define (music-element? obj)
  (if (plist? obj)
    (case (get-plist-val 'type obj)
      ((note pause sequential-element parallel-element) #t)
      (else #f))
    #f))


; SELECTOR - DURATION	
; In: 'elem' - music element
; Out: duration of the music element
(define (get-duration elem)

; =============================================================================
; MUSIC ELEMENT FUNCTIONS 
; =============================================================================

; PREDICATE - MUSIC ELEMENT
; In: 'obj' - any value
; Out: #t or #f 
; ** only returns true if 'obj' is a property list that has 'type of
; any of the four music elements
(define (music-element? obj)
  (if (plist? obj)
    (case (get-plist-val 'type obj)
      ((note pause sequential-element parallel-element) #t)
      (else #f))
    #f))


; SELECTOR - DURATION	
; In: 'elem' - music element
; Out: duration of the music element
(define (get-duration elem)
  
  (define (invalid-input?) (not (music-element? elem)))
  (define (basic-element?) (or (note? elem) (pause? elem)))
  (define (basic-duration) (get-plist-val 'duration elem))
  (define (elements) (get-elements elem))
  (define (first-element) (car elem))
  (define (remaining-elements) (cdr elem))
  (define (max-of-elements) 
    (apply max (map get-duration (elements))))
  
  (cond	((invalid-input?) (error "Input must be a music element"))
        ((basic-elem?) (basic-duration)) 
	((sequential-element? elem) (get-duration (elements)))
	 ;(apply max (map get-duration (get-elements elem))))
	((null? (remaining-elements)) (get-duration (first-element)))
	(else (+ 
		(get-duration (first-element)) 
		(get-duration (remaining-elements))))))


; SCALING - MUSIC ELEMENT (general)
; In: 'elem' - music element
;     'factor' - a factor to multipy the durations by
; Out: New, equivalent music element with all 
;      the durations recursively scaled by 'factor'
(define (scale elem factor)
  (define (wrapped-elem) (list elem))
  (define (seed) '())
  (car (scale-list (wrapped-elem) factor (seed))))

; Tail recursive helper method for (scale elem factor)
; Takes a collection of music elements and returns a collection 
; with each element scaled by a the factor
(define (scale-list lst factor res)

  (define (head) (car lst))
  (define (tail) (cdr lst))
  (define (head-basic?) (or (note? (head)) (pause? (head))))
  (define (head-elements) (get-elements (head)))
  (define (update-head-elements new-elems) 
    (update-plist-val 'elements new-elems (head)))
  (define (append-to-res elem)
    (append res (list elem)))

  (cond ((null? lst) res) 
	((head-basic?) 
	 (scale-list (tail) factor 
		    (append-to-res (scale-basic (head) factor)))) 
	(else 
	  (scale-list (tail) factor 
		     (append-to-res 
		       (update-head-elements 
			 (scale-list (head-elements) factor '())))))))



; TRANSPOSE - MUSIC ELEMENT (general)
; In: 'elem' - music element
;     'factor' - a factor to multipy the durations by
; Out: New, equivalent music element with 'delta' recursively
;      added  to all the note pitches 
(define (transpose elem delta) 
  (define (wrapped-elem) (list elem))
  (define (seed) '())
  (car (transpose-list (wrapped-elem) delta (seed))))

(define (transpose-list lst delta res)
  
  (define (head) (car lst))
  (define (tail) (cdr lst))
  (define (head-elements) (get-elements (head)))
  (define (update-head-elements new-elems) 
    (update-plist-val 'elements new-elems (head)))
  (define (append-to-res elem)
    (append res (list elem)))

  (cond ((null? lst) res) 
	((note? (head)) 
	 (transpose-list (tail) delta 
			 (append-to-res (transpose-note (head) delta)))) 
	((pause? (head))
	 (transpose-list (tail) delta 
			 (append-to-res (head))))
	(else 
	  (transpose-list (tail) delta 
			  (append-to-res 
			    (update-head-elements 
			      (transpose-list (head-elements) delta '())))))))
  

; TRANSPOSE - MUSIC ELEMENT (general)
; In: 'elem' - music element
;     'factor' - a factor to multipy the durations by
; Out: New, equivalent music element with 'delta' recursively
;      added  to all the note pitches 
(define (reinstrument elem inst) 
  (define (wrapped-elem) (list elem))
  (define (seed) '())
  (car (reinstrument-list (wrapped-elem) inst (seed))))

(define (reinstrument-list lst inst res)
  
  (define (head) (car lst))
  (define (tail) (cdr lst))
  (define (head-elements) (get-elements (head)))
  (define (update-head-elements new-elems)
    (update-plist-val 'elements new-elems (head)))
  (define (append-to-res elem)
    (append res (list elem)))

  (cond ((null? lst) res)
	((note? (head))
	 (reinstrument-list (tail) inst
			 (append-to-res (reinstrument-note (head) inst))))
	((pause? (head))
	 (reinstrument-list (tail) inst
			 (append-to-res (head))))
	(else
	  (reinstrument-list (tail) inst
			  (append-to-res
			    (update-head-elements
			      (reinstrument-list (head-elements) inst '())))))))

(define (reinstrument elem inst)
  (define (transformer) 
    (make-transformer 
      (lambda (note val) (reinstrument-note note val))
      (lambda (pause val) pause)))
  ((transformer) elem inst))

(define (make-transformer trans-note trans-pause)
  (lambda (elem val)
    (transform-list (list elem) trans-note trans-pause val '())))

(define (transform-list lst trans-note trans-pause val res)
  
  (define (head) (car lst))
  (define (tail) (cdr lst))
  (define (head-elements) (get-elements (head)))
  (define (update-head-elements new-elems)
    (update-plist-val 'elements new-elems (head)))
  (define (append-to-res elem)
    (append res (list elem)))

  (cond ((null? lst) res)
	((note? (head))
	 (transform-list (tail) trans-note trans-pause val
			 (append-to-res (trans-note (head) val))))
	((pause? (head))
	 (transform-list (tail) trans-note trans-pause val
			 (append-to-res (trans-pause (head) val))))
	(else
	  (transform-list (tail) trans-note trans-pause val
			  (append-to-res
			    (update-head-elements
			      (transform-list 
				(head-elements) trans-note trans-pause val '())))))))

; SCALING - BASIC ELEMENT (note, pause)
; In: 'elem' - a basic music element (note/pause)
;     'factor' - integer scaling factor
; Out: new note/pause  with duration scaled by 'factor'
(define (scale-basic elem factor)
  (define (basic-element?) (or (note? elem) (pause? elem)))
  (define (duration) (get-plist-val 'duration elem))
  (cond ((not (basic-element?)) (error "Not a basic element (note/pause)"))
        ((not (integer? factor)) (error "Factor must be an integer"))
	((<= factor 0) (error "Factor must be positive"))
	(else (update-plist-val 'duration (* (duration) factor) elem))))

; TRANSPOSE - BASIC ELEMENT (note)
; In: 'note' - a basic music note
;     'delta' - value to add to pitch
; Out: new note with duration scaled by 'factor'
(define (transpose-note note delta)
  (define (pitch) (get-plist-val 'pitch note))
  (define (new-pitch) (+ (pitch) delta))
  (cond ((not (note? note)) (error "Not a note"))
        ((not (integer? delta)) (error "Value must be an integer"))
	((or (< (new-pitch) 0) (> (new-pitch) 127)) (error "Invalid value"))
	(else (update-plist-val 'pitch (new-pitch) note))))

; REINSTRUMENTATION - BASIC ELEMENT (note)
; In: 'note' - a note element
;     'instrument' - new instrument
; Out: new note with instrument updated to 'instrument'
(define (reinstrument-note note instrument)
  (cond ((not (note? note)) (error "Not a note"))
	((not (valid-instrument? instrument)) (error "Instrument is invalid"))
	(else (update-plist-val 'instrument instrument note))))

; SELECTOR - ELEMENTS - For SEQUENTIAL and PARALLEL elements
; In: 'element' - a sequential or parallel element

; Out: elements nested inside 'element'
(define (get-elements element)
  (define (elements) (get-plist-val 'elements element))
  (cond ((not (or 
		(sequential-element? element) 
		(parallel-element? element))) 
	 (error "element must be a property list"))
	((not (elements)) (error "elements attribute not found on element"))
	(else (elements))))

; General constructor for collection-type elements (sequential/parallel)
; In: 'type' - type of the element (sequential/parallel)
;     'elements' - list of elements to be included in the collection
; Out: either a sequential element or parallel element property list
(define (new-collection-element type elements)
  (define (all-elements-music?)
    (for-all? 
      (lambda (element) (music-element? element))
      elements))
  (define (valid-type?)
    (or 
      (eq? type 'sequential-element) 
      (eq? type 'parallel-element)))
  (cond ((not (valid-type?)) (error "Invalid collection element type")) 
        ((not (list? elements)) (error "elements must be a list"))
	((not (all-elements-music?)) 
	 (error "Input list must only contain music elements"))
	(else (list 'type type 'elements elements))))

; Predicate builder for music elements
(define (make-predicate type)
  (define (valid-types) '(note pause sequential-element parallel-element))
  (define (valid-type?)
    (member? type (valid-types))) 
  (if (not (valid-type?)) (error "Invalid type argument")
    (lambda (obj)
      (if (plist? obj)
        (eq? (get-plist-val 'type obj) type) 
        #f))))
  



; =============================================================================
; SEQUENTIAL ELEMENT
; =============================================================================

; CONSTRUCTOR 
; In: 'elements' - variable length list of music elements
; Out: property list with 'type 'sequential-element and a property 'elements
;      which contains the nested music elements
(define (new-sequential-element . elements)
  (new-collection-element 'sequential-element elements))

; PREDICATE - SEQUENTIAL ELEMENT
; In: 'obj' - any value
; Out: #t or #f 
; ** only returns true if 'obj' is a property list
;    that has 'type of 'sequential-element
(define (sequential-element? obj)
  (if (plist? obj)
    (eq? (get-plist-val 'type obj) 'sequential-element)
    #f))

; =============================================================================
; PARALLEL ELEMENT
; =============================================================================

; CONSTRUCTOR 
; In: 'elements' - variable length list of music elements
; Out: property list with 'type 'parallel-element and a property 'elements
;      which contains the nested music elements
(define (new-parallel-element . elements)
  (new-collection-element 'parallel-element elements))

; PREDICATE - SEQUENTIAL ELEMENT
; In: 'obj' - any value
; Out: #t or #f 
; ** only returns true if 'obj' is a property list
;    that has 'type of 'parallel-element
(define (parallel-element? obj)
  ((make-predicate 'parallel-element) obj))
			  
			  









