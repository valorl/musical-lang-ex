; =============================================================================
; GENERAL HELPERS
; =============================================================================
; List member predicate implemented as an extension of 
; the memq method, but returns #t instead of a list
(define (member? elem lst)
  (list? (memq elem lst)))

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


; =============================================================================
; CONSTRUCTORS
; =============================================================================
; NOTE Constructor 
; ............................................................................
; In: 'pitch' - integer pitch value
;     'duration' - integer duration value (number of time units)
;     'instrument' - instrument symbol
; Out: note element represented as a property list starting with 'type 'note
(define (new-note pitch duration instrument)
  (cond ((not (valid-pitch? pitch)) (error "Invalid pitch value"))
	((not (integer? duration)) (error "Invalid duration value"))
	((not (valid-instrument? instrument)) (error "Invalid instrument value"))
	(else 
	  (list 'type 'note 
		'pitch pitch 
		'duration duration 
		'instrument instrument))))

; PAUSE Constructor 
; ............................................................................
; In: 'duration' - integer duration value (number of time units)
; Out: pause element represented as a property list with 'type 'pause
(define (new-pause duration)
  (cond ((not (integer? duration)) (error "Invalid duration value"))
	(else (list 'type 'pause 'duration duration))))

; SEQUENTIAL ELEMENT Constructor 
; ............................................................................
; In: 'elements' - variable length list of music elements
; Out: property list with 'type 'sequential-element and a property 'elements
;      which contains the nested music elements
(define (new-sequential-element . elements)
  (new-collection-element 'sequential-element elements))

; PARALLEL ELEMENT Constructor 
; ............................................................................
; In: 'elements' - variable length list of music elements
; Out: property list with 'type 'parallel-element and a property 'elements
;      which contains the nested music elements
(define (new-parallel-element . elements)
  (new-collection-element 'parallel-element elements))

; Generic Constructor for collection elements
; ............................................................................
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


; =============================================================================
; PREDICATES
; =============================================================================
; Generic predicate function maker 
; ............................................................................
; In: 'type' - type of the element to make a predicate for
; Out: a boolean function that accepts a paramenter 'obj' and returns 
;      #t if 'obj' is of type 'type'
(define (make-predicate type)
  (define (valid-types) '(note pause sequential-element parallel-element))
  (define (valid-type?)
    (member? type (valid-types))) 
  (if (not (valid-type?)) (error "Invalid type argument")
    (lambda (obj)
      (if (plist? obj)
        (eq? (get-plist-val 'type obj) type) 
        #f))))

; NOTE Predicate
; ............................................................................
(define (note? obj)
  ((make-predicate 'note) obj))

; PAUSE Predicate
; ............................................................................
(define (pause? obj)
  ((make-predicate 'pause) obj))

; SEQUENTIAL ELEMENT Predicate
; ............................................................................
(define (sequential-element? obj)
  ((make-predicate 'sequential-element) obj))

; PREDICATE - SEQUENTIAL ELEMENT
; ............................................................................
(define (parallel-element? obj)
  ((make-predicate 'parallel-element) obj))

; PREDICATE - MUSIC ELEMENT
; ............................................................................
; In: 'obj' - any value
; Out: #t or #f 
; ** only returns true if 'obj' is a property list that has 'type of
;    any of the four music elements
(define (music-element? obj)
  (if (plist? obj)
    (case (get-plist-val 'type obj)
      ((note pause sequential-element parallel-element) #t)
      (else #f))
    #f))

; =============================================================================
; SELECTORS
; =============================================================================
; Pitch (Note)
; ............................................................................
(define (get-pitch note)
  (define (pitch) (get-plist-val 'pitch note))
  (cond ((not (note? note)) (error "Input must be a note"))
	((not (pitch)) (error "pitch attribute not found on note"))
	(else (pitch))))

; Instrument (Note)
; ............................................................................
(define (get-instrument note)
  (define (instrument) (get-plist-val 'instrument note))
  (cond ((not (note? note)) (error "Input must be a note"))
	((not (instrument)) (error "instrument attribute not found on note"))
	(else (instrument))))

; Elements (Sequential element, Parallel element)
; ............................................................................
; In: 'element' - sequential or parallel music element
; Out: a list of elements nested inside 'element'
(define (get-elements element)
  (define (elements) (get-plist-val 'elements element))
  (cond ((not (or 
		(sequential-element? element) 
		(parallel-element? element))) 
	 (error "element must be a property list"))
	((not (elements)) (error "elements attribute not found on element"))
	(else (elements))))

; Duration (Any element)
; ............................................................................
; In: 'elem' - Any music element
; Out: the integer duration of the element
; ** In case of a collection element, the durations are recursively summed up,
;    where the duration of a 'sequenital-element is a simple sequential sum
;    and the duration of a 'parallel-element is the maximum duration out of
;        all nested music elements
(define (get-duration elem)
  
  (define (head-basic?) (or (note? elem) (pause? elem)))
  (define (basic-duration) (get-plist-val 'duration elem))
  (define (elements) (get-elements elem))
  (define (first-element) (car elem))
  (define (remaining-elements) (cdr elem))
  (define (max-of-elements) 
    (apply max (map get-duration (elements))))
  
  (cond	((head-basic?) (basic-duration)) 
	((sequential-element? elem) (get-duration (elements)))
	((parallel-element? elem) 
	 (apply max (map get-duration (elements))))
	((null? (remaining-elements)) (get-duration (first-element)))
	(else (+ 
		(get-duration (first-element)) 
		(get-duration (remaining-elements))))))

; =============================================================================
; TRANSFORMATION FUNCTIONS (transposition, scaling, reinstrumentation)
; =============================================================================
; -----------------------------------------------------------------------------
; Generic 
; -----------------------------------------------------------------------------

; Transformer maker function
; In: 'trans-note' - transformation function to apply to 'note elements
;     'trans-pause' - transformation function to apply to 'pause elements
; Out: A transformer function that accepts an element ('elem')
;      and a value ('val') and transforms 'elem' by 'val' according
;      to the provided 'trans-note' and 'trans-pause' functions
(define (make-transformer trans-note trans-pause)
  (lambda (elem val)
    (car (transform-list (list elem) trans-note trans-pause val '()))))

; Generic element-list transformer
; In: 'lst' - list of music elements
;     'trans-note' - transformation function to apply to 'note elements
;     'trans-pause' - transformation function to apply to 'pause elements
;     'val' - the value used in transformation (e.g. scaling factor)
;     'res' - list where transformed elements are iteratively accumulated
; Out: new list with all notes/pauses from 'lst' transformed according to 
;      the provided functions
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

; -----------------------------------------------------------------------------
; Transposition
; -----------------------------------------------------------------------------

; Basic transposition (note) 
; In: 'note' - a basic music note
;     'delta' - value to add to pitch
; Out: new note with 'delta' added to its pitch
(define (transpose-note note delta)
  (define (pitch) (get-plist-val 'pitch note))
  (define (new-pitch) (+ (pitch) delta))
  (cond ((not (note? note)) (error "Not a note"))
        ((not (integer? delta)) (error "Value must be an integer"))
	((or (< (new-pitch) 0) (> (new-pitch) 127)) (error "Invalid value"))
	(else (update-plist-val 'pitch (new-pitch) note))))

; Generic transposition (any element)
; In: 'elem' - any music element
;     'delta' - the value to transpose 'elem' by
; Out: new element with 'delta' added to all note pitches
(define (transpose elem delta)
  (define (transformer)
    (make-transformer
      (lambda (note val) (transpose-note note val))
      (lambda (pause val) pause)))
  ((transformer) elem delta))

; -----------------------------------------------------------------------------
; Scaling
; -----------------------------------------------------------------------------

; SCALING - BASIC ELEMENT (note, pause)
; In: 'elem' - a basic music element (note/pause)
;     'factor' - integer scaling factor
; Out: new note/pause with duration scaled by 'factor'
(define (scale-basic elem factor)
  (define (basic-element?) (or (note? elem) (pause? elem)))
  (define (duration) (get-plist-val 'duration elem))
  (cond ((not (basic-element?)) (error "Not a basic element (note/pause)"))
        ((not (integer? factor)) (error "Factor must be an integer"))
	((<= factor 0) (error "Factor must be positive"))
	(else (update-plist-val 'duration (* (duration) factor) elem))))

; Generic transposition (any element)
; In: 'elem' - any music element
;     'factor - the factor to scale 'elem' with
; Out: new element with all durations multiplied by 'factor'
(define (scale elem factor)
  (define (transformer)
    (make-transformer
      (lambda (note val) (scale-basic note val))
      (lambda (pause val) (scale-basic pause val))))
  ((transformer) elem factor))

; -----------------------------------------------------------------------------
; Reinstrumentation
; -----------------------------------------------------------------------------

; REINSTRUMENTATION - BASIC ELEMENT (note)
; In: 'note' - a note element
;     'instrument' - new instrument
; Out: new note with instrument updated to 'instrument'
(define (reinstrument-note note instrument)
  (cond ((not (note? note)) (error "Not a note"))
	((not (valid-instrument? instrument)) (error "Instrument is invalid"))
	(else (update-plist-val 'instrument instrument note))))

; Generic reinstrumentation (any element)
; In: 'elem' - any music element
;     'inst' - new instrument
; Out: new element with all note instruments set to 'inst'
(define (reinstrument elem inst)
  (define (transformer) 
    (make-transformer 
      (lambda (note val) (reinstrument-note note val))
      (lambda (pause val) pause)))
  ((transformer) elem inst))


; =============================================================================
; ABSOLUTE TIME AND FLATTENING
; =============================================================================

; Flatten an element and add absolute time 
; In: 'elem' - any music element
; Out: a flat list of elements containing 'elem and its sub-elements
;      where every note and pause objects are extended with an
;      additional 'timestamp attribute which holds the absolute time
(define (flatten-with-abs-time elem)
  (flatten-with-abs-time-h (list elem) 0 '()))
  
; Helper for (flatten-with-abs-time-h)
; In : 'lst' - list of music elements
;      'total' - the total duration at the time of calling this function
;      'res' - a flat list where all elements (with abs time) are accumulated
; Out: a flat list of notes and pauses with absolute timestamps
(define (flatten-with-abs-time-h lst total res)
  (define (head) (car lst))
  (define (tail) (cdr lst))
  (define (head-basic?) (or (note? (head)) (pause? (head))))
  (define (head-elements) (get-elements (head)))
  (define (update-head-elements new-elems)
    (update-plist-val 'elements new-elems (head)))
  (define (append-to-res elem)
    (append res (list elem)))
  (define (with-time elem timestamp)
    (cond ((not plist?) (error "add-timestamp: not a plist"))
	  (else (append elem (list 'timestamp timestamp)))))
  (define (flat-with-timestamps-parallel par-elem)
    (apply append (map 
		    (lambda (e) (flatten-with-abs-time-h (list e) total '())) 
		    (get-elements par-elem)))) 
  (cond ((null? lst) res)
	((head-basic?)
	 (flatten-with-abs-time-h 
	   (tail) (+ total (get-duration (head))) 
	   (append-to-res (with-time (head) total))))
	((sequential-element? (head))
	 (flatten-with-abs-time-h 
	   (append (head-elements) (tail))
	   total
	   res))
	((parallel-element? (head))
	 (flatten-with-abs-time-h
	   (tail)
	   (get-duration (head))
	   (append res (flat-with-timestamps-parallel (head)))))
	(else (error "Invalid input"))))








