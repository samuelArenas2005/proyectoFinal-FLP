#lang eopl
(require racket/file)
(require racket/string)
(require racket/vector)
(require racket/hash)
(require (only-in racket/base 
                    flush-output
                    read-line))

;;================================================================================
;; PROYECTO FINAL DE FLP
;; INTERPRETADOR CON COMENTARIOS, SECCIONES Y EJEMPLOS
;;--------------------------------------------------------------------------------

; Integrantes:
; 
; Samuel Arenas Valencia 202341928
; Juan Manuel Ampudia 202342174
; Nicolas David Cordoba 202343576


;;================================================================================
;; REPOSTORIO DE GITHUB
;;--------------------------------------------------------------------------------

;;https://github.com/samuelArenas2005/proyectoFinal-FLP.git

;;==========================================================================================
;;  SECCION 1 - Gramatica BNF abstracta y concreta
;;------------------------------------------------------------------------------------------
;;
;;  <programa>       ::= <expresion>    
;;                      <un-programa (exp)>
;;
;;  <expresion>     ::= <numero>
;;                      <numero-lit (num)>
;;                  ::= "\"<texto>"\"
;;                      <texto-lit (txt)>
;;                  ::= <identificador>
;;                      <var-exp (id)>
;;                  ::= (<expresion> <primitiva-binaria> <expresion>)
;;                      <primapp-bin-exp (exp1 prim-binaria exp2)>
;;                  ::= <primitiva-n-aria>(<expresion>)
;;                      <primapp-un-exp (prim-unaria exp)>
;;
;;                  Añadida en los diferentes puntos del taller
;;
;;                  ::= Si <expresion> entonces <expresion> sino <expresion> finSI
;;                      <condicional-exp (test-exp true-exp false-exp)>
;;                  ::=  declarar {<identifier> = <expresion>(;)}* {<expresion>}
;;                      <variableLocal-exp (ids exps cuerpo>
;;                  ::= procedimiento({<identificador>}*(,)) haga <expresion> finProc
;;                      <procedimiento-ex  (ids cuerpo)>
;;                  ::= evaluar <expresion> (<expresion>(,))* finEval
;;                      <app-exp (exp exps)>
;;
;;                  Gramatica llamados recursivos
;;
;;                  ::= rec {<identificador> ({<identificador>}*(,)) = <expresion>}* in <expresion>
;;                  ::= letrec {identifier ({identifier}*(,)) = <expresion>}* in <expresion>
;;                     <letrec-exp proc-names idss bodies bodyletrec>
;;
;;
;;  <primitiva-binaria>     ::= + (primitiva-suma)
;;                          ::= ~ (primitiva-resta)
;;                          ::= / (primitiva-div)
;;                          ::= * (primitiva-multi)
;;                          ::= concat (primitiva-concat)
;;
;;
;;  <primitiva-n-aria>      ::= longitud (primitiva-longitud)
;;                          ::= add1 (primitiva-add1)
;;                          ::= sub1 (primitiva-sub1)

                

;;==========================================================================================
;; ESPECIFICACIÓN LÉXICA
;;------------------------------------------------------------------------------------------

(define scanner-spec-simple-interpreter
'((espacioBlanco
   (whitespace) skip)
  (comentarios
   ("#" (arbno (not #\newline))) skip)
  (identificador
   (letter (arbno (or letter digit "?" "-" "_" ":" ))) symbol)
  (numero
   (digit (arbno digit)) number)
  (numero
   ("-" digit (arbno digit)) number)
  (numero
   (digit (arbno digit) "." (arbno digit)) number)
  (numero
   ("-" digit (arbno digit) "." (arbno digit)) number)
 ))


;;==========================================================================================
;; ESPECIFICACIÓN SINTÁCTICA (GRAMÁTICA)
;;------------------------------------------------------------------------------------------

(define grammar-simple-interpreter
  '((programa ( (arbno expresion) "return") un-programa)

    ;; Expresiones básicas
    (expresion (numero) numero-lit)
    (expresion ("\"" identificador "\"") texto-lit)
    (expresion ("(" expresion primitiva-binaria expresion ")") primapp-bin-exp)
    (expresion (primitiva-n-aria "(" (separated-list expresion ",") ")") primapp-n-exp)

    ;;-Listas

    (expresion ("[" (separated-list expresion ",") "]") list-exp)

    ;;-Diccionarios
    (expresion ("{" (separated-list expresion ":" expresion ",") "}") dictionary-exp)
    (expresion ("set" "(" identificador "," expresion ","  expresion ")")  set-exp)

    ; — Condicionales
    (expresion ("if" expresion "{" expresion (arbno expresion) "}"
                     (arbno "else if" expresion "{" expresion (arbno expresion) "}")
                     "else" "{" expresion (arbno expresion) "}")
               condicional-exp)

    ;;  — Procedimientos
    (expresion ( "lambda" "(" (separated-list identificador ",") ")" "=>" "{" ( arbno expresion )"}")
               procedimiento-ex)

    ;;  — Evaluación de procedimientos
    (expresion ("'" expresion "(" (separated-list expresion ",") ")")
               app-exp)

    ;; Implementacion de lenguaje imperativo

    (expresion (identificador id-tail) id-exp)
    
    (id-tail ("=" expresion) reassign-tail) ; Es una reasignación
    (id-tail () id-simple-tail) ;Es una variable
    (id-tail ("." "[" expresion "]" id-tail) index-tail)
    

    ;(index-suffix (";") simple-suffix)
    ;(index-suffix ("=" expresion ";") assign-suffix)


    ;;variables mutables
    (expresion  ("var" (separated-list identificador "=" expresion ",") ";")  var-exp)

    ;variables inmutables
    (expresion ("const" (separated-list identificador "=" expresion ",") ";")  const-exp)
    
    (expresion ("for" identificador "in" expresion "{" (arbno expresion) "}" ) for-exp)

    (expresion ("while" expresion "{" (arbno expresion) "}"  ) while-exp)

    (expresion ("object" identificador "=" object-exp ) prototype-exp)

    (object-exp ( "{" (separated-list identificador ":" expresion ",") "}") prototype-object )

    (object-exp ( "clone" "(" expresion ")" ) clone-object)

    (expresion ("null") null-exp)


    ;; Primitivas binarias
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("-") primitiva-resta)
    (primitiva-binaria ("*") primitiva-mult)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("%") primitiva-mod)
    (primitiva-binaria ("concat") primitiva-concat)
    

    ;; Primitivas unarias
    (primitiva-n-aria ("length") primitiva-longitud)
    (primitiva-n-aria ("++") primitiva-add1)
    (primitiva-n-aria ("--") primitiva-sub1)
    (primitiva-n-aria ("print") primitiva-print)
    (primitiva-n-aria ("input") primitiva-input)
    (primitiva-n-aria ("toInt") primitiva-toInt)

    ;;Primitivas booleanas

    (expresion ("false") false-exp)
    (expresion ("true") true-exp)

    (primitiva-binaria (">") primitiva-mayor)
    (primitiva-binaria ("<") primitiva-menor)
    (primitiva-binaria ("==") primitiva-igual)
    (primitiva-binaria (">=") primitiva-mayorIgual)
    (primitiva-binaria ("<=") primitiva-menorIgual)
    (primitiva-binaria ("!=") primitiva-diferente)

    (primitiva-binaria ("and") primitiva-y)
    (primitiva-binaria ("&&") primitiva-y2)
    (primitiva-binaria ("or") primitiva-o)
    (primitiva-binaria ("||") primitiva-o2)
    (primitiva-binaria ("xor") primitiva-xor)

    (primitiva-n-aria ("not") primitiva-not)
    (primitiva-n-aria ("!") primitiva-not2)

    ;;Primitivas de listas
    (primitiva-n-aria ("empty?") primitiva-vacio?)
    (primitiva-n-aria ("list?") primitiva-lista?)
    (primitiva-n-aria ("head") primitiva-cabeza)
    (primitiva-n-aria ("tail") primitiva-cola)
    (primitiva-n-aria ("range") primitiva-range)
    (primitiva-n-aria ("zeros") primitiva-zeros)
    
    (primitiva-binaria ("append") primitiva-append)

    ;;Primitivas de diccionarios
    (primitiva-n-aria ("diccionario?") primitiva-diccionario?)
    (primitiva-n-aria ("get") primitiva-get)
    (primitiva-n-aria ("values") primitiva-values)
    (primitiva-n-aria ("keys") primitiva-keys)

    ;;Complejos
    (primitiva-n-aria ("complex") primitiva-complex)

    ;;Clone
  ))


;;==========================================================================================
;;CONSTRUCCIÓN DEL INTÉRPRETE
;------------------------------------------------------------------------------------------

;;-----------------------------------------------------------------------------------------
;;  Construcción automática de los datatypes según la gramática y el scanner definidos
;;-----------------------------------------------------------------------------------------
(sllgen:make-define-datatypes
  scanner-spec-simple-interpreter
  grammar-simple-interpreter)


;------------------------------------------------------------------------------------------
;  Parser, Scanner e Interfaz del Intérprete
;------------------------------------------------------------------------------------------

;; Analizador léxico y sintáctico integrados
(define scan&parse
  (sllgen:make-string-parser
    scanner-spec-simple-interpreter
    grammar-simple-interpreter))

;; Solo el analizador léxico (scanner)
(define just-scan
  (sllgen:make-string-scanner
    scanner-spec-simple-interpreter
    grammar-simple-interpreter))

;; Intérprete completo: FrontEnd + Evaluación
(define interpretador
  (sllgen:make-rep-loop
    "--> "
    (lambda (pgm) (eval-programa pgm))
    (sllgen:make-stream-parser
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;; Evaluación del programa principal
(define eval-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (body)
          (eval-secuencia body (init-env))))))

(define eval-secuencia
  (lambda (exps ambi)
    (cond
      ((null? exps) (display 'void))

      (else ; Hay al menos una expresión
       (let ((primera-exp (car exps))
             (resto-exps (cdr exps)))

         (cases expresion primera-exp

          ;; CASO 1: 'var' 
           (var-exp (ids exps-def)
             (let* ((len (length ids))
                    (vec-cajas (make-vector len))
                    (mut-list (map (lambda (i) #t) (iota len))) ; #t puede mutar   
                    (nuevo-ambi (ambiente-extendido-record
                                 ids
                                 vec-cajas
                                 mut-list
                                 ambi))
                    (valores (eval-rands exps-def nuevo-ambi)))
               (for-each
                 (lambda (val pos)
                   (vector-set! vec-cajas pos val))
                 valores
                 (iota len))  
               (eval-secuencia resto-exps nuevo-ambi)))

          ;; CASO 2: 'const' (CON RECURSIÓN)
           (const-exp (ids exps-def)
             (let* ((len (length ids))
                    (vec-cajas (make-vector len))
                    (mut-list (map (lambda (i) #f) (iota len))) ; #f no mutable
                    (nuevo-ambi (ambiente-extendido-record
                                 ids
                                 vec-cajas
                                 mut-list
                                 ambi))
                    (valores (eval-rands exps-def nuevo-ambi)))             
               (for-each
                 (lambda (val pos)
                   (vector-set! vec-cajas pos val))
                 valores
                 (iota len))
               (eval-secuencia resto-exps nuevo-ambi)))

           
           (prototype-exp (id object)
                          (apply-prototype id object resto-exps ambi)
                          )
          

           ;; CASO 3: Es cualquier otra expresión
           (else
             (let ((val (eval-expresion primera-exp ambi)))
               (if (null? resto-exps)
                   val 
                   (eval-secuencia resto-exps ambi)))))
           )))))

;Funcion aux que retorna una lista de booleanos de tamaño del numero de variables o constantes declaradas
(define vals-bool
  (lambda (vals bool)
    (if (null? vals) '() (cons bool (vals-bool (cdr vals) bool)) )
    )
  )

  (define apply-prototype
  (lambda (id object resto-exps ambi)
    (cases object-exp object
      
      (prototype-object (keys-ids values-exps)
        (created-object id keys-ids values-exps resto-exps ambi) )
      
      (clone-object (exps)
        (let ((original-dic (eval-expresion exps ambi)))
          (if (dictionary? original-dic)
              
              (let* ((cloned-dic (cases dictionary original-dic
                                   (dict (keys values)
                                     (dict (vector-copy keys) (vector-copy values)))))
                     
                     (nuevo-ambi (ambiente-extendido
                                  (list id)
                                  (list cloned-dic)
                                  '(#t)
                                  ambi)))
               
                (eval-secuencia resto-exps nuevo-ambi))
              
              (eopl:error 'apply-prototype
                          "Se esperaba clonar un prototipo (diccionario), pero se obtuvo ~s"
                          original-dic))))
      )))
  
   (define created-object (lambda (obj-id keys-ids values-exps resto-exps ambi)
             (let* ((claves (map symbol->string keys-ids))
                    (obj-dic-vacio (create-dictionary '() '()))
                    (nuevo-ambi (ambiente-extendido
                                 (list obj-id)
                                 (list obj-dic-vacio)
                                 '(#t)
                                 ambi))
                    (obj-ref (buscar-variable-ref obj-id nuevo-ambi))
                    (valores (eval-rands values-exps nuevo-ambi)))
               (for-each
                (lambda (k v)
                  (let ((current-dic (deref obj-ref)))
                    (dic-set! current-dic k v obj-ref)))
                claves
                valores)
               (eval-secuencia resto-exps nuevo-ambi))))

;; Sección 2 — Ambiente Inicial
(define init-env
  (lambda ()
    (ambiente-extendido
      '(void a)
      '(null 1)
      '(#f #f)
      (ambiente-vacio))))


;------------------------------------------------------------------------------------------
;  Evaluacion de expresiones (eval-expresion)
;------------------------------------------------------------------------------------------

(define eval-expresion
  (lambda (exp ambi)
    (cases expresion exp

      ;; Sección base — Literales y variables
      (numero-lit (num) num)
      (texto-lit (txt) (symbol->string txt))
      (id-exp (id idTail) (apply-idTail idTail id ambi '()) )

      ;;Listas
      (list-exp (exps) (list->vector (eval-rands exps ambi)))

      ;;Diccionarios
      (dictionary-exp (keys values) (create-dictionary (eval-rands keys ambi) (eval-rands values ambi)))
      (set-exp (id exp1 exp2) (dic-set! (buscar-variable id ambi) (eval-rand exp1 ambi) (eval-rand exp2 ambi) (buscar-variable-ref id ambi)))
      ;; Sección base — Primitivas binarias y unarias
      (primapp-bin-exp (exp1 prim-binaria exp2)
        (apply-primitiva-bin
          (eval-expresion exp1 ambi)
          prim-binaria
          (eval-expresion exp2 ambi)))
      
      (primapp-n-exp (prim-n-aria exps)
        (apply-primitiva-n prim-n-aria (eval-rands exps ambi)))

      ;; Sección 4 — Condicionales
      (condicional-exp (cond expV restExpV otherCond otherExpV otherRestExpV expF restExpF)
        
        (if (valor-verdad? (eval-expresion cond ambi))
            (eval-secuencia (cons expV restExpV) ambi)
            (eval-condicionales otherCond 
                                otherExpV 
                                otherRestExpV 
                                expF 
                                restExpF 
                                ambi)))

      ;; Sección 6 — Definición de procedimientos
      (procedimiento-ex (ids cuerpo)
        (cerradura ids cuerpo ambi))

      ;; Sección 7 — Evaluación de procedimientos
      (app-exp (procedimiento-exp rands)
        (let ((proc (eval-expresion procedimiento-exp ambi))
              (args (eval-rands rands ambi)))
          (if (procVal? proc)
              (apply-procedure proc args)
              (eopl:error 'eval-expresion
                "Error: Intento de aplicar el valor no-procedimiento ~s" proc))))

      ;; Sección 8 — Llamados recursivos
      ;;(rec-exp (proc-names idss bodies body-rec)
        ;;(eval-expresion body-rec
          ;;(ambiente-extendido-recursivo proc-names idss bodies ambi)))

      (for-exp (id iterable-exp body-exps)
        (let* ((iterable-val (eval-expresion iterable-exp ambi))
               (lista-a-iterar (if (vector? iterable-val)
                                   (vector->list iterable-val)
                                   iterable-val)))
          (if (not (list? lista-a-iterar))
              (eopl:error 'for "La expresión 'in' de un 'for' debe ser una lista o vector, pero fue: ~s" iterable-val)
              (eval-for-helper id lista-a-iterar body-exps ambi)
              )))

      (while-exp (iterable-exp body-exp)
                 (eval-while-helper iterable-exp body-exp ambi)
                 )

      (prototype-exp (id object)
        (eopl:error 'eval-expresion
         "Definición 'object' encontrada en un lugar inesperado (donde se esperaba un valor)."))

      ;; Asignaciones

      (var-exp (ids exps-def)
        (eopl:error 'eval-expresion
          "Definición 'var' encontrada en un lugar inesperado (donde se esperaba un valor)."))

      (const-exp (ids exps-def)
        (eopl:error 'eval-expresion
          "Definición 'const' encontrada en un lugar inesperado (donde se esperaba un valor)."))

      

      ;; Valores booleanos
      (true-exp () #t)
      (false-exp () #f)
      (null-exp () (buscar-variable 'void ambi))
    )
  ))

(define eval-condicionales
  (lambda (lista-conds lista-expsV lista-restsV expF restF ambi)
    (cond
      ((null? lista-conds)
       (eval-secuencia (cons expF restF) ambi))
      
      (else
       (let ((primera-cond (car lista-conds))
             (primer-expV (car lista-expsV))
             (primer-restV (car lista-restsV)))
         
         (if (valor-verdad? (eval-expresion primera-cond ambi))
             
             (eval-secuencia (cons primer-expV primer-restV) ambi)
        
             (eval-condicionales (cdr lista-conds)
                                 (cdr lista-expsV)
                                 (cdr lista-restsV)
                                 expF
                                 restF
                                 ambi)))))))

(define eval-for-helper
  (lambda (id items-lista body-exps ambi)
    (if (null? items-lista)
        'void
        (begin
          (let* ((val-actual (car items-lista))
                 (loop-ambi (ambiente-extendido (list id) (list val-actual) '(#t) ambi)))
            
            (eval-secuencia body-exps loop-ambi))
          
          (eval-for-helper id (cdr items-lista) body-exps ambi)
          ))))

(define eval-while-helper

  (lambda (cond-exp body-exp ambi)
    (let ((cond-val (eval-expresion cond-exp ambi)))
      (if (valor-verdad? cond-val)
          (begin
            (eval-secuencia body-exp ambi)
            (eval-while-helper cond-exp body-exp ambi))
          (buscar-variable 'void ambi)
          ))))


;------------------------------------------------------------------------------------------
;  Funciones de evaluacion adicionales
;------------------------------------------------------------------------------------------

;; Evaluación de las primitivas binarias
(define apply-primitiva-bin
  (lambda (exp1 prim-binaria exp2)
    (cases primitiva-binaria prim-binaria
      (primitiva-suma () (decideOperation exp1 exp2 + complexSum))
      (primitiva-resta () (decideOperation exp1 exp2 - complexSubtract))
      (primitiva-mult () (decideOperation exp1 exp2 * complexMultiplication))
      (primitiva-div () (decideOperation exp1 exp2 / complexDivision))
      (primitiva-mod () (modulo exp1 exp2))
      (primitiva-concat () (string-append exp1 exp2) )
      (primitiva-mayor () (> exp1 exp2))
      (primitiva-menor () (< exp1 exp2))
      (primitiva-igual () (eqv? exp1 exp2))
      (primitiva-mayorIgual () (>= exp1 exp2))
      (primitiva-menorIgual () (<= exp1 exp2))
      (primitiva-diferente () (not (eqv? exp1 exp2)))
      (primitiva-y () (and exp1 exp2))
      (primitiva-y2 () (and exp1 exp2))
      (primitiva-o () (or exp1 exp2))
      (primitiva-o2 () (or exp1 exp2))
      (primitiva-xor () (not (eqv? exp1 exp2)))
      (primitiva-append () (vector-append exp1 exp2))
      )
    )
  )

;;Decide si operar como complejos o como enteros
(define decideOperation
  (lambda (arg1 arg2 intOp complexOp)
    (cond
      [(and (complex? arg1) (complex? arg2)) (complexOp arg1 arg2)]
      [(and (number? arg1) (number? arg2)) (intOp arg1 arg2)]
      [else (eopl:error 'decideOperation "Error: Se esperaban 2 numeros o 2 complejos, se recibieron ('~s', '~s')" arg1 arg2)]
      )))


;; Evaluación de las primitivas unarias
(define apply-primitiva-n
  (lambda (prim exps)
    (cases primitiva-n-aria prim
      
      (primitiva-longitud ()
        (let ((arg (get-n-arg exps 'longitud 1)))
          (string-length arg)))
          
      (primitiva-add1 ()
        (let ((arg (get-n-arg exps 'add1 1)))
          (+ arg 1)))
          
      (primitiva-sub1 ()
        (let ((arg (get-n-arg exps 'sub1 1)))
          (- arg 1)))
          
      (primitiva-not ()
        (let ((arg (get-n-arg exps 'not 1)))
          (not arg)))
          
      (primitiva-not2 ()
        (let ((arg (get-n-arg exps 'not2 1)))
          (not arg)))
          
      (primitiva-toInt ()
        (let ((arg (get-n-arg exps 'toInt 1)))
          (string->number (symbol->string arg 1))))
          
      (primitiva-print ()(begin (for-each 
            (lambda (arg)
              (display (to-external-form arg))    
              (display " "))    
            exps)
          (newline)
          'null))
          
      (primitiva-input ()
        (let ((arg (get-n-arg exps 'input 1)))
          (begin (display arg) (flush-output) (string->symbol (read-line)))))
          
      ;-Listas
      (primitiva-vacio? ()
        (let ((arg (get-n-arg exps 'empty? 1)))
          (vector-empty? arg)))
          
      (primitiva-lista? ()
        (let ((arg (get-n-arg exps 'list? 1)))
          (vector? arg))) 
          
      (primitiva-cabeza ()
        (let ((arg (get-n-arg exps 'head 1)))
          (vector-ref arg 0)))
          
      (primitiva-cola ()
        (let ((arg (get-n-arg exps 'tail 1)))
          (vector-drop arg 1)))

      (primitiva-range ()
        (let ((arg (get-n-arg exps 'range 1)))
          (letrec (
                  (build-list-helper
                    (lambda (current)
                      (if (> current (- arg 1))
                          '()
                          (cons current (build-list-helper (+ current 1)))
                          )))
                  )
                (list->vector (build-list-helper 0)))))

      (primitiva-zeros ()
        (let ((arg (get-n-arg exps 'range 1)))
          (letrec (
                  (build-list-helper
                    (lambda (count)
                      (if (<= count 0)
                          '()
                          (cons 0 (build-list-helper (- count 1)))
                          )))
                  )
                (list->vector (build-list-helper arg)))))

      ;;Diccionarios
      (primitiva-diccionario? () (let ((arg (get-n-arg exps 'diccionario? 1)))
          (dictionary? arg)))

      (primitiva-get () (let ((args (get-n-arg exps 'diccionario? 2)))
          (dic-ref (car args) (cadr args))))

      (primitiva-values () (let ((arg (get-n-arg exps 'diccionario? 1)))
          (dic-values arg)))

      (primitiva-keys () (let ((arg (get-n-arg exps 'diccionario? 1)))
          (dic-keys arg)))

      ;;Complejos
      (primitiva-complex () (let ((args (get-n-arg exps 'complex 2)))
          (a-complex (car args) (cadr args))))
      )))

(define get-n-arg
  (lambda (exps prim-name n)
    (if (and (list? exps) (= (length exps) n))
        (if (= (length exps) 1)(car exps) exps)
        (eopl:error prim-name
          "Error: solo se aceptan ~s entradas, pero se recibieron ~s"
          n (length exps)))))

(define to-external-form
  (lambda (exp)
    (cond
      [(dictionary? exp) (to-external-form-dic exp)]
      [(complex? exp) (to-external-form-complex exp)]
      [else exp]
      )))

(define to-external-form-dic
  (lambda (dic)
    (cases dictionary dic
      (dict (keys values)
        (let* ((n (vector-length keys))
               (comma (string->symbol ","))) ; símbolo de coma
          (cond
            [(zero? n) '()] ; diccionario vacío → lista vacía
            [else
             (let loop ((i 0) (acc '()))
               (if (= i n)
                   (reverse acc)
                   (let ((k (vector-ref keys i))
                         (v (vector-ref values i)))
                     (if (= i (- n 1))
                         ;; último: (k : v)
                         (loop (+ i 1) (cons v (cons ': (cons k acc))))
                         ;; intermedios: (k : v ,)
                         (loop (+ i 1)
                               (cons comma (cons v (cons ': (cons k acc)))))))))]))))))

(define to-external-form-complex
  (lambda (complex)
    (let* ((real (complex-real-p complex))
           (imag (complex-i-p complex))
           (sign (if (negative? imag) " - " " + "))
           (abs-imag (abs imag)))
      (string-append (number->string real)
                     sign
                     (number->string abs-imag)
                     "i"))))
 

(define apply-idTail
  (lambda (idTail id ambi reference)
    (cases id-tail idTail
      (reassign-tail (exp) (apply-reassign id exp ambi))
      (id-simple-tail () (buscar-variable id ambi))
      (index-tail (exp id-tail) (apply-idTail-recursive id-tail ambi (buscar-variable-ref id ambi) (eval-expresion exp ambi)))
      )))

(define apply-idTail-recursive
  (lambda (idTail ambi ref index)
    (let ((dato (deref ref)))
      (cases id-tail idTail
        (index-tail (exp id-tail)
          (cond
            [(vector? dato)
             (if (number? index)
                 (apply-idTail-recursive id-tail ambi (decide-ref index dato) (eval-expresion exp ambi))
                 (eopl:error 'idTail-recursive "el indice de una lista espera un numero, se envio ~s" index))]
            [(dictionary? dato) (apply-idTail-recursive id-tail ambi (a-ref2 index dato) (eval-expresion exp ambi))]
            [else (eopl:error 'idTail-recursive "Error: se esperaba una lista o un diccionario, se envio ~s" dato)]))
        (id-simple-tail ()
          (cond
            [(vector? dato)
             (if (number? index)
                 (vector-ref dato index)
                 (eopl:error 'idTail-recursive "el indice de una lista espera un numero, se envio ~s" index))]
            [(dictionary? dato) (dic-ref dato index)]
            [else (eopl:error 'idTail-recursive "Error: se esperaba una lista o un diccionario, se envio ~s" dato)]))
        (reassign-tail (exp)
          (cond
            [(vector? dato)
             (if (number? index)
                 (vector-set! dato index (eval-expresion exp ambi))
                 (eopl:error 'idTail-recursive "el indice de una lista espera un numero, se envio ~s" index))]
            [(dictionary? dato) (dic-set! dato index (eval-expresion exp ambi) ref)]
            [else (eopl:error 'idTail-recursive "Error: se esperaba una lista o un diccionario, se envio ~s" dato)]))
        ))))

(define apply-reassign
  (lambda (id exp ambi)
    (if (is-mutable? id ambi)
    (begin
    (setref!
           (buscar-variable-ref id ambi)
           (eval-expresion exp ambi))
           (buscar-variable 'void ambi)
    )
     (eopl:error 'apply-reassign
          "No se puede reasignar el valor de una constante")
     )
  )
)



; Seccion 7 - funciones auxiliares para aplicar eval-expression 

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expresion rand env)))

(define apply-procedure
  (lambda (proc args)
    (cases procVal proc 
      (cerradura (lista-ID body env) 
        (eval-secuencia body 
                        (ambiente-extendido lista-ID args (vals-bool lista-ID #t ) env))) 
    )))

;;==========================================================================================
;; SECCIÓN 2 — DEFINICIÓN DE LOS AMBIENTES
;;------------------------------------------------------------------------------------------
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))
;; Definición de los tipos de ambiente
(define-datatype ambiente ambiente?
  (ambiente-vacio)
  (ambiente-extendido-record (ids (list-of symbol?))
                      (vec vector?)
                      (mutable (list-of boolean?))
                      (ambi ambiente?)))

(define ambiente-extendido
  (lambda (syms vals mut env)
    (ambiente-extendido-record syms (list->vector vals) mut env)))

(define ambiente-extendido-recursivo
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (ambiente-extendido-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (cerradura ids body env)))
            (iota len) idss bodies)
          env)))))
;; Definición de la función buscar-variable

(define buscar-variable
  (lambda (var ambi)
    (deref (buscar-variable-ref var ambi))))

(define buscar-variable-ref
  (lambda (var ambi)
    (cases ambiente ambi
      (ambiente-vacio ()
                        (eopl:error 'apply-env-ref "Error, La variable ~s no existe" var))
      (ambiente-extendido-record (ids vals mutable ambi-viejo)
                           (let ((pos (list-find-position var ids)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (buscar-variable-ref var ambi-viejo))))
      )
    )
 )

(define is-mutable?
  (lambda (var ambi)
    (cases ambiente ambi
      (ambiente-vacio ()
        (eopl:error 'ambiente-vacio "Error, la variable ~s no existe" var))
      
      (ambiente-extendido-record (ids vals mutable ambi-viejo)
        (let ((pos (list-find-position var ids)))
          (if (number? pos)
              (list-ref mutable pos)
              (is-mutable? var ambi-viejo)))))))


;; Funciones auxiliares del ambiente

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (equal? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                  (+ list-index-r 1)
                  #f))))))

;;==========================================================================================
;; DEFINICIÓN DE LOS DICCIONARIOS
;;------------------------------------------------------------------------------------------

(define-datatype dictionary dictionary?
  (dict (keys vector?) (values vector?))
  )

(define schemevalue?
  (lambda (x)
    #t))

(define all-different?
  (lambda (lst)
    (cond
      [(null? lst) #t] ; Caso base: la lista vacía no tiene duplicados
      [(member (car lst) (cdr lst)) #f] ; Si el 1ro está en el resto, hay duplicado
      [else (all-different? (cdr lst))] ; Si no, revisa el resto
      )))

(define create-dictionary
  (lambda (keys values)
    (cond
      ; 1. Falla si las longitudes son diferentes
      [(not (= (length keys) (length values)))
       (eopl:error 'create-dictionary "Diferente cantidad de llaves y valores")]
      
      ; 2. Falla si las llaves no son únicas
      [(not (all-different? keys))
       (eopl:error 'create-dictionary "Las llaves contienen valores duplicados")]
      
      ; 3. Éxito: si todo lo anterior pasó, crea el dict
      [else
       (dict (list->vector keys) (list->vector values))]
      )))

(define dic-keys
  (lambda(map)
    (cases dictionary map
      (dict (keys values) keys)
      )
    )
  )

(define dic-values
  (lambda(map)
    (cases dictionary map
      (dict (keys values) values)
      )
    )
  )

(define vector-find-position
  (lambda (key vector)
    (list-find-position key (vector->list vector))))

(define dic-ref
  (lambda (map key)
    (cases dictionary map
      (dict (keys values) (let((pos (vector-find-position key (dic-keys map))))
                            (if (number? pos) (list-ref (vector->list (dic-values map)) pos) (eopl:error 'dic-ref "La llave '~s' no existe en ~s" key map))))
      )
    )
  )

(define dic-set!
  (lambda (map key value ref)
            (let* ((keys (dic-keys map)) (values (dic-values map)) (pos (vector-find-position key keys))) ; Usamos el auxiliar de vector
              (if (number? pos)
                  
                  ; --- Caso 1: La llave SÍ existe ---
                  (vector-set! values pos value)
                  
                  ; --- Caso 2: La llave NO existe ---
                  ; Extiende el diccionario. Creamos *nuevos* vectores (más largos).
                  (setref! ref (dict (vector-append keys (vector key)) (vector-append values (vector value) )))
                  ))))

;;==========================================================================================
;; DEFINICIÓN DE LOS COMPLEJOS
;;------------------------------------------------------------------------------------------

(define-datatype complex complex?
  (a-complex (real-part number?) (imaginary-part number?))
  )

(define int->complex
  (lambda (int)
    (a-complex int 0)))

(define complex-real-p
  (lambda (comp)
    (cases complex comp
      (a-complex (real i) real))))

(define complex-i-p
  (lambda (comp)
    (cases complex comp
      (a-complex (real i) i))))

;; Suma: (a+bi) + (c+di) = (a+c) + (b+d)i
(define complexSum
  (lambda (arg1 arg2)
    (let* ((a (complex-real-p arg1))
           (b (complex-i-p    arg1))
           (c (complex-real-p arg2))
           (d (complex-i-p    arg2))
           (real-part (+ a c))
           (imag-part (+ b d)))
      (a-complex real-part imag-part))))

;; Resta: (a+bi) - (c+di) = (a-c) + (b-d)i
(define complexSubtract
  (lambda (arg1 arg2)
    (let* ((a (complex-real-p arg1))
           (b (complex-i-p    arg1))
           (c (complex-real-p arg2))
           (d (complex-i-p    arg2))
           (real-part (- a c))
           (imag-part (- b d)))
      (a-complex real-part imag-part))))

;; Multiplicación: (a+bi)(c+di) = (ac - bd) + (ad + bc)i
(define complexMultiplication
  (lambda (arg1 arg2)
    (let* ((a (complex-real-p arg1))
           (b (complex-i-p    arg1))
           (c (complex-real-p arg2))
           (d (complex-i-p    arg2))
           (real-part (- (* a c) (* b d)))
           (imag-part (+ (* a d) (* b c))))
      (a-complex real-part imag-part))))

;; División: (a+bi)/(c+di) = [(ac+bd) + (bc-ad)i] / (c2 + d2)
(define complexDivision
  (lambda (arg1 arg2)
    (let* ((a (complex-real-p arg1))
           (b (complex-i-p    arg1))
           (c (complex-real-p arg2))
           (d (complex-i-p    arg2))
           (den (+ (* c c) (* d d)))              ; c2 + d2
           (real-part (/ (+ (* a c) (* b d)) den)) ; (ac + bd)/den
           (imag-part (/ (- (* b c) (* a d)) den)) ; (bc - ad)/den
           )
      (a-complex real-part imag-part))))


;;==========================================================================================
;; REFERENCIAS
;;------------------------------------------------------------------------------------------

(define-datatype reference reference?
  (a-ref (position integer?)(vec vector?))
  (a-ref2 (key schemevalue?)(dic dictionary?))
  )

(define decide-ref
  (lambda (index structure)
    (cond
      [(and (number? index) (vector? structure) (a-ref index structure))]
      [(and (schemevalue? index) (dictionary? structure) (a-ref2 index structure))]
      )
    )
  )

(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec) (vector-ref vec pos))
      (a-ref2 (key dic) (dic-ref dic key))
      )))

(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
        (vector-set! vec pos val))
      (a-ref2 (key dic)
        (dic-set! dic key val '()))
      )))
;;==========================================================================================
;; SECCIÓN 3 — BOOLEANOS
;;------------------------------------------------------------------------------------------

;; Definición de la función valor-verdad?
(define valor-verdad?
  (lambda (x)
    (cond
      [(boolean? x) x]
      [(equal? x 0) #f]
      [(equal? x "") #f]
      [(equal? x 'null) #f]
      [(equal? x (vector)) #f]
      [(and (dictionary? x) (vector-empty? (dic-keys x))) #f]
      [else #t]
)))

;;==========================================================================================
;; SECCIÓN 4 — CONDICIONALES
;;------------------------------------------------------------------------------------------
;;  Esta sección ya fue implementada en la gramática (producción condicional-exp)
;;  y en la función eval-expresion (manejo del caso condicional-exp).
;;==========================================================================================


;;==========================================================================================
;; SECCIÓN 5 — VARIABLES LOCALES
;;------------------------------------------------------------------------------------------
;;  Esta sección ya fue implementada en la gramática (producción variableLocal-exp)
;;  y en la función eval-expresion (manejo del caso variableLocal-exp).
;;==========================================================================================



;;==========================================================================================
;; SECCIÓN 6 — PROCEDIMIENTOS
;;------------------------------------------------------------------------------------------

;Tipo de dato de una cerradura (closure)
(define-datatype procVal procVal?
  (cerradura
    (lista-ID (list-of symbol?))
    (body (list-of expresion?))
    (env ambiente?)))

;;==========================================================================================
;; SECCIÓN 7 — EVALUACIÓN DE PROCEDIMIENTOS
;;------------------------------------------------------------------------------------------
;;  Esta sección ya fue implementada en la gramática (producción app-exp)
;;  y en la función eval-expresion (manejo del caso app-exp).
;;==========================================================================================

;(interpretador)

;;==========================================================================================
;; SECCIÓN 8 — LLAMADOS RECURSIVOS
;;------------------------------------------------------------------------------------------
;;  Esta sección ya fue implementada en la gramática (producción rec-exp)
;;  y en la función eval-expresion (manejo del caso rec-exp).
;;==========================================================================================

;;======================================================================
;; EJECUTOR DE ARCHIVO
;;======================================================================

;; 1. Lee el contenido de tu archivo de programa como un solo string
(define programa-como-string
 (file->string "pruebas.txt"))

;; 2. Usa tu parser existente para convertir el string en un AST
;;    (Esto asume que tu parser se llama 'scan&parse')
(define ast-del-programa
  (scan&parse programa-como-string))

;; 3. Llama a tu evaluador principal con el AST
;;    (Esto asume que 'eval-programa' es la función que
;;     recibe el AST y configura el ambiente y el almacén)
(eval-programa ast-del-programa)

;Iniciar directamente el interpretador