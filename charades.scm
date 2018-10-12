(define jogar
  (lambda ()
    (newline)
    (display "**********************")(newline)
    (display "****** CHARADAS ******")(newline)
    (display "**********************")(newline)
    (newline)
    (display "Tente adivinhar as 3 charadas num máximo de 5 tentativas para ganhar o jogo. A cada palavra descoberta, desbloqueia a pista seguinte. Boa sorte!")
    (newline)
    (display "Se precisar de uma ajuda, escreva 'dica', mas tenha atenção que gasta uma tentativa!")
    (newline)
    (newline)
    ;escolhe um conjunto de perguntas aleatório do dicionário
    (let ((niv (list-ref f (random (length f)))))
      (jogo-charadas (list-tail niv 1) (list-ref p (list-ref niv 0)) 5 1 '()))
  )
)

;;;;;;;;;;;;;;;
;;; CHARADA ;;;
;;;;;;;;;;;;;;;

(define jogo-charadas
  (lambda (frases pistas max-tentativas nivel falha)
    ;o utilizador ganha quando não há mais pistas para encriptar
    (if (null? pistas)
        (display "Muitos Parabéns! Ganhou.")
        ;se as tentativas restantes forem 0, perde
        (if (zero? max-tentativas)
            (begin
              (display "Mas infelizmente perdeu. O que importa é tentar!")
              (newline)
              (display "A palavra correta era: ")
              (display (list-ref pistas 0)))
            
            
            ;interface dos níveis
            (begin
              (display "NIVEL ")
              (display nivel)
              (newline)
              (display "- Tentativas restantes: ")
              (display max-tentativas)
              (newline)
              (display "- Tentativas falhadas: ")
              (display falha)
              (newline)
              (display "- Esta é a sua pista: ")
              (display (list-ref frases 0))
              (newline)
              (display "- Próxima pista: ")
              (display (encriptar (list-ref frases 1) (list-ref pistas 0)))
              (newline)
              (newline)
              (display "Qual é o seu palpite?")
              (newline)
              (let ((tentativa
                     (read-line)))
                
                
                ;caso o jogador pretenda uma dica
                (if (string-ci=? "dica" tentativa)
                    ;caso só reste 1 tentativa, deixa de ser possível utiliza-la
                    (if (< max-tentativas 2)
                        (begin
                          (newline)
                          (display "----------------")
                          (newline)
                          (newline)
                          (display "Já só tem 1 tentativa, não pode utilizar mais dicas!")
                          (newline)
                          (newline)
                          (jogo-charadas frases pistas max-tentativas nivel falha))
                        ;escolhe aleatoriamente entre 2 tipos de dicas
                        (dica (random 2) frases pistas max-tentativas nivel falha))
                    
                    
                    (begin
                      (newline)
                      (display "----------------")
                      (newline)
                      (newline)
                      (begin
                        (display "Nova pista: ")
                        ;desencripta a frase com a pass que ler, podendo fazer ou não sentido
                        (display (desencriptar
                                  (encriptar (list-ref frases 1) (list-ref pistas 0))
                                  tentativa))
                        (newline)
                        (newline)
                        (if (string-ci=? (list-ref pistas 0) tentativa)
                            (begin
                              (display "Fantástico!")
                              (newline)
                              (newline)
                              ;se a palavra estiver correta, vai jogar com as restantes palavras
                              (jogo-charadas (list-tail frases 1) (list-tail pistas 1) max-tentativas (add1 nivel) '()))
                            (begin
                              (display "Não me parece que a pista se torne muito útil! Tente novamente...")
                              (newline)
                              ;cobre os casos em que já só tem 1 tentativa, não propondo usar a dica
                              (if (> max-tentativas 2)
                                  (begin
                                    (display "Se precisar de uma ajuda, escreva 'dica', mas tenha atenção que gasta uma tentativa!")
                                    (newline)
                                    (newline)
                                    ;se nao estiver correta, as tentativas diminuem
                                    (jogo-charadas frases pistas (sub1 max-tentativas) nivel (cons tentativa falha)))
                                  (begin
                                    (newline)
                                    (jogo-charadas frases pistas (sub1 max-tentativas) nivel (cons tentativa falha)))))))))))))
  )
)

;;;;;;;;;;;;;;;
;;;; DICAS ;;;;
;;;;;;;;;;;;;;;

(define dica
  (lambda (num frases pistas max-tentativas nivel falha)
    (case num
      
      ;dica 1 - diz uma letra
      ((0) (begin
             (newline)
             (display "----------------")
             (newline)
             (newline)
             (display "DICA: A letra número - ")
             (let ((p (random (length (string->list (list-ref pistas 0))))))
               (display (add1 p))
               (display " - ")
               (display "corresponde a um '")
               (display (string-ref (list-ref pistas 0) p))
               (display "'."))
             (newline)
             (newline)
             (jogo-charadas frases pistas (sub1 max-tentativas) nivel falha)))
      
      ;dica 2 - diz o comprimento da palavra
      (else (begin
              (newline)
              (display "----------------")
              (newline)
              (newline)
              (display "DICA: A palavra tem ")
              (display (length (string->list (list-ref pistas 0))))
              (display " letras.")
              (newline)
              (newline)
              (jogo-charadas frases pistas (sub1 max-tentativas) nivel falha))))))


;;;;;;;;;;;;;;;
;; ENCRIPTAR ;;
;;;;;;;;;;;;;;;

;utiliza a cifra de vigenère
(define encriptar
  (lambda (mensagem pass)
      (desencadeia-parcial
       (enc
        (numerico (string->list mensagem))
        (numerico (string->list pass))))
  )
)

(define enc
  (lambda (mensagem pass)
    (if (null? mensagem)
        '()
        (append
         (list (+ (list-ref mensagem 0) (list-ref pass 0)))
         (enc (cdr mensagem) (append (cdr pass) (list (car pass))))))
  )
)

(define desencadeia-parcial
  (lambda (n)
      (list->string (literado n))
  )
)

;;;;;;;;;;;;;;;;;;
;; DESENCRIPTAR ;;
;;;;;;;;;;;;;;;;;;

(define desencriptar
  (lambda (mensagem pass)
      (encadeia-parcial
       (desenc
        (numerico (string->list mensagem))
        (numerico (string->list pass))))
  )
)

(define desenc
  (lambda (mensagem pass)
    (if (null? mensagem)
        '()
        (append
         (list (+ (- (list-ref mensagem 0) (list-ref pass 0)) 27))
         (desenc (cdr mensagem) (append (cdr pass) (list (car pass))))))
  )
)

(define encadeia-parcial
  (lambda (n)
    (list->string (literado n))
  )
)

;;;;;;;;;;;;;;;;;;;
;;; CONVERSORES ;;;
;;;;;;;;;;;;;;;;;;;

(define numerico
  (lambda (frase)
    (if (not (null? frase))
        (case (list-ref frase 0)
          ((#\a) (append (list 0) (numerico (cdr frase))))
          ((#\b) (append (list 1) (numerico (cdr frase))))
          ((#\c) (append (list 2) (numerico (cdr frase))))
          ((#\d) (append (list 3) (numerico (cdr frase))))
          ((#\e) (append (list 4) (numerico (cdr frase))))
          ((#\f) (append (list 5) (numerico (cdr frase))))
          ((#\g) (append (list 6) (numerico (cdr frase))))
          ((#\h) (append (list 7) (numerico (cdr frase))))
          ((#\i) (append (list 8) (numerico (cdr frase))))
          ((#\j) (append (list 9) (numerico (cdr frase))))
          ((#\k) (append (list 10) (numerico (cdr frase))))
          ((#\l) (append (list 11) (numerico (cdr frase))))
          ((#\m) (append (list 12) (numerico (cdr frase))))
          ((#\n) (append (list 13) (numerico (cdr frase))))
          ((#\o) (append (list 14) (numerico (cdr frase))))
          ((#\p) (append (list 15) (numerico (cdr frase)))) 
          ((#\q) (append (list 16) (numerico (cdr frase)))) 
          ((#\r) (append (list 17) (numerico (cdr frase))))
          ((#\s) (append (list 18) (numerico (cdr frase))))
          ((#\t) (append (list 19) (numerico (cdr frase))))
          ((#\u) (append (list 20) (numerico (cdr frase))))
          ((#\v) (append (list 21) (numerico (cdr frase))))
          ((#\x) (append (list 22) (numerico (cdr frase))))
          ((#\y) (append (list 23) (numerico (cdr frase))))
          ((#\w) (append (list 24) (numerico (cdr frase))))
          ((#\z) (append (list 25) (numerico (cdr frase))))
          ((#\space) (append (list 26) (numerico (cdr frase)))))
        '())
  )
)

(define literado
  (lambda (num)
    (if (not (null? num))
        (case (remainder (list-ref num 0) 27)
          ((0) (append (list #\a) (literado (cdr num))))
          ((1) (append (list #\b) (literado (cdr num))))
          ((2) (append (list #\c) (literado (cdr num))))
          ((3) (append (list #\d) (literado (cdr num))))
          ((4) (append (list #\e) (literado (cdr num))))
          ((5) (append (list #\f) (literado (cdr num))))
          ((6) (append (list #\g) (literado (cdr num))))
          ((7) (append (list #\h) (literado (cdr num))))
          ((8) (append (list #\i) (literado (cdr num))))
          ((9) (append (list #\j) (literado (cdr num))))
          ((10) (append (list #\k) (literado (cdr num))))
          ((11) (append (list #\l) (literado (cdr num))))
          ((12) (append (list #\m) (literado (cdr num))))
          ((13) (append (list #\n) (literado (cdr num))))
          ((14) (append (list #\o) (literado (cdr num))))
          ((15) (append (list #\p) (literado (cdr num)))) 
          ((16) (append (list #\q) (literado (cdr num)))) 
          ((17) (append (list #\r) (literado (cdr num))))
          ((18) (append (list #\s) (literado (cdr num))))
          ((19) (append (list #\t) (literado (cdr num))))
          ((20) (append (list #\u) (literado (cdr num))))
          ((21) (append (list #\v) (literado (cdr num))))
          ((22) (append (list #\x) (literado (cdr num))))
          ((23) (append (list #\y) (literado (cdr num))))
          ((24) (append (list #\w) (literado (cdr num))))
          ((25) (append (list #\z) (literado (cdr num))))
          ((26) (append (list #\space) (literado (cdr num)))))
        '())
  )
)


;;;;;;;;;;;;;;;;;;;
;;; DICIONARIOS ;;;
;;;;;;;;;;;;;;;;;;;

(define f
  (list
   ;lista das pistas tem de ter sempre +1 do que as palavras
   (list 0 "segundo maior pais em area da europa depois da russia" "planeta mais proximo do sol" "maior estado dos usa" "terminou")
   (list 1 "autor do quadro guernica" "pais onde se deram os jogos olimpicos mais recentes" "capital da eslovaquia" "terminou")))

(define p
  (list
   (list "ucrania" "mercurio" "alaska")
   (list "picasso" "brasil" "bratislava")))


;;;;;;;;;;;;;;;;;;;
;;;;;; ANEXO ;;;;;;
;;;;;;;;;;;;;;;;;;;

;tentativas de conversores mais eficientes

;(define literado
;  (lambda (num)
;    (if (not (null? num))
;        (append 
;         (list (integer->char (+ (remainder (list-ref num 0) 27) (char->integer #\a)))
;         (numerico (cdr num))))
;        '())
;  )
;)
; (define numerico;or maiusculas
;  (lambda (frase)
;    (if (not (null? frase))
;        (if (char-ci=? (list-ref frase 0) #\space)
;            (append 
;             '(26)
;             (numerico (cdr frase)))
;            (append
;             (list (- (char->integer (list-ref frase 0)) (sub1 (char->integer #\a))))
;             (numerico (cdr frase))))
;        '())
;  )
;)