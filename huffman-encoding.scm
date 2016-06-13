(define (count-occurences cmp-op el lst)
  (length (filter (lambda(x) (cmp-op el x)) lst)))

(define (custom-member? cmp-op x lst)
  (cond
    ((null? lst) #f)
    ((cmp-op (car lst) x) #t)
    (else (custom-member? cmp-op x (cdr lst)))))

(define (remove-duplicates cmp-op lst)
  (foldr (lambda (e a)
           (if (not (custom-member? cmp-op e a))
               (cons e a)
               a))
         '()
         lst))

(define (frequency-list cmp-op lst)
  (map (lambda (x) (list x (count-occurences cmp-op x lst))) (remove-duplicates cmp-op lst)))

(define (alphabet cmp-op lst)
  (remove-duplicates cmp-op lst))

(define (create-leaf item weight)
  (list 'leaf item weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (leaf-item x) (cadr x))

(define (leaf-weight x) (caddr x))

(define (create-tree left-subtree right-subtree)
  (list left-subtree
        right-subtree
        (append (tree-items left-subtree) (tree-items right-subtree))
        (+ (tree-weight left-subtree) (tree-weight right-subtree))))

(define (left-subtree tree) (car tree))

(define (right-subtree tree) (cadr tree))

(define (tree-items tree)
  (if (leaf? tree)
      (list (leaf-item tree))
      (caddr tree)))

(define (tree-weight tree)
  (if (leaf? tree)
      (leaf-weight tree)
      (cadddr tree)))

(define (choose-subtree bit tree)
  (cond ((= bit 0) (left-subtree tree))
        ((= bit 1) (right-subtree tree))))

(define (selection-sort-leaves cmp-op leaves)
   (cond ((null? leaves) '() )
         (else (cons (smallest-leaf leaves)
                     (selection-sort-leaves cmp-op
                                            (remove-leaf cmp-op (smallest-leaf leaves) leaves))))))

(define (smallest-leaf ls)
  (define (helper leaves min-leaf)
    (cond ((null? leaves) min-leaf)
          ((< (weight-leaf (car leaves))
              (weight-leaf min-leaf)) (helper (cdr leaves) (car leaves)))
          (else (helper (cdr leaves) min-leaf))))
  (if (null? ls)
      '()
      (helper (cdr ls) (car ls))))

(define (remove-leaf cmp-op leaf-for-removal leaves)
  (cond ((null? leaves) '())
        ((cmp-op (leaf-item (car leaves)) (leaf-item leaf-for-removal)) (remove-leaf cmp-op leaf-for-removal (cdr leaves)) )
        (else (cons (car leaves) (remove-leaf cmp-op leaf-for-removal (cdr leaves))))))

(define (insert-in-correct-position x set)
  (cond ((null? set) (list x))
        ((< (tree-weight x) (tree-weight (car set))) (cons x set))
        (else (cons (car set)
                    (insert-in-correct-position x (cdr set))))))

(define (insertion-sort leaves)
  (if (null? leaves)
      '()
      (let ((leaf (car leaves)))
        (insert-in-correct-position leaf
                                         (insertion-sort (cdr leaves))))))

(define (ordered-leaf-set cmp-op lst)
  (let* ((frequency-pairs (frequency-list cmp-op lst))
         (unordered-leaf-set (map (lambda (x) (create-leaf
                                               (car x)
                                               (cadr x)))
                                  (frequency-list cmp-op lst))))
    (insertion-sort unordered-leaf-set)))

(define (huffman-tree cmp-op lst)
  (merge (ordered-leaf-set cmp-op lst )))

(define (merge set)
  (cond ((= (length set) 1) (car set))
        (else (merge (insert-in-correct-position (create-tree (car set) (cadr set)) (cddr set))))))

(define (all-encodings tree)
  (define (helper encoding tree)
    (cond ((leaf? tree) (list (cons (leaf-item tree) (reverse encoding))))
          (else (append (helper (append (list 0) encoding) (left-subtree tree))
                      (helper (append (list 1) encoding) (right-subtree tree))))))
  (helper '() tree))

(define (decode bits tree)
  (define (decode-helper bits current-branch)
    (if (null? bits)
        '()
        (let ((next-subtree
               (choose-subtree (car bits) current-branch)))
          (if (leaf? next-subtree)
              (cons (leaf-item next-subtree)
                    (decode-helper (cdr bits) tree))
              (decode-helper (cdr bits) next-subtree)))))
  (decode-helper bits tree))

(define (choose-subtree bit tree)
  (cond ((= bit 0) (left-subtree tree))
        ((= bit 1) (right-subtree tree))))

(define (encode message cmp-op)
  (define (encode-helper message encodings)
    (cond ((null? message) '())
          (else (cons (cdr (assq (car message) encodings))
                      (encode-helper (cdr message) encodings)))))
  (if (null? message)
      '()
      (let* ((tree (huffman-tree cmp-op message))
             (encodings (all-encodings tree)))
        (cons tree (apply append (encode-helper message encodings))))))

(define t1
  (create-tree (create-leaf 'A 4)
                       (create-tree
                        (create-leaf 'B 2)
                        (create-tree (create-leaf 'D 1)
                                             (create-leaf 'C 1)))))

(define m2 '(A D A B B C A))
(define t2 (huffman-tree eq? m2))

(define m3 '(B A C A D A E A F A B B A A A G A H))
(define t3 (huffman-tree eq? m3))

(define b4 '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(define m4 '(A D A B B C A))

(define (run)
    (display "Hello, dear cryptographer.Choose what to do?\n1)encode\n2)decode\n")
    (define choice (read))
    (process-answer choice))

(define (process-answer x)
  (cond ((eq? x 1) (encode-routine))
        ((eq? x 2) (decode-routine))
        (else (run))))

(define (encode-routine)
  (display "Enter a filepath to read the message for encoding\n")
  (define file-path (read))
  (define message (call-with-input-file file-path
                    (lambda (p)
                      (let f ((x (read p)))
                        (if (eof-object? x)
                            '()
                            (cons x (f (read p))))))))
  (display "You are encoding:")
  (display message)
  (newline)
  (display "Enter a compare function (= , eq? eqv?)\n")
  (define cmp-op (read))
  (display "Do you want to save your results(y/n)\n")
  (define save-choice (read))
  (define result (encode message (cond ((eq? cmp-op '=) =)
                                       ((eq? cmp-op 'eq?) eq?)
                                       ((eq? cmp-op 'eqv?) eqv?))))
  (if (eq? save-choice 'y)
       (write-result-to-file result)
       '())
  result)

(define (decode-routine)
  (display "Enter a file path to read the encoded message from. \n")
  (define file-path (read))
  (define encoded-bit-message (call-with-input-file file-path
                                (lambda (p)
                                  (let f ((x (read p)))
                                    (if (eof-object? x)
                                        '()
                                        (cons x (f (read p))))))))
  (display "You are decoding:")
  (display (cdar encoded-bit-message))
  (newline)
  (display "With huffman tree:\n")
  (display (caar encoded-bit-message))
  (newline)
  (decode  (cdar encoded-bit-message) (caar encoded-bit-message)))

(define (write-result-to-file result)
  (display "Enter a destination to save the result\n")
  (define destination (read))
  (let ((out (open-output-file destination)))
    (write result out)
    (close-output-port out)))