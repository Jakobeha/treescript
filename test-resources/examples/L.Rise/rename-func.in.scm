(define (test-compile name)
  (let [(base-path (string-append "~/tests/" name))
        (input (read-file (string-append base-path "-in.txt")))
        (expected (read-file (string-append base-path "-out.txt")))]
    (compile (string-append base-path ".c") :log #t)
    (let [(actual (read-file (string-append base-path "-log") :delete-after #t))]
      (assert-equal expected actual))))

(test-compile "simple")
(test-compile "fibonacci")
(test-compile "gui")
