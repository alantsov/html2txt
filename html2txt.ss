(module html2txt mzscheme
   (require (prefix h. (lib "html.ss" "html")))
   (require (prefix x. (lib "xml.ss"  "xml" )))
   (require (lib "match.ss"))
   (require (lib "list.ss"))
   
   (provide convert) 
  
   (define (in-html filename) 
     (h.read-html 
      (open-input-file filename)))
    
   (define (drop-n str)
     (list->string 
      ( filter (lambda (c) (not (char=? #\newline c) ))
               (string->list str))))
  
   (define (extract-pcdata some-content)  
      (cond  [(x.pcdata? some-content) 
                 (list (drop-n (x.pcdata-string some-content)))]
             [(x.entity? some-content)
                 '()]
             [else  
                 (extract-pcdata-from-element some-content)]))
    
    (define (convert-html-full an-html)
      (apply append (map extract-pcdata (h.html-full-content an-html))))
    
    (define (wrap-char chr)
      (list (list->string (list chr))))
    
    (define (char->string chr)
       (list->string (list chr)))
  
    (define (wrap-html an-html chr)
      (apply append 
             (list (convert-html-full an-html) 
                (wrap-char chr))))
  
    (define (extract-pcdata-from-element an-html)
      (cond
          [(h.style? an-html)
             '()]
          [(h.script? an-html)
             '()]
          [(h.br? an-html)
             (wrap-char #\newline)]
          [(h.td? an-html)
             (wrap-html an-html #\tab) ]
          [(h.tr? an-html)
             (wrap-html an-html #\newline) ]
          [(h.p? an-html)
             (wrap-html an-html #\newline) ]
          [(h.li? an-html)
            (apply append 
               (list 
                '("-")
                 (convert-html-full an-html)
                 (wrap-char  #\newline))) ]
          [(h.ul? an-html)
            (apply append 
               (list 
                 (wrap-char  #\newline)
                 (convert-html-full an-html)
                 (wrap-char  #\newline))) ]
	  [(h.html-full? an-html)
             (convert-html-full an-html)]
	  [(h.html-element? an-html) '()]
          [else '()] ))
  
    (define (get-pointers an-html)
     (cond
          [(h.a? an-html)(list (get-link an-html))]
          [(h.html-full? an-html)
            (apply append (map get-pointers  (h.html-full-content an-html)))]
          [else '()]
          ))
  
    (define (get-link an-html)
      (map 
        ( lambda (attr) 
           (apply string-append 
                  (list (x.attribute-value attr) (char->string #\newline)))) 
        ( filter 
          (lambda (attr) (eq? 'href (x.attribute-name attr)))
               (h.html-element-attributes an-html))))
  
    (define (href-list an-html) 
      (apply string-append 
             (list 
              (char->string  #\newline) 
              "links" 
              (char->string  #\newline) 
              (apply string-append
                  (apply append (get-pointers an-html))))))
  
    (define (result filename)
      (letrec ([an-html (in-html filename)])        
        (apply string-append 
          (list
            (apply string-append 
                (extract-pcdata-from-element an-html))
            (href-list an-html)))))
          
    (define (out filename)  
      (open-output-file  filename 'replace))
  
    (define (convert html-file txt-file )
      ( letrec ([res (result html-file)]
                [out-port (out "300.txt")] )
       (write-string 
          res
          out-port 
          1
          (string-length res))
       (close-output-port out-port)   
         )))
;; (require "html2txt.ss")
;;  (convert "300.html" "300.txt")