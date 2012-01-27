(require 'ert)
(require 'adoc-mode)

;; todo
;; test for font lock multiline property 
;; todo
;;  !!!!!!!!!!!!!
;;  Lock Support Mode must be set to nil
;;  !!!!!!!!!!!!!

;; todo: test for presence of adoc-reserved (we do white-box testing here)


(defun adoctest-faces (name &rest args)
  (set-buffer (get-buffer-create (concat "adoctest-" name))) 
  (delete-region (point-min) (point-max))

  (while args
    (insert (propertize (car args) 'adoctest (cadr args)))
    (setq args (cddr args)))

  (adoc-mode)
  (font-lock-fontify-buffer)
  (goto-char (point-min))
  (let ((not-done t))
    (while not-done
      (let* ((tmp (get-text-property (point) 'adoctest))
	     (tmp2 (get-text-property (point) 'face)))
	(cond
	 ((null tmp)) ; nop
	 ((eq tmp 'no-face)
	  (ert-should (null tmp2)))
	 (t
	  (if (and (listp tmp2) (not (listp tmp)))
	      (ert-should (and (= 1 (length tmp2)) (equal tmp (car tmp2))))
	    (ert-should (equal tmp tmp2)))))
	(if (< (point) (point-max))
	    (forward-char 1)
	  (setq not-done nil)))))
  (kill-buffer (concat "adoctest-" name)))

(ert-deftest adoctest-test-titles-simple ()
  (adoctest-faces "titles-simple"
   "= " markup-meta-hide-face "document title" markup-title-0-face "\n" nil
   "\n" nil
   "== " markup-meta-hide-face "chapter 1" markup-title-1-face "\n" nil
   "\n" nil
   "=== " markup-meta-hide-face "chapter 2" markup-title-2-face "\n" nil
   "\n" nil
   "==== " markup-meta-hide-face "chapter 3" markup-title-3-face "\n" nil
   "\n" nil
   "===== " markup-meta-hide-face "chapter 4" markup-title-4-face "\n" nil
   "\n" nil

   "= " markup-meta-hide-face "document title" markup-title-0-face " =" markup-meta-hide-face "\n" nil
   "\n" nil
   "== " markup-meta-hide-face "chapter 1" markup-title-1-face " ==" markup-meta-hide-face "\n" nil
   "\n" nil
   "=== " markup-meta-hide-face "chapter 2" markup-title-2-face " ===" markup-meta-hide-face "\n" nil
   "\n" nil
   "==== " markup-meta-hide-face "chapter 3" markup-title-3-face " ====" markup-meta-hide-face "\n" nil
   "\n" nil
   "===== " markup-meta-hide-face "chapter 4" markup-title-4-face " =====" markup-meta-hide-face "\n" nil
   "\n" nil

   ;; todo
   ;; ensure somehow adoc-enable-two-line-title is t
   "document title" markup-title-0-face "\n" nil
   "==============" markup-meta-hide-face "\n" nil
   "\n" nil
   "chapter 1" markup-title-1-face "\n" nil
   "---------" markup-meta-hide-face "\n" nil
   "\n" nil
   "chapter 2" markup-title-2-face "\n" nil
   "~~~~~~~~~" markup-meta-hide-face "\n" nil
   "\n" nil
   "chapter 3" markup-title-3-face "\n" nil
   "^^^^^^^^^" markup-meta-hide-face "\n" nil
   "\n" nil
   "chapter 4" markup-title-4-face "\n" nil
   "+++++++++" markup-meta-hide-face "\n" nil
   "\n" nil

   "." markup-meta-face "Block title" markup-gen-face "\n" nil ))

(ert-deftest adoctest-test-delimited-blocks-simple ()
  (adoctest-faces "delimited-blocks-simple"

   ;; note that the leading spaces are NOT allowed to have adoc-align face
   "////////" markup-meta-hide-face "\n" nil
   " comment line 1\n comment line 2" markup-comment-face "\n" nil
   "////////" markup-meta-hide-face "\n" nil
   "\n" nil
   "++++++++" markup-meta-hide-face "\n" nil
   " passthrouh line 1\n passthrouh line 2" markup-passthrough-face "\n" nil
   "++++++++" markup-meta-hide-face "\n" nil
   "\n" nil
   "--------" markup-meta-hide-face "\n" nil
   " listing line 1\n listing line 2" markup-code-face "\n" nil
   "--------" markup-meta-hide-face "\n" nil
   "\n" nil
   "........" markup-meta-hide-face "\n" nil
   " literal line 1\n literal line 2" markup-verbatim-face "\n" nil
   "........" markup-meta-hide-face "\n" nil
   "\n" nil

   "________" markup-meta-hide-face "\n" nil
   "quote line 1\nquote line 2" nil "\n" nil
   "________" markup-meta-hide-face "\n" nil
   "\n" nil
   "========" markup-meta-hide-face "\n" nil
   "example line 1\nexample line 2" nil "\n" nil
   "========" markup-meta-hide-face "\n" nil
   "\n" nil
   "********" markup-meta-hide-face "\n" nil
   "sidebar line 1\nsidebar line 2" markup-secondary-text-face "\n" nil
   "********" markup-meta-hide-face "\n" nil
   "\n" nil
   "--" markup-meta-hide-face "\n" nil
   "open block line 1\nopen block line 2" nil "\n" nil
   "--" markup-meta-hide-face "\n" nil))

(ert-deftest adoctest-test-comments ()
  (adoctest-faces "comments"
    ;; as block macro 
    "// lorem ipsum\n" markup-comment-face
    "\n" nil
    ;; as inline macro
    "lorem ipsum\n" 'no-face
    "// dolor sit\n" markup-comment-face
    "amen\n" 'no-face
    "\n" nil
    ;; as delimited block
    ;; tested in delimited-blocks-simple
    ))

(ert-deftest adoctest-test-anchors ()
  (adoctest-faces "anchors"
    ;; block id
    "[[" markup-meta-face "foo" markup-anchor-face "]]" markup-meta-face "\n" nil
    "[[" markup-meta-face "foo" markup-anchor-face "," markup-meta-face
      "bar" markup-secondary-text-face "]]" markup-meta-face "\n" nil

    ;; special inline syntax: [[id]] [[id,xreftext]]
    "lorem " 'no-face "[[" markup-meta-face "foo" markup-anchor-face "]]"
      markup-meta-face "ipsum" 'no-face "\n" nil
    "lorem " 'no-face "[[" markup-meta-face "foo" markup-anchor-face "," markup-meta-face
      "bla bli bla blu" markup-secondary-text-face "]]" markup-meta-face "ipsum" 'no-face "\n" nil

    ;; general inline macro syntax
    "lorem " 'no-face "anchor" markup-command-face ":" markup-meta-face
      "foo" markup-anchor-face
      "[]" markup-meta-face "ipsum" 'no-face "\n" nil
    "lorem " 'no-face "anchor" markup-command-face ":" markup-meta-face
      "foo" markup-anchor-face
      "[" markup-meta-face "bla bli bla blu" markup-secondary-text-face "]" markup-meta-face
      "ipsum" 'no-face "\n" nil

    ;; biblio
    "lorem " 'no-face "[[" markup-meta-face "[foo]" markup-gen-face "]]" markup-meta-face
      " ipsum\n" 'no-face
    ))

(ert-deftest adoctest-test-references ()
  (adoctest-faces "references"
     "lorem " 'no-face "xref" markup-command-face ":" markup-meta-face
       "foo" markup-reference-face "[]" markup-meta-face "\n" nil
     "lorem " 'no-face "xref" markup-command-face ":" markup-meta-face
       "foo" markup-internal-reference-face "[" markup-meta-face
       "bla bli bla blu" markup-reference-face "]" markup-meta-face  "\n" nil
       ))

(ert-deftest adoctest-test-images ()
  (adoctest-faces "images"
     ;; block macros
     ;; empty arglist
     "image" markup-complex-replacement-face "::" markup-meta-face 
       "./foo/bar.png" markup-internal-reference-face
       "[]" markup-meta-face "\n" nil
     ;; pos attribute 0 = alternate text 
     "image" markup-complex-replacement-face "::" markup-meta-face 
       "./foo/bar.png" markup-internal-reference-face
       "[" markup-meta-face "lorem ipsum" markup-secondary-text-face "]" markup-meta-face "\n" nil
     ;; keyword title
     "image" markup-complex-replacement-face "::" markup-meta-face 
       "./foo/bar.png" markup-internal-reference-face
       "[" markup-meta-face "alt" markup-attribute-face "=" markup-meta-face "lorem ipsum" markup-secondary-text-face "]" markup-meta-face "\n" nil
     ;; keyword alt and title
     "image" markup-complex-replacement-face "::" markup-meta-face 
       "./foo/bar.png" markup-internal-reference-face
       "[" markup-meta-face "alt" markup-attribute-face "=" markup-meta-face "lorem ipsum" markup-secondary-text-face "," markup-meta-face
       "title" markup-attribute-face "=" markup-meta-face "lorem ipsum" markup-secondary-text-face "]" markup-meta-face "\n" nil

     ;; no everything again with inline macros
     "foo " 'no-face "image" markup-complex-replacement-face ":" markup-meta-face 
       "./foo/bar.png" markup-internal-reference-face
       "[]" markup-meta-face "bar" 'no-face "\n" nil

     "foo " 'no-face "image" markup-complex-replacement-face ":" markup-meta-face 
       "./foo/bar.png" markup-internal-reference-face
       "[" markup-meta-face "lorem ipsum" markup-secondary-text-face "]" markup-meta-face "bar" 'no-face "\n" nil

     "foo " 'no-face "image" markup-complex-replacement-face ":" markup-meta-face 
       "./foo/bar.png" markup-internal-reference-face
       "[" markup-meta-face "alt" markup-attribute-face "=" markup-meta-face "lorem ipsum" markup-secondary-text-face "]" markup-meta-face "bar" 'no-face "\n" nil

     "foo " 'no-face "image" markup-complex-replacement-face ":" markup-meta-face 
       "./foo/bar.png" markup-internal-reference-face
       "[" markup-meta-face "alt" markup-attribute-face "=" markup-meta-face "lorem ipsum" markup-secondary-text-face "," markup-meta-face
       "title" markup-attribute-face "=" markup-meta-face "lorem ipsum" markup-secondary-text-face "]" markup-meta-face "bar" 'no-face "\n" nil))

(ert-deftest adoctest-test-attribute-list ()
  (adoctest-faces "attribute-list"
    ;; positional attribute 
    "[" markup-meta-face "hello" markup-value-face "]" markup-meta-face "\n" nil 		  
    ;; positional attribute containing spaces
    "[" markup-meta-face "hello world" markup-value-face "]" markup-meta-face "\n" nil 		  
    ;; positional attribute as string
    "[\"" markup-meta-face  "hello world" markup-value-face "\"]" markup-meta-face "\n" nil 		  

    ;; multiple positional attributes
    "[" markup-meta-face "hello" markup-value-face "," markup-meta-face "world" markup-value-face "]" markup-meta-face "\n" nil 		  

    ;; keyword attribute
    "[" markup-meta-face "salute" markup-attribute-face "=" markup-meta-face "hello" markup-value-face "]" markup-meta-face "\n" nil 		  
    ;; keyword attribute where value is a string
    "[" markup-meta-face "salute" markup-attribute-face "=\"" markup-meta-face "hello world" markup-value-face "\"]" markup-meta-face "\n" nil 		  

    ;; multiple positional attributes, multiple keyword attributes
    "[" markup-meta-face "lorem" markup-value-face "," markup-meta-face "ipsum" markup-value-face "," markup-meta-face
        "dolor" markup-attribute-face "=" markup-meta-face "sit" markup-value-face "," markup-meta-face
        "dolor" markup-attribute-face "=" markup-meta-face "sit" markup-value-face "]" markup-meta-face "\n" nil 		  

    ;; is , within strings really part of the string and not mistaken as element separator
    "[\"" markup-meta-face "lorem,ipsum=dolor" markup-value-face "\"]" markup-meta-face "\n" nil 		  
    ;; does escaping " in strings work
    "[\"" markup-meta-face "lorem \\\"ipsum\\\" dolor" markup-value-face "\"]" markup-meta-face "\n" nil 		  
    ))

(ert-deftest adoctest-test-block-macro ()
  (adoctest-faces "block-macro"
    "lorem" markup-command-face "::" markup-meta-face "ipsum[]" markup-meta-face))

(ert-deftest adoctest-test-quotes-simple ()
  (adoctest-faces "test-quotes-simple"
   ;; note that in unconstraned quotes cases " ipsum " has spaces around, in
   ;; constrained quotes case it doesn't
   "Lorem " nil "`" markup-meta-hide-face "ipsum" '(markup-typewriter-face markup-verbatim-face) "`" markup-meta-hide-face " dolor\n" nil
   "Lorem " nil "+++" markup-meta-hide-face " ipsum " '(markup-typewriter-face markup-verbatim-face) "+++" markup-meta-hide-face " dolor\n" nil
   "Lorem " nil "$$" markup-meta-hide-face " ipsum " '(markup-typewriter-face markup-verbatim-face) "$$" markup-meta-hide-face " dolor\n" nil
   "Lorem " nil "**" markup-meta-hide-face " ipsum " markup-strong-face "**" markup-meta-hide-face " dolor\n" nil
   "Lorem " nil "*" markup-meta-hide-face "ipsum" markup-strong-face "*" markup-meta-hide-face " dolor\n" nil
   "Lorem " nil "``" markup-replacement-face "ipsum" nil "''" markup-replacement-face " dolor\n" nil
   "Lorem " nil "'" markup-meta-hide-face "ipsum" markup-emphasis-face "'" markup-meta-hide-face " dolor\n" nil
   "Lorem " nil "`" markup-replacement-face "ipsum" nil "'" markup-replacement-face " dolor\n" nil
   "Lorem " nil "++" markup-meta-hide-face " ipsum " markup-typewriter-face "++" markup-meta-hide-face " dolor\n" nil
   "Lorem " nil "+" markup-meta-hide-face "ipsum" markup-typewriter-face "+" markup-meta-hide-face " dolor\n" nil
   "Lorem " nil "__" markup-meta-hide-face " ipsum " markup-emphasis-face "__" markup-meta-hide-face " dolor\n" nil
   "Lorem " nil "_" markup-meta-hide-face "ipsum" markup-emphasis-face "_" markup-meta-hide-face " dolor\n" nil
   "Lorem " nil "##" markup-meta-hide-face " ipsum " markup-gen-face "##" markup-meta-hide-face " dolor\n" nil
   "Lorem " nil "#" markup-meta-hide-face "ipsum" markup-gen-face "#" markup-meta-hide-face " dolor\n" nil
   "Lorem " nil "~" markup-meta-hide-face " ipsum " markup-subscript-face "~" markup-meta-hide-face " dolor\n" nil
   "Lorem " nil "^" markup-meta-hide-face " ipsum " markup-superscript-face "^" markup-meta-hide-face " dolor\n" nil))

(ert-deftest adoctest-test-quotes-medium ()
  (adoctest-faces "test-quotes-medium"
   ;; test wheter constrained/unconstrained quotes can spawn multiple lines
   "Lorem " 'no-face "*" markup-meta-hide-face "ipsum" markup-strong-face "\n" nil
   "dolor" markup-strong-face "*" markup-meta-hide-face " sit" 'no-face "\n" nil
   "Lorem " 'no-face "__" markup-meta-hide-face "ipsum" markup-emphasis-face "\n" nil
   "dolor" markup-emphasis-face "__" markup-meta-hide-face " sit" 'no-face "\n" nil
   "\n" nil

   ;; tests border case that delimiter is at the beginnin/end of an paragraph/line
   ;; constrained at beginning
   "*" markup-meta-hide-face "lorem" 'markup-strong-face "*" markup-meta-hide-face " ipsum\n" 'no-face
   "\n" nil
   ;; constrained at end
   "lorem " 'no-face "*" markup-meta-hide-face "ipsum" markup-strong-face "*" markup-meta-hide-face "\n" nil
   "\n" nil
   ;; constrained from beginning to end
   "*" markup-meta-hide-face "lorem" markup-strong-face "*" markup-meta-hide-face "\n" nil
   "\n" nil
   ;; unconstrained at beginning. Note that "** " at the beginning of a line would be a list item.
   "__" markup-meta-hide-face " lorem " 'markup-emphasis-face "__" markup-meta-hide-face " ipsum\n" 'no-face
   "\n" nil
   ;; unconstrained at end
   "lorem " 'no-face "__" markup-meta-hide-face " ipsum " markup-emphasis-face "__" markup-meta-hide-face "\n" nil
   "\n" nil
   ;; unconstrained from beginning to end
   "__" markup-meta-hide-face " lorem " markup-emphasis-face "__" markup-meta-hide-face "\n" nil
   "\n" nil
   
   ;; test wheter quotes can nest
   ;; done by meta-face-cleanup

   ;; tests that quotes work within titles / labeled lists
   "== " markup-meta-hide-face "chapter " markup-title-1-face "*" markup-meta-hide-face "1" '(markup-title-1-face markup-strong-face) "*" markup-meta-hide-face " ==" markup-meta-hide-face "\n" nil
   "\n" nil
   "chapter " markup-title-2-face "_" markup-meta-hide-face "2" '(markup-title-2-face markup-emphasis-face) "_" markup-meta-hide-face "\n" nil
   "~~~~~~~~~~~" markup-meta-hide-face "\n" nil
   "." markup-meta-face "lorem " 'markup-gen-face "_" markup-meta-hide-face "ipsum" '(markup-gen-face markup-emphasis-face) "_" markup-meta-hide-face "\n" nil
   "\n" nil
   "lorem " markup-gen-face "+" markup-meta-hide-face "ipsum" '(markup-gen-face markup-typewriter-face) "+" markup-meta-hide-face " sit" markup-gen-face "::" markup-list-face " " adoc-align "\n" nil
   ))

;; test border cases where the quote delimiter is at the beginning and/or the
;; end of the buffer
(ert-deftest adoctest-test-quotes-medium-2 ()
  (adoctest-faces "test-quotes-medium-2"
    "*" markup-meta-hide-face "lorem" markup-strong-face "*" markup-meta-hide-face " ipsum" 'no-face))
(ert-deftest adoctest-test-quotes-medium-3 ()
  (adoctest-faces "test-quotes-medium-3"
    "lorem " 'no-face "*" markup-meta-hide-face "ipsum" markup-strong-face "*" markup-meta-hide-face))
(ert-deftest adoctest-test-quotes-medium-4 ()
  (adoctest-faces "test-quotes-medium-4"
    "*" markup-meta-hide-face "lorem" markup-strong-face "*" markup-meta-hide-face))

(ert-deftest adoctest-test-lists-simple ()
  (adoctest-faces "test-lists-simple"
   " " adoc-align "-" markup-list-face " " adoc-align "uo list item\n" nil
   " " adoc-align "*" markup-list-face " " adoc-align "uo list item\n" nil
   " " adoc-align "**" markup-list-face " " adoc-align "uo list item\n" nil
   " " adoc-align "***" markup-list-face " " adoc-align "uo list item\n" nil
   " " adoc-align "****" markup-list-face " " adoc-align "uo list item\n" nil
   " " adoc-align "*****" markup-list-face " " adoc-align "uo list item\n" nil
   "+" markup-list-face " " adoc-align "uo list item\n" nil

   " " adoc-align "1." markup-list-face " " adoc-align "o list item\n" nil
   " " adoc-align "a." markup-list-face " " adoc-align "o list item\n" nil
   " " adoc-align "B." markup-list-face " " adoc-align "o list item\n" nil
   " " adoc-align "ii)" markup-list-face " " adoc-align "o list item\n" nil
   " " adoc-align "II)" markup-list-face " " adoc-align "o list item\n" nil

   " " adoc-align "." markup-list-face " " adoc-align "implicitly numbered list item\n" nil
   " " adoc-align ".." markup-list-face " " adoc-align "implicitly numbered list item\n" nil
   " " adoc-align "..." markup-list-face " " adoc-align "implicitly numbered list item\n" nil
   " " adoc-align "...." markup-list-face " " adoc-align "implicitly numbered list item\n" nil
   " " adoc-align "....." markup-list-face " " adoc-align "implicitly numbered list item\n" nil
   "<1>" markup-list-face " " adoc-align "callout\n" nil
   "1>" markup-list-face " " adoc-align "callout\n" nil

   " " adoc-align "term" markup-gen-face "::" markup-list-face " " adoc-align "lorem ipsum\n" nil
   " " adoc-align "term" markup-gen-face ";;" markup-list-face " " adoc-align "lorem ipsum\n" nil
   " " adoc-align "term" markup-gen-face ":::" markup-list-face " " adoc-align "lorem ipsum\n" nil
   " " adoc-align "term" markup-gen-face "::::" markup-list-face " " adoc-align "lorem ipsum\n" nil
   " " adoc-align "question" markup-gen-face "??" markup-list-face "\n" nil
   "glossary" markup-gen-face ":-" markup-list-face "\n" nil

   "-" markup-list-face " " adoc-align "uo list item\n" nil
   "+" markup-meta-face "\n" nil
   "2nd list paragraph\n" nil ))

(ert-deftest adoctest-test-lists-medium ()
  (adoctest-faces "test-lists-medium"
   ;; white box test: labeled list item font lock keyword is implemented
   ;; specially in that it puts adoc-reserved text-property on the preceding
   ;; newline. However it shall deal with the situation that there is no
   ;; preceding newline, because we're at the beginning of the buffer
   "lorem" markup-gen-face "::" markup-list-face " " nil "ipsum" 'no-face))

(ert-deftest adoctest-test-inline-macros ()
  (adoctest-faces "inline-macros"
    "commandname" markup-command-face ":target[" markup-meta-face "attribute list" markup-value-face "]" markup-meta-face "\n" nil))

(ert-deftest adoctest-test-meta-face-cleanup ()
  ;; begin with a few simple explicit cases which are easier to debug in case of troubles

  ;; 1) test that meta characters always only have a single meta and don't get
  ;;    markup-bold/-emphasis/... face just because they are within a
  ;;    strongh/emphasis/... construct.
  ;; 2) test that nested quotes really apply the faces of both quotes to the inner text 
  (adoctest-faces "meta-face-cleanup-1"
    "*" markup-meta-hide-face "lorem " markup-strong-face
        "_" markup-meta-hide-face "ipsum" '(markup-strong-face markup-emphasis-face) "_" markup-meta-hide-face
    " dolor" markup-strong-face "*" markup-meta-hide-face "\n" nil)
  (adoctest-faces "meta-face-cleanup-2"
    "_" markup-meta-hide-face "lorem " markup-emphasis-face
        "*" markup-meta-hide-face "ipsum" '(markup-strong-face markup-emphasis-face) "*" markup-meta-hide-face
    " dolor" markup-emphasis-face "_" markup-meta-hide-face "\n" nil)

  ;; now test all possible cases
  ;; mmm, that is all possible cases inbetween constrained/unconstrained quotes

  ;; .... todo
  )

;; inline substitutions only within the block they belong to. I.e. don't cross
;; block boundaries.
(ert-deftest adoctest-test-inline-subst-boundaries ()
  (adoctest-faces "inline-subst-boundaries"

   ;; 1) don't cross title boundaries. 
   ;; 2) don't cross paragraph boundaries.
   ;; 3) verify that the (un)constrained quotes would work however 
   "== " markup-meta-hide-face "chapter ** 1" markup-title-1-face "\n" nil
   "lorem ** ipsum\n" 'no-face
   "\n" nil
   "lorem " 'no-face "**" markup-meta-hide-face " ipsum " markup-strong-face "**" markup-meta-hide-face "\n" nil
   "\n" nil

   "== " markup-meta-hide-face "chapter __ 1" markup-title-1-face " ==" markup-meta-hide-face "\n" nil
   "lorem __ ipsum\n" 'no-face
   "\n" nil
   "lorem " 'no-face "__" markup-meta-hide-face " ipsum " markup-emphasis-face "__" markup-meta-hide-face "\n" nil
   "\n" nil

   "chapter ++ 1" markup-title-1-face "\n" nil
   "------------" markup-meta-hide-face "\n" nil
   "lorem ++ ipsum\n" 'no-face
   "\n" nil
   "lorem " 'no-face "++" markup-meta-hide-face " ipsum " markup-typewriter-face "++" markup-meta-hide-face "\n" nil
   "\n" nil

   "." markup-meta-face "block ^title" markup-gen-face "\n" nil
   "lorem^ ipsum\n" 'no-face
   "\n" nil
   "lorem " 'no-face "^" markup-meta-hide-face " ipsum " markup-superscript-face "^" markup-meta-hide-face "\n" nil
   "\n" nil

   ;; Being able to use a ** that potentially could be mistaken as an end
   ;; delimiter as start delimiter
   "== " markup-meta-hide-face "chapter ** 1" markup-title-1-face "\n" nil
   "lorem " 'no-face "**" markup-meta-hide-face " ipsum " markup-strong-face "**" markup-meta-hide-face "\n" nil
   "\n" nil

   ;; don't cross list item boundaries
   "-" markup-list-face " " nil "lorem ** ipsum\n" 'no-face
   "-" markup-list-face " " nil "dolor ** sit\n" 'no-face
   ;; test that a quote within the list element works
   "-" markup-list-face " " nil "dolor " 'no-face "**" markup-meta-hide-face " sit " markup-strong-face "**" markup-meta-hide-face "\n" nil
   ;; dont mistake '**' list elements for quote starters/enders 
   "**" markup-list-face " " nil "lorem ** ipsum\n" 'no-face
   "**" markup-list-face " " nil "dolor ** sit\n" 'no-face
   "**" markup-list-face " " nil "dolor ** sit\n" 'no-face
   ;; don't cross list item boundaries in the case of labeled lists
   "lorem ** ipsum " markup-gen-face "::" markup-list-face " " nil "sit ** dolor\n" 'no-face
   "lorem ** ipsum " markup-gen-face "::" markup-list-face " " nil "sit ** dolor\n" 'no-face))

;; todo: also test for warnings
(ert-deftest adoctest-pre-test-byte-compile ()
  (ert-should (byte-compile-file (locate-library "adoc-mode.el" t)))
  (ert-should (load "adoc-mode.el" nil nil t))
  (ert-should (byte-compile-file (locate-library "adoc-mode-test.el" t)))
  (ert-should (load "adoc-mode-test.el" nil nil t)))

;; todo
;; - test also for multiple versions of (X)Emacs
;; - compare adoc-mode fontification with actuall output from AsciiDoc, being
;;   almost the ultimative test for correctness

(defun adoc-test-run()
  (interactive)
  (save-buffer "adoc-mode.el")
  (save-buffer "adoc-mode-test.el")
  (ert-run-tests-interactively "^adoctest-pre-test-byte-compile")
  (ert-run-tests-interactively "^adoctest-test-"))

