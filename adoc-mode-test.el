
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
	  (setq not-done nil))))))

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
   "dolor" markup-emphasis-face "__" markup-meta-hide-face " sit" 'no-face "\n" nil))

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

(ert-deftest adoctest-test-meta-face-cleanup ()
  ;; begin with a few simple explicit cases which are easier to debug in case of troubles
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
(ert-deftest adoctest-test-byte-compile ()
  (ert-should (byte-compile-file (locate-library "adoc-mode.el" t))))

(defun adoc-test-run()
  (interactive)
  (save-buffer "adoc-mode.el")
  (load "adoc-mode.el" nil nil t) ; really .el, not .elc
  (save-buffer "adoc-mode-test.el")
  (load-library "adoc-mode-test")
  (ert-run-tests-interactively "^adoctest-test-"))

(global-set-key [(f5)] 'adoc-test-run)
