(require 'ert)
(require 'adoc-mode)

;; todo
;; test for font lock multiline property 

(defun adoctest-make-buffer (&rest args)
  (set-buffer (get-buffer-create "adoctest")) 
  (delete-region (point-min) (point-max))

  (while args
    (insert (propertize (car args) 'adoctest (cadr args)))
    (setq args (cddr args)))

  (adoc-mode)
  (font-lock-fontify-buffer))

(ert-deftest adoctest-foo ()
  ;; todo
  ;;  !!!!!!!!!!!!!
  ;;  Lock Support Mode must be set to nil
  ;;  !!!!!!!!!!!!!

  ;; todo: test for presence of adoc-reserved (we do white-box testing here)
  

  ;; the white after = really is part of the delimiter
  (adoctest-make-buffer
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

   "////////" markup-meta-hide-face "\n" nil
   "comment line 1\ncomment line 2" markup-comment-face "\n" nil
   "////////" markup-meta-hide-face "\n" nil
   "\n" nil
   "++++++++" markup-meta-hide-face "\n" nil
   "passthrouh line 1\npassthrouh line 2" markup-passthrough-face "\n" nil
   "++++++++" markup-meta-hide-face "\n" nil
   "\n" nil
   "--------" markup-meta-hide-face "\n" nil
   "listing line 1\nlisting line 2" markup-code-face "\n" nil
   "--------" markup-meta-hide-face "\n" nil
   "\n" nil
   "........" markup-meta-hide-face "\n" nil
   "literal line 1\nliteral line 2" markup-verbatim-face "\n" nil
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
   "--" markup-meta-hide-face "\n" nil
   "\n" nil
   )

  (goto-char (point-min))
  (let ((not-done t))
    (while not-done
      (let* ((tmp (get-text-property (point) 'adoctest))
	     (tmp2 (get-text-property (point) 'face)))
	(when tmp
	  (ert-should (equal tmp2 tmp))))
      (if (< (point) (point-max))
	  (forward-char 1)
	(setq not-done nil)))))

(ert-run-tests-interactively "^adoctest-")

