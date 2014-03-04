;;Using rainbow delimiters as a basis for plugin
;;https://github.com/jlr/closure-bricks
;;Inspired by Scheme Bricks http://www.pawfal.org/dave/blog/tag/scheme-bricks/

(eval-when-compile (require 'cl))

;;; Customize interface:

(defgroup closure-bricks nil
  "Highlight nested parentheses, brackets, and braces according to their depth."
  :prefix "closure-bricks-"
  :link '(url-link :tag "Website for closure-bricks (EmacsWiki)"
                   "http://www.emacswiki.org/emacs/RainbowDelimiters")
  :group 'applications)

(defgroup closure-bricks-faces nil
  "Faces for successively nested pairs of delimiters.

When depth exceeds innermost defined face, colors cycle back through."
  :tag "Color Scheme"
  :group 'closure-bricks
  :link '(custom-group-link "closure-bricks")
  :link '(custom-group-link :tag "Toggle Delimiters" "closure-bricks-toggle-delimiter-highlighting")
  :prefix 'closure-bricks-faces-)

;; Choose which delimiters you want to highlight in your preferred language:

(defgroup closure-bricks-toggle-delimiter-highlighting nil
  "Choose which delimiters to highlight."
  :tag "Toggle Delimiters"
  :group 'closure-bricks
  :link '(custom-group-link "closure-bricks")
  :link '(custom-group-link :tag "Color Scheme" "closure-bricks-faces"))

(defcustom closure-bricks-highlight-parens-p t
  "Enable highlighting of nested parentheses -- ().

Non-nil (default) enables highlighting of parentheses.
Nil disables parentheses highlighting."
  :tag "Highlight Parentheses?"
  :type 'boolean
  :group 'closure-bricks-toggle-delimiter-highlighting)

(defcustom closure-bricks-highlight-brackets-p t
  "Enable highlighting of nested brackets -- [].

Non-nil (default) enables highlighting of brackets.
Nil disables bracket highlighting."
  :tag "Highlight Brackets?"
  :type 'boolean
  :group 'closure-bricks-toggle-delimiter-highlighting)

(defcustom closure-bricks-highlight-braces-p t
  "Enable highlighting of nested braces -- {}.

Non-nil (default) enables highlighting of braces.
Nil disables brace highlighting."
  :tag "Highlight Braces?"
  :type 'boolean
  :group 'closure-bricks-toggle-delimiter-highlighting)


;;; Faces:

;; Unmatched delimiter face:
(defface closure-bricks-unmatched-face
  '((((background light)) (:background "#88090B"))
    (((background dark)) (:background "#88090B")))
  "Face to highlight unmatched closing delimiters in."
  :group 'closure-bricks-faces)

;; Faces for highlighting delimiters by nested level:
(defface closure-bricks-depth-1-face
  '((((background light)) (:background "grey55"))
    (((background dark)) (:background "grey55")))
  "Nested delimiters face, depth 1 - outermost set."
  :tag "Rainbow Delimiters Depth 1 Face -- OUTERMOST"
  :group 'closure-bricks-faces)

(defface closure-bricks-depth-2-face
  '((((background light)) (:background "#93a8c6"))
    (((background dark)) (:background "#93a8c6")))
  "Nested delimiters face, depth 2."
  :group 'closure-bricks-faces)

(defface closure-bricks-depth-3-face
  '((((background light)) (:background "#b0b1a3"))
    (((background dark)) (:background "#b0b1a3")))
  "Nested delimiters face, depth 3."
  :group 'closure-bricks-faces)

(defface closure-bricks-depth-4-face
  '((((background light)) (:background "#97b098"))
    (((background dark)) (:background "#97b098")))
  "Nested delimiters face, depth 4."
  :group 'closure-bricks-faces)

(defface closure-bricks-depth-5-face
  '((((background light)) (:background "#aebed8"))
    (((background dark)) (:background "#aebed8")))
  "Nested delimiters face, depth 5."
  :group 'closure-bricks-faces)

(defface closure-bricks-depth-6-face
  '((((background light)) (:background "#b0b0b3"))
    (((background dark)) (:background "#b0b0b3")))
  "Nested delimiters face, depth 6."
  :group 'closure-bricks-faces)

(defface closure-bricks-depth-7-face
  '((((background light)) (:background "#90a890"))
    (((background dark)) (:background "#90a890")))
  "Nested delimiters face, depth 7."
  :group 'closure-bricks-faces)

(defface closure-bricks-depth-8-face
  '((((background light)) (:background "#a2b6da"))
    (((background dark)) (:background "#a2b6da")))
  "Nested delimiters face, depth 8."
  :group 'closure-bricks-faces)

(defface closure-bricks-depth-9-face
  '((((background light)) (:background "#9cb6ad"))
    (((background dark)) (:background "#9cb6ad")))
  "Nested delimiters face, depth 9."
  :group 'closure-bricks-faces)


;;; Faces 10+:
;; NOTE: Currently unused. Additional faces for depths 9+ can be added on request.

(defconst closure-bricks-max-face-count 9
  "Number of faces defined for highlighting delimiter levels.

Determines depth at which to cycle through faces again.")

;;; Face utility functions

(defsubst closure-bricks-depth-face (depth)
  "Return face-name for DEPTH as a string 'closure-bricks-depth-DEPTH-face'.

For example: 'closure-bricks-depth-1-face'."
  (concat "closure-bricks-depth-"
          (number-to-string
           (or
            ;; Our nesting depth has a face defined for it.
            (and (< depth closure-bricks-max-face-count)
                 depth)
            ;; Deeper than # of defined faces; cycle back through to beginning.
            ;; Depth 1 face is only applied to the outermost delimiter pair.
            ;; Cycles infinitely through faces 2-9.
            (let ((cycled-depth (mod depth closure-bricks-max-face-count)))
              (if (/= cycled-depth 0)
                  ;; Return face # that corresponds to current nesting level.
                  (mod depth closure-bricks-max-face-count)
                ;; Special case: depth divides evenly into max, correct face # is max.
                closure-bricks-max-face-count))))
          "-face"))


;;; Nesting level

(defvar closure-bricks-all-delimiters-syntax-table nil
  "Syntax table (inherited from buffer major-mode) which uses all delimiters.

When closure-bricks-minor-mode is first activated, it sets this variable and
the other closure-bricks specific syntax tables based on the current
major-mode. The syntax table is constructed by the function
'closure-bricks-make-syntax-table-all-delimiters'.")

;; syntax-table: used with parse-partial-sexp for determining current depth.
(defun closure-bricks-make-syntax-table-all-delimiters (syntax-table)
  "Inherit SYNTAX-TABLE and add delimiters intended to be highlighted by mode."
  (let ((table (copy-syntax-table syntax-table)))
    (modify-syntax-entry ?\( "()  " table)
    (modify-syntax-entry ?\) ")(  " table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    table))

(defun closure-bricks-depth (point)
  "Return # of nested levels of parens, brackets, braces POINT is inside of."
  (save-excursion
    (beginning-of-defun)
    (let ((depth
           (with-syntax-table closure-bricks-all-delimiters-syntax-table
             (car (parse-partial-sexp (point) point)))))
      (if (>= depth 0)
          depth
        0)))) ; ignore negative depths created by unmatched closing parens.


;;; Text properties

;; Backwards compatibility: Emacs < v23.2 lack macro 'with-silent-modifications'.
(eval-and-compile
  (unless (fboundp 'with-silent-modifications)
    (defmacro with-silent-modifications (&rest body)
      "Defined by closure-bricks.el for backwards compatibility with Emacs < 23.2.
 Execute BODY, pretending it does not modify the buffer.
If BODY performs real modifications to the buffer's text, other
than cosmetic ones, undo data may become corrupted.

This macro will run BODY normally, but doesn't count its buffer
modifications as being buffer modifications.  This affects things
like buffer-modified-p, checking whether the file is locked by
someone else, running buffer modification hooks, and other things
of that nature.

Typically used around modifications of text-properties which do
not really affect the buffer's content."
      (declare (debug t) (indent 0))
      (let ((modified (make-symbol "modified")))
        `(let* ((,modified (buffer-modified-p))
                (buffer-undo-list t)
                (inhibit-read-only t)
                (inhibit-modification-hooks t)
                deactivate-mark
                ;; Avoid setting and removing file locks and checking
                ;; buffer's uptodate-ness w.r.t the underlying file.
                buffer-file-name
                buffer-file-truename)
           (unwind-protect
               (progn
                 ,@body)
             (unless ,modified
               (restore-buffer-modified-p nil))))))))

(defsubst closure-bricks-propertize-delimiter (loc depth)
  "Highlight a single delimiter at LOC according to DEPTH.

LOC is the location of the character to add text properties to.
DEPTH is the nested depth at LOC, which determines the face to use.

Sets text properties:
`font-lock-face' to the appropriate delimiter face.
`rear-nonsticky' to prevent color from bleeding into subsequent characters typed by the user."
  (with-silent-modifications
    (let ((delim-face (if (<= depth 0)
                          "closure-bricks-unmatched-face"
                        (closure-bricks-depth-face depth))))
      ;; (when (eq depth -1) (message "Unmatched delimiter at char %s." loc))
      (add-text-properties loc (1+  loc)
                           `(font-lock-face ,delim-face
                                            ;;rear-nonsticky t
                                            background "#a2b6da")))))


(defsubst closure-bricks-propertize-block (start-loc end-loc depth)
  (with-silent-modifications
    (let ((delim-face (if (<= depth 0)
                          "closure-bricks-unmatched-face"
                        (closure-bricks-depth-face depth))))
      ;; (when (eq depth -1) (message "Unmatched delimiter at char %s." loc))
      (add-text-properties start-loc (+ end-loc 1)
                           `(font-lock-face ,delim-face
                                            rear-nonsticky t)))))



(defsubst closure-bricks-unpropertize-delimiter (loc)
  "Remove text properties set by closure-bricks mode from char at LOC."
  (with-silent-modifications
    (remove-text-properties loc (- 1 loc)
                            '(font-lock-face nil rear-nonsticky nil))))


(defun closure-bricks-char-ineligible-p (loc)
  "Return t if char at LOC should be skipped, e.g. if inside a comment.

Returns t if char at loc meets one of the following conditions:
- Inside a string.
- Inside a comment.
- Is an escaped char, e.g. ?\)"
  (let ((parse-state (save-excursion
                       (beginning-of-defun)
                       ;; (point) is at beg-of-defun; loc is the char location
                       (parse-partial-sexp (point) loc))))
    (or
     (nth 3 parse-state)                ; inside string?
     (nth 4 parse-state)                ; inside comment?
     (and (eq (char-before loc) ?\\)  ; escaped char, e.g. ?\) - not counted
          (and (not (eq (char-before (1- loc)) ?\\)) ; special-case: ignore ?\\
               (eq (char-before (1- loc)) ?\?))))))
;; NOTE: standard char read syntax '?)' is not tested for because emacs manual
;; states punctuation such as delimiters should _always_ use escaped '?\)' form.


(defsubst closure-bricks-apply-color (delim depth loc)
  "Apply color for DEPTH to DELIM at LOC following user settings.

DELIM is a string specifying delimiter type.
DEPTH is the delimiter depth, or corresponding face # if colors are repeating.
LOC is location of character (delimiter) to be colorized."
  (and
   ;; Ensure user has enabled highlighting of this delimiter type.
   (symbol-value (intern-soft
                  (concat "closure-bricks-highlight-" delim "s-p")))
   (closure-bricks-propertize-delimiter loc
                                            depth)))

(defsubst closure-bricks-apply-color-block (delim depth start-loc loc)
  "Apply color for DEPTH to DELIM at LOC following user settings.

DELIM is a string specifying delimiter type.
DEPTH is the delimiter depth, or corresponding face # if colors are repeating.
LOC is location of character (delimiter) to be colorized."
  (and
   ;; Ensure user has enabled highlighting of this delimiter type.
   (symbol-value (intern-soft
                  (concat "closure-bricks-highlight-" delim "s-p")))
   (closure-bricks-propertize-block start-loc loc depth)))


;;; JIT-Lock functionality

;; Used to skip delimiter-by-delimiter `closure-bricks-propertize-region'.
(defvar closure-bricks-delim-regex "\\(\(\\|\)\\|\\[\\|\\]\\|\{\\|\}\\)"
  "Regex matching all opening and closing delimiters the mode highlights.")

;; main function called by jit-lock:
(defun closure-bricks-propertize-region (start end)
  "Highlight delimiters in region between START and END.

Used by jit-lock for dynamic highlighting."
  (save-excursion
    (goto-char start)
    ;; START can be anywhere in buffer; determine the nesting depth at START loc
    (let ((depth (closure-bricks-depth start)))
      (while (and (< (point) end)
                  (re-search-forward closure-bricks-delim-regex end t))
        (backward-char) ; re-search-forward places point after delim; go back.
        (unless (closure-bricks-char-ineligible-p (point))
          (let ((delim (char-after (point))))
            (cond ((eq ?\( delim)
                   (setq depth (1+ depth))
                   (setq paren-start (point)))
                  ((eq ?\) delim)
                   (closure-bricks-apply-color-block "paren" depth paren-start (point))
                   (setq depth (or (and (<= depth 0) 0) ; unmatched paren
                                   (1- depth)))))))
        ;; move past delimiter so re-search-forward doesn't pick it up again
        (forward-char)))))

(defun closure-bricks-unpropertize-region (start end)
  "Remove highlighting from delimiters between START and END."
  (save-excursion
    (goto-char start)
    (while (and (< (point) end)
                (re-search-forward closure-bricks-delim-regex end t))
      ;; re-search-forward places point 1 further than the delim matched:
      (closure-bricks-unpropertize-delimiter (1- (point))))))

;;; Minor mode:

;;;###autoload
(define-minor-mode closure-bricks-mode
  "Highlight nested parentheses, brackets, and braces according to their depth."
  nil "" nil ; No modeline lighter - it's already obvious when the mode is on.
  (if (not closure-bricks-mode)
      (progn
        (jit-lock-unregister 'closure-bricks-propertize-region)
        (closure-bricks-unpropertize-region (point-min) (point-max)))
    (jit-lock-register 'closure-bricks-propertize-region t)
    ;; Create necessary syntax tables inheriting from current major-mode.
    (set (make-local-variable 'closure-bricks-all-delimiters-syntax-table)
         (closure-bricks-make-syntax-table-all-delimiters (syntax-table)))))

;;;###autoload
(defun closure-bricks-mode-enable ()
  (closure-bricks-mode 1))

;;;###autoload
(define-globalized-minor-mode global-closure-bricks-mode
  closure-bricks-mode closure-bricks-mode-enable)

(provide 'closure-bricks)

;;; closure-bricks.el ends here.
