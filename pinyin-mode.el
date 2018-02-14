;;; pinyin-mode.el --- input mode for pinyin

;; Version: 0.0.1
;; last updated : 2018-02-12
(require 'thingatpt)
(setq pinyin-chars-list '(["a" "ā" "á" "ǎ" "à" "â"]
                          ["e" "ē" "é" "ě" "è" "ê"]
                          ["o" "ō" "ó" "ǒ" "ò"]
                          ["i" "ī" "í" "ǐ" "ì" "î"]
                          ["u" "ū" "ú" "ǔ" "ù"]
                          ["ü" "ǖ" "ǘ" "ǚ" "ǜ"]

))

(defun pinyin-regex ()
  (concat "[" (mapconcat #'(lambda (chars)
                             (mapconcat 'identity chars "")) pinyin-chars-list "") "]"))

(defun pinyin-find-chars (char list)
  (when list
    (let* ((chars (car list)))
      (if (cl-position char chars :test #'equal) chars
        (pinyin-find-chars char (cdr list))))))

(defun pinyin-make-tone (type)
  (save-excursion
    (save-restriction
      (narrow-to-region (car (bounds-of-thing-at-point 'word)) (point))
      (and (re-search-backward (pinyin-regex))
           (replace-match (aref (pinyin-find-chars (match-string 0) pinyin-chars-list) type))))))


(defun pinyin-make-tone-0 () (interactive) (pinyin-make-tone 0))
(defun pinyin-make-tone-1 () (interactive) (pinyin-make-tone 1))
(defun pinyin-make-tone-2 () (interactive) (pinyin-make-tone 2))
(defun pinyin-make-tone-3 () (interactive) (pinyin-make-tone 3))
(defun pinyin-make-tone-4 () (interactive) (pinyin-make-tone 4))
(defun pinyin-make-tone-5 () (interactive) (pinyin-make-tone 5))


(define-minor-mode pinyin-mode
  "Input pinyin"
  :lighter " PinYin"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "0") 'pinyin-make-tone-0)
            (define-key map (kbd "1") 'pinyin-make-tone-1)
            (define-key map (kbd "2") 'pinyin-make-tone-2)
            (define-key map (kbd "3") 'pinyin-make-tone-3)
            (define-key map (kbd "4") 'pinyin-make-tone-4)
            (define-key map (kbd "4") 'pinyin-make-tone-5)
            map))

;;; pinyin-mode.el ends here
