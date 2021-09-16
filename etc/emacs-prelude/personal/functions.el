;; add separators to numbers, e.g. 12345 => 12,345
;; TODO: bind to key a-la https://github.com/knu/operate-on-number.el
(defun add-number-grouping (number &optional separator)
  "Add commas to NUMBER and return it as a string.
    Optional SEPARATOR is the string to use to separate groups.
    It defaults to a comma."
  (let ((num (number-to-string number))
	    (op (or separator ",")))
	(while (string-match "\\(.*[0-9]\\)\\([0-9][0-9][0-9].*\\)" num)
	  (setq num (concat 
                 (match-string 1 num) op
                 (match-string 2 num))))
	num))
