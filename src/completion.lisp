(in-package :lem-core)

(defvar *file-completion-ignore-case* t)

(defvar *completion-scoring-rules*
  '((:full-match-filename . 1000)
    (:full-match-candidate . 900)
    (:starts-with-filename . 800)
    (:starts-with-candidate . 700)
    (:component-matches . 1)))

(defun split-into-components (string &optional separator)
  "Split string into components, using an optional SEPARATOR or space as default."
  (if separator
      (str:split separator string)
      (str:words string)))

(defun component-matches-p (component candidate test)
  "Check if COMPONENT matches CANDIDATE using TEST."
  (or (funcall test component candidate)
      (every (lambda (char)
               (funcall test (string char) candidate))
             component)))

(defun all-components-match-p (components candidate test fuzzy)
  "Check if all COMPONENTS match CANDIDATE using TEST. If FUZZY is true, allow partial matches."
  (or (null components)
      (if fuzzy
          (every (lambda (component)
                   (component-matches-p component candidate test))
                 components)
          (let ((candidate-components (split-into-components candidate)))
            (and (<= (length components) (length candidate-components))
                 (every (lambda (comp1 comp2)
                          (funcall test comp1 comp2))
                        components candidate-components))))))

(defun completion-score (pattern candidate test scoring-rules)
  "Compute a score for CANDIDATE matching PATTERN using TEST and SCORING-RULES."
  (let* ((components (split-into-components pattern))
         (filename (str:substring (1+ (or (position #\/ candidate :from-end t) -1)) nil candidate))
         (score 0))
    (dolist (rule scoring-rules)
      (let ((rule-type (car rule))
            (rule-score (cdr rule)))
        (case rule-type
          ((:full-match-filename :full-match-candidate)
           (when (funcall test pattern filename)
             (incf score rule-score)))
          ((:starts-with-filename :starts-with-candidate) 
           (when (str:starts-with-p pattern filename :ignore-case t)
             (incf score rule-score)))
          (:component-matches
           (incf score (* rule-score
                          (count-if (lambda (comp) 
                                      (funcall test comp candidate))
                                    components)))))))
    score))

(defun completion (name elements &key separator (key #'identity) (scoring-rules *completion-scoring-rules*) (fuzzy t))
  (let* ((components (split-into-components name separator))
         (case-sensitive (some #'upper-case-p name))
         (test (lambda (a b) 
                    (search a b :test (if case-sensitive #'char= #'char-equal))))
         (matches
           (remove-if-not
            (lambda (elt)
              (let ((elt-str (funcall key elt)))
                (all-components-match-p components elt-str test fuzzy)))
            elements)))
    (sort matches
          (lambda (a b)
            (> (completion-score name (funcall key a) test scoring-rules)
               (completion-score name (funcall key b) test scoring-rules))))))

(defun completion-file (str directory &key directory-only (scoring-rules *completion-scoring-rules*) (fuzzy t))
  (setf str (expand-file-name str directory))
  (let* ((input-directory
           (let ((dir (directory-namestring str)))
             (or (ignore-errors (virtual-probe-file dir)) dir)))
         (input-pathname (merge-pathnames (enough-namestring str (directory-namestring str))
                                          input-directory))
         (files (mapcar #'namestring (list-directory input-directory :directory-only directory-only))))
    (let ((strings
            (loop
              :for pathname :in (directory-files input-pathname)
              :for namestr := (namestring pathname)
              :append
                 (completion (let ((str (enough-namestring namestr input-directory)))
                               (if (uiop:directory-pathname-p str)
                                   (string-right-trim "/" str)
                                   str))
                             files
                             :separator "-."
                             :key #'(lambda (path)
                                      (enough-namestring path input-directory))
                             :scoring-rules scoring-rules
                             :fuzzy fuzzy))))
      strings)))

(defun completion-strings (str strings &key key (scoring-rules *completion-scoring-rules*) (fuzzy t))
  (completion str strings :key key :scoring-rules scoring-rules :fuzzy fuzzy))

(defun completion-buffer (str &optional (buffer-list (buffer-list)) (scoring-rules *completion-scoring-rules*) (fuzzy t))
  (completion str buffer-list
              :key (lambda (buffer)
                     (or (buffer-name buffer)
                         (and (buffer-filename buffer)
                              (file-namestring (buffer-filename buffer)))))
              :scoring-rules scoring-rules
              :fuzzy fuzzy))