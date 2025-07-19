;; TODO:
;;  1. Revise grammar based on gathered notes.
;;  2. Try parsing some J scripts and dumping 'em back out.
;;  3. Manually implement closures in J.
;;  4. Automatic closure generation.

;; Grammar TODO
;;    - Various ways of defining functions
;;     {{ }}
;;     3 : 0
;;     )
;;   - control words (see "T-block" in notes)
;;     if. else. elseif. assert. 
;;     break. continue. for.
;;     for_ijk. goto_lbl.
;;     label_lbl.
;;     return. select.
;;     case.
;;     fcase.
;;     throw. catcht. try.
;;     catch.
;;     catchd.
;;     catcht. while.
;;     whilst.
;;   - Other weird 'm : n' constructs

(defpackage :jpm
  (:use :cl :esrap :alexandria))

(in-package jpm)


;;;; PARSE TREE DATA STRUCTURES.
;;;; For when a list isn't enough.
(defclass parse-node ()
  ((children :initarg :children :reader children)))

(defclass assignment-node (parse-node)
  ((id :initarg :id :accessor id)
   (op :initarg :op :accessor op)
   (val :initarg :val :accessor val)))

(defun make-assignment-node (list)
  (destructuring-bind (id ws op val) list
    (declare (ignorable ws))
    (make-instance 'assignment-node :children list :id id :op op :val val)))


;;;; TRAVERSAL INTERFACE.
(defgeneric visit (visitor tree))

;; By default, do nothin'.
(defmethod visit (visitor tree)
  (declare (ignore visitor tree)))

;; But we do want to descend into the tree.
(defmethod visit (visitor (tree list))
  (dolist (child tree)
    (visit visitor child)))
(defmethod visit (visitor (node parse-node))
  (visit visitor (children node)))


;;;; OUTPUT.
(defclass output-visitor ()
  ((ostream :initarg :ostream :reader ostream)))

(defmethod visit ((visitor output-visitor) (s string))
  (format (ostream visitor) "~a" s))

(defun serialise-j (ostream lines)
  (let ((visitor (make-instance 'output-visitor :ostream ostream)))
    (dolist (line lines)
      (visit visitor line)
      (format ostream "~%"))))


;;;; GRAMMAR RULES.
(defrule jscript
    (? sentences))

(defrule sentences
    (and sentence (* (and nl sentence)))
  (:lambda (list)
    (cons (first list)
          ;; Remove the newlines.
          (loop for line in (second list)
                collect (second line)))))

;; "The sentence is the executable element in a J script. Basically, it is
;;  one line of code."
;; "Each line is a J sentence, which may include, or be, a comment, or be
;; blank or empty."
;;
;; (From: https://code.jsoftware.com/wiki/Vocabulary/Words)
(defrule sentence
    (?
     (or comment
         commentless-sentence))
  (:function alexandria:flatten))

(defrule commentless-sentence
    (and (or whitespace
             (and "(" commentless-sentence ")")
             string
             ;; The PRIMITIVE rule must come before NUMBER and
             ;; IDENTIFIER because they can be prefixes of primitives.
             ;; Likewise, assignment operators should come before PRIMITIVE
             ;; because of "=".
             assignment
             primitive
             array
             identifier)
         (? sentence))
  (:function alexandria:flatten))

(defrule comment (and "NB." (* (not nl)))
  (:text t))

(defrule whitespace (+ (or #\Space #\Tab))
  (:text t))
(defrule nl (or #\Return #\Linefeed #\Newline)
  (:text t))

;; "Names (used for pronouns and other surrogates, and assigned referents by
;; the copula, as in prices=: 4.5 12) begin with a letter and may continue
;; with letters, underlines, and digits."
;;
;; (From: https://wiki.jsoftware.com/wiki/Books/MathForTheLayman/Language_and_Grammar)
(defrule identifier (and letter (* (or #\_ letter digit)))
  (:text t))
(defrule letter (character-ranges (#\a #\z) (#\A #\Z)))

;; "Numbers are denoted by digits, the underbar (for negative signs and for
;; infinity and minus infinity -- when used alone or in pairs), the period
;; (used for decimal points and necessarily preceded by one or more digits),
;; the letter e (as in 2.4e3 to signify 2400 in exponential form), and the
;; letter j to separate the real and imaginary parts of a complex number, as
;; in 3e4j_0.56. Also see the discussion of Constants."
(defrule number
    (or (and real-number (? (and "j" real-number)))
        "__"
        "_."
        "_")
  (:text t))
(defrule real-number
    (and integer
         (? (and "." (+ digit)))
         (? (and "e" integer)))
  (:text t))
(defrule integer (and (? "_") (+ digit))
  (:text t))
(defrule digit (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

;; "A list of ASCII characters is denoted by the list enclosed in single
;; quotes, a pair of adjacent single quotes signifying the quote itself"
(defrule string (and "'"
                     (+ (or "''" (not "'")))
                     "'")
  (:text t))

;; "A numeric list or vector is denoted by a list of numbers separated by spaces."
(defrule array
    (and number
         (* (and (+ " ") number)))
  (:text t))

;; "A primitive or primary may be denoted by a single graphic (such as +
;; for plus) or by a graphic modified by one or more following inflections (a
;; period or colon), as in +. and +: for or and nor. A primary may also
;; be an inflected name, as in e. and o. for membership and pi times. A
;; primary cannot be assigned a referent."
;;
;; Also, see: https://code.jsoftware.com/wiki/NuVoc
(defrule primitive
    (or "="
        ;; Need to be careful that primitives that are prefixes of other
        ;; primitives come **after**.
        "<:" ">:" "<." ">." "<" ">"
        "__:" "_:"
        "+." "+:" "+"
        "*." "*:" "*"
        "-." "-:" "-"
        "%." "%:" "%"
        "^." "^:" "^" 
        "$." "$:" "$"
        "~." "~:" "~"
        "|." "|:" "|"
        "."
        ":." "::" ":"
        ",." ",:" ","
        ";." ";:" ";"
        "#." "#;" "#"
        "!." "!:" "!"
        "/.." "/." "/:" "/"
        "\\." "\\:" "\\"
        "[." "[:" "["
        "]." "]:" "]"
        "}." "}:" "}"
        "{::" "{:" "{." "{"
        "\"." "\":" "\""
        "`:" "`"
        "@." "@:" "@"
        "&.:" "&." "&:" "&"
        "?." "?"
        "A." "b."
        "c." "C.!." "C." "e."
        "E." "f." "F.." "F.:"
        "F:." "F::" "F:" "F."
        "H." "i." "i:"
        "I." "j." "L."
        "L:" "m." "M."
        "o." "p.." "p."
        "p:" "q:"
        "r." "s:" "S:"
        "t." "T." "u:"
        "x:" "Z:"
        (and (? "_") digit ":")
        "u." "v." "a:" "a.")
  (:text t))

(defrule assignment
    (and identifier (* whitespace) assignment-op commentless-sentence)
  (:function make-assignment-node))
(defrule assignment-op (or "=." "=:")
  (:text t))

;; Different sentences are allowed inside definitions.
;; TODO.
(defrule def-sentences)

(defrule if.
    (and "if." def-sentences "do." def-sentences
         (* (and "elseif." def-sentences "do." def-sentences))
         (? (and "else." def-sentences))
         "end."))
