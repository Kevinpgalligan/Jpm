;; TODO:
;;  1. Revise grammar based on gathered notes.
;;  2. Try parsing some J scripts and dumping 'em back out.
;;  3. Manually implement closures in J.
;;  4. Automatic closure generation.

(defpackage :jpm
  (:use :cl :esrap :alexandria))

(in-package jpm)

(defclass jobj ()
  (()
   (content :initarg :jthing :accessor content)))

(defrule jscript
    (? (and sentence (* (and nl sentence))))
  (:lambda (list)
    (cons (first list)
          (second list))))

(defrule sentence
    (? (or comment
           (and (or whitespace
                    subexpr
                    string
                    array
                    primitive
                    identifier)
                sentence)))
  (:function alexandria:flatten))

(defrule commentless-sentence
    (? (and (or whitespace
                (and "(" commentless-sentence ")")
                string
                array
                primitive
                identifier)
            commentless-sentence))
  (:function alexandria:flatten))

(defrule comment (and "NB." (* (not nl)))
  (:text t))

(defrule whitespace (or #\Space #\Tab)
  (:text t))
(defrule nl (or #\Return #\Linefeed)
  (:text t))

#|
"Names (used for pronouns and other surrogates, and assigned referents by the copula, as in prices=: 4.5 12) begin with a letter and may continue with letters, underlines, and digits."
|#
(defrule identifier (and letter (+ (or #\_ letter digit)))
  (:text t))
(defrule letter (character-ranges (#\a #\z) (#\A #\Z)))

#|
"Numbers are denoted by digits, the underbar (for negative signs and for infinity and minus infinity -- when used alone or in pairs), the period (used for decimal points and necessarily preceded by one or more digits), the letter e (as in 2.4e3 to signify 2400 in exponential form), and the letter j to separate the real and imaginary parts of a complex number, as in 3e4j_0.56. Also see the discussion of Constants."
|#
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

;; "A list of ASCII characters is denoted by the list enclosed in single quotes, a pair of adjacent single quotes signifying the quote itself"
(defrule string (and "'"
                     (+ (or "''" (not "'")))
                     "'")
  (:text t))

;; "A numeric list or vector is denoted by a list of numbers separated by spaces."
(defrule array
    (and number
         (* (and (+ " ") number)))
  (:text t))

#|
"A primitive or primary may be denoted by a single graphic (such as + for plus) or by a graphic modified by one or more following inflections (a period or colon), as in +. and +: for or and nor. A primary may also be an inflected name, as in e. and o. for membership and pi times. A primary cannot be assigned a referent."

Also, see: https://code.jsoftware.com/wiki/NuVoc

The PRIMITIVE rule must come before NUMBER and IDENTIFIER because some of
their rules can be prefixes of these rules. Likewise, assignment operators
should come before PRIMITIVE because of "=".
|#
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

;; TODO:
;; Start putting it all together. Starting out with a single line ("sentence").
;;   - brackets
;;   - assignment
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
;;    - Various ways of defining functions
;;     {{ }}
;;     3 : 0
;;     )
;;    - Other weird 'm : n' constructs

(defrule assignment-op (or "=." "=:")
  (:text t))
