;;; -*- lexical-binding: t -*-

;;; shopping-lisp.el --- create shopping lists within Emacs by selecting recipes

;; Copyright (C) 2013 Simon West

;; Author:  Simon West
;; Version: 1.0
;; Keywords:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License Version 3 as
;; published by the Free Software Foundation.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.	If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; In short: you may use this code any way you like, as long as you
;; don't charge money for it, remove this notice, or hold anyone liable
;; for its results.

(require 'calc-ext)
(require 'widget)
(eval-when-compile (require 'wid-edit))

(defun shopping-to-calc (shopping-sexp)
  "Convert a shopping-lisp format quantity, e.g., (1.0 kg), to a
calc expression.

Example:
  (shopping-to-calc '(1.0 kg))
    --> (* (float 1 0) (var kg var-kg))
"
  (math-read-expr (format "%f %s" (float (cl-first shopping-sexp))
                          (symbol-name (cl-second shopping-sexp)))))

(defun shopping-math-simplify-units (quantity)
  "A wrapper around math-simplify-units such that
if (math-simplify-units quantity) evaluates to 0, we
return (float 0 0) for consistency."
  (let ((simplified (math-simplify-units quantity)))
    (if (numberp simplified)
        (if (= 0 simplified)
            '(float 0 0)
          (error "Should not get here"))
      simplified)))

(defun try-sum-unitful (q1 q2)
"Use shopping-math-simplify-units to try and sum two unitful quantities. If
  their dimensions are different, return nil.

Example:
  (try-sum-unitful (shopping-to-calc '(10 kg)) (shopping-to-calc '(500 g)))
    --> (* (float 105 -1) (var kg var-kg))

  (try-sum-unitful (shopping-to-calc '(10 kg)) (shopping-to-calc '(5 l)))
    --> nil
"
(let ((sum-of-quantities (shopping-math-simplify-units (list '+ q1 q2))))
  (if (eq (car sum-of-quantities) '+) nil
    sum-of-quantities)))

(defun unitful-geq-zero (q)
  "determine if a unitful quantity is >=0."
  (if (eq (cl-first q) '*)
      (>= (cl-second (cl-second q)) 0) 't))

(defun unitful-eq-zero (q)
  "determine if a unitful quantity is zero."
  (if (eq (cl-first q) '*)
      (= (cl-second (cl-second q)) 0) 't))

(defun try-min-unitful (q1 q2)
  "Find the minimum of two unitful quantities. Returns nil if the
units are different."
  (let ((difference (shopping-math-simplify-units (list '- q1 q2))))
    (if (eq (car difference) 'float) q1
      (if (eq (car difference) '-) nil
        (if (unitful-geq-zero difference) q2 q1)))))

(defun shopping-sum-quantities (quantity-list)
  " Sum up a list of calc-format unitful quantities.

Returns: a list whose elements are the quantity sums for each
unique dimension in the input list.

Example that sums up some masses and volumes:

  (shopping-sum-quantities
     (list (shopping-to-calc '(10 kg))
        (shopping-to-calc '(5 kg))
        (shopping-to-calc '(5 l)) (shopping-to-calc '(10 pt))
        (shopping-to-calc '(2 kg))))

    --> ((* (float 973176473002 -11) (var l var-l))
         (* (float 17 0) (var kg var-kg)))

The return type here is a list of length two, because there are
two unique dimensions in the input list (mass and volume).
"
  (if (= 1 (length quantity-list)) quantity-list
    (if (= 0 (length quantity-list)) '()
      (let* ((first-qty (car quantity-list))
             (sum-of-remainder (shopping-sum-quantities (cdr quantity-list)))
             (all-sums (mapcar (lambda (qty) (try-sum-unitful first-qty qty))
                               sum-of-remainder))
             (all-sums-dedup (delete nil (delete-dups all-sums))))
        (if (> (length all-sums-dedup) 1)
            (error "all-sums-dedup should be either () or ([SUM QTY])")
          (if (equal all-sums-dedup '()) ; all sums were nil,
                                        ; so first-qty has a
                                        ; different dimension
                                        ; to all other
                                        ; quantities
              (append (list first-qty) sum-of-remainder)
                                        ; TODO: modify
                                        ; sum-of-remainder so it
                                        ; includes the summed quantity
                                        ; and return it
            (progn
              (setf (nth (cl-position-if (lambda (x) (not (equal x nil))) all-sums)
                         sum-of-remainder)
                    (car all-sums-dedup))
              sum-of-remainder)))))))

(defun shopping-reload-data ()
  "Reload the recipe and ingredient files."
  (if (eq nil (boundp 'shopping-recipe-file))
      (error "Error: shopping-recipe-file is not defined"))
  (if (eq nil (boundp 'shopping-ingredient-file))
      (error "Error: shopping-ingredient-file is not defined"))
  (load shopping-recipe-file)
  (load shopping-ingredient-file))

(defun shopping-get-quantity-from-ingredient-expr (ingredient-expr)
  "Get the quantity for an ingredient appearing in
the :ingredients list in a recipe. If no quantity is specified
for the ingredient then nil is returned. E.g., for (\"Salt\"),
nil will be returned, but for (\"Squash\" 750 g), the return
value will be (750 g). Note that this will be the cdr of the
input, not a freshly-created list."
  (if (= 1 (length ingredient-expr)) nil (cdr ingredient-expr)))

(defun shopping-is-unitful-quantity (quantity)
  "Test whether a quantity appearing in the :ingredients list in
a recipe has units, where the quantity is something that would be
returned from shopping-get-quantity-from-ingredient-expr.
E.g., (750 g) would return true, but (3) would return nil, and
nil would return nil (recall that nil is a valid quantity -- it
indicates that no quantity was specified)."
  (and (= 2 (length quantity))
       (numberp (cl-first quantity))
       (symbolp (cl-second quantity))))

(defun shopping-is-unitless-quantity (quantity)
  "Test whether a quantity appearing in the :ingredients list in
a recipe is unitless but not nil. E.g., (750 g) would return
false, (3) would return true, nil would return false."
  (and (= 1 (length quantity))
       (numberp (cl-first quantity))))

(defun shopping-is-unspecified-quantity (quantity)
  "Test whether a quantity appearing in the :ingredients list in
a recipe is nil, i.e., unspecified."
  (eq nil quantity))

(defun shopping-add-composite-quantities (quantity1 quantity2)
  "Composite quantities are lists of length three. The first
element is nil or t and indicates that an unspecified amount was
required in some recipe. The second element is a number
representing a unitless quantity, and the third is a list of
unitful calc quantities, each with unique dimensions.

Example 1:
 (setq q1 (shopping-quantity-to-composite '()))
 (setq q2 (shopping-quantity-to-composite '(1)))
 (shopping-add-composite-quantities q1 q2)
  --> (t 1 nil)

Example 2:

 (setq q1 (shopping-quantity-to-composite '(50 g)))
 (setq q2 (shopping-quantity-to-composite '(10 ml)))
 (shopping-add-composite-quantities q1 q2)
   --> (nil 0 ((* (float 5 1) (var g var-g)) ((* (float 1 1) (var ml var-ml)))))
"
  (list (or (cl-first quantity1) (cl-first quantity2))
        (+ (cl-second quantity1) (cl-second quantity2))
        (shopping-sum-quantities (append (cl-third quantity1) (cl-third quantity2)))))


(defun shopping-diff-quantities (q1 q2)
  "q1, q2 are lists of unitful quantities, each element in list
  having different units. This removes q2 from q1, in the sense
  for each unitful quantity in q2 there must be at least that
  amount of that unit in q1, and the function subtracts it out.

  Example:

  (shopping-diff-quantities
     (list (shopping-to-calc '(10 kg))) (list (shopping-to-calc '(1 g))))

  --> ((* (float 9999 -3) (var kg var-kg)))

  (shopping-diff-quantities
     (list (shopping-to-calc '(10 kg)) (shopping-to-calc '(10 l)))
     (list (shopping-to-calc '( 1  g)) (shopping-to-calc '(1 ml)))
  )
"
  (let ((q2-length (length q2)))
    (if (= 0 q2-length) (copy-tree q1)
      (if (= 1 q2-length)
          (let* ((qty-to-remove (cl-first q2))
                 (new-list '()))
            (dolist (qty q1 new-list)
              (let* ((difference (shopping-math-simplify-units (list '- qty qty-to-remove))))
                (if (eq (car difference) '-)
                    (setq new-list (append (list qty) new-list))
                  (setq new-list (append (list difference) new-list))))))
        (shopping-diff-quantities (shopping-diff-quantities q1 (cdr q2)) (list (car q2)))))))


(defun logical-to-int (a) (if a 1 0))
(defun int-to-logical (a) (if (= 0 a) nil t))

(defun shopping-remove-composite-quantity (q1 q2)
  "Remove composite quantity q2 from quantity q1. If quantity q2
is not contained in q1, raise error."
  (progn
    (if (and (not (cl-first q1)) (cl-first q2))
        (error "removal of q2 from q1 failed because q2 has an
indefinite amount but q1 does not"))
    (if (< (cl-second q1) (cl-second q2))
        (error "removal of q2 from q1 failed because there are more units in q2"))
    (list (int-to-logical (logxor (logical-to-int (cl-first q1)) (logical-to-int (cl-first q2))))
          (- (cl-second q1) (cl-second q2))
          (shopping-diff-quantities (cl-third q1) (cl-third q2)))))

(defun composite-quantity-is-zero (q)
     (and (not (cl-first q)) (= 0 (cl-second q))
          (if (eq (cl-third q) nil) t
            (cl-every 'unitful-eq-zero (cl-third q)))))

(defun shopping-subtract-lists (l1 l2)
  "Subtract shopping list 2 from shopping list 1. If list 2 is
  not entirely contained in list 1, raise error."
  (if (= (length l2) 1)
      (if (string= (caar l1) (caar l2))
          (let ((difference (shopping-remove-composite-quantity (cdar l1) (cdar l2))))
            ; if difference is a zero composite quantity (test with
            ; composite-quantity-is-zero), then should just return (cdr l1)
            (if (composite-quantity-is-zero difference) (cdr l1)
              (append (list (append (list (caar l1)) difference))
                      (cdr l1)))
          )
        (append (list (car l1)) (shopping-subtract-lists (cdr l1) l2)))
    (shopping-subtract-lists (shopping-subtract-lists l1 (list (car l2))) (cdr l2))))

(defun shopping-quantity-to-composite (quantity)
  "Convert a quantity appearing in a shopping list to a 'composite quantity'.

See doc for shopping-add-composite-quantities for the meaning of
a 'composite' quantity.

Example:

  (shopping-quantity-to-composite '())
    --> (t 0 nil)

  (shopping-quantity-to-composite '(50))
    --> (nil 50 nil)

  (shopping-quantity-to-composite '(10 g))
    --> (nil 0 ((* (float 1 1) (var g var-g))))

This function will not affect the input list, but note that for
unitful quantities (third example above), the input list will
become part of the output list (i.e., the input list is not
copied).
"
  (cond ((shopping-is-unspecified-quantity quantity) '(t 0 ()))
        ((shopping-is-unitless-quantity quantity) `(nil ,(cl-first quantity) ()))
        ((shopping-is-unitful-quantity quantity) `(nil 0 ,(list (shopping-to-calc quantity))))))

(defun shopping-add-ingredient-to-shopping-list (shopping-list ingredient-info)
  " Add an ingredient to the given shopping list. Note that
shopping-list is altered by this function.

shopping list is an alist from ingredient
name to quantity information.

ingredient-info is a list of the form (name quantity unit),
or (name quantity), or (name), i.e., an element from
the :ingredients list in a recipe,

Note that it doesn't actually sum up the unitful
quantities. Rather, they're just collected into a list. The
summing up comes on string conversion (perhaps this should be
changed?).

Example:
  (setq my-shopping '())
  (shopping-add-ingredient-to-shopping-list my-shopping '(\"Unsalted butter\" 50 g))
  (shopping-add-ingredient-to-shopping-list my-shopping '(\"Unsalted butter\" 100 g))
  (shopping-add-ingredient-to-shopping-list my-shopping '(\"Cashew nuts\"))

    --> ((\"Cashew nuts\" t 0 nil) (\"Unsalted butter\" nil 0 ((* (float 15 1) (var g var-g)))))

Note: there are no '.' symbols in the elements of the resulting
alist. That's because the second value in each element is a
list. E.g., (1 2 3 4) is just another way of writing (1 . (2 3 4)),
which can be verified by evaluating '(1 . (2 3 4))
"
  (let* ((sl-quantity
          (shopping-quantity-to-composite
           (shopping-get-quantity-from-ingredient-expr ingredient-info)))
         (ingredient (cl-first ingredient-info))
         (item (assoc ingredient shopping-list)))
    (if (eq item nil)
        (push `(,ingredient . ,sl-quantity) shopping-list)
      (progn
        (setf (cdr item) (shopping-add-composite-quantities (cdr item) sl-quantity))
        shopping-list))))

(defun shopping-add-substitutable-ingredients-to-shopping-list
  (shopping-list substitutable-ingredient-list)
  "Given a shopping list and a list of n substitutable
ingredients, returns n new shopping lists, one for each
variation. Does not modify input lists. All the returned lists
use copies of the original shopping-list, produced by copy-tree.

Example:

  (setq my-shopping '((\"Cashew nuts\" t 0 nil)))
  (shopping-add-substitutable-ingredients-to-shopping-list my-shopping
      '((\"Black pepper\") (\"Red pepper\")))

   --> (((\"Red pepper\" t 0 nil) (\"Cashew nuts\" t 0 nil))
        ((\"Black pepper\" t 0 nil) (\"Cashew nuts\" t 0 nil)))

"
  (cl-loop with result = '()
        for i from 0
        while (< i (length substitutable-ingredient-list))
        do
        (let ((branched-list (copy-tree shopping-list))
              (ingredient-info (nth i substitutable-ingredient-list)))
          (progn
            (setq branched-list (shopping-add-ingredient-to-shopping-list branched-list ingredient-info))
            (add-to-list 'result branched-list)))
        finally
        return result))

(defun shopping-add-recipe-to-shopping-lists (shopping-lists recipe)
  " shopping-lists is a list of shopping lists. We need this
because whenever there's an 'or' ingredient, a shopping list
branches into two or more lists. This function does not modify
the input lists, it creates deep copies and returns those
instead."
  (let ((ingredients (cl-getf recipe ':ingredients)))
    (cl-loop with results = (copy-tree shopping-lists)
          for i from 0
          while (< i (length ingredients))
          do
          (let ((ingredient-info (nth i ingredients)))
            (if (stringp (cl-first ingredient-info)) ; a single
                                                  ; ingredient rather
                                                  ; than a list of
                                                  ; substitutable
                                                  ; ingredients
                (setq results
                      (mapcar (lambda (sl)
                                (shopping-add-ingredient-to-shopping-list
                                 sl ingredient-info))
                        results))
              (let ((branched-results '())) ; the else clause for
                                            ; above if, so
                                            ; ingredient-info is a
                                            ; list of substitutable
                                            ; ingredients
                (cl-loop for j from 0
                      while (< j (length results))
                      do
                      (setq branched-results (append branched-results
                       (shopping-add-substitutable-ingredients-to-shopping-list
                        (nth j results)
                        ingredient-info)))
                      finally
                      (setq results branched-results)))))
          finally
          return results)))


(defun shopping-shopping-list-entry-to-string (entry)
" Convert an entry in the internal representation of a shopping
list to a readable string.

The internal representation of a shopping list looks like this:

  (
   (\"Cashew nuts\" t 0 nil)
   (\"Unsalted butter\" nil 0 ((* (float 15 1) (var g var-g))))
  )

Example:

  (shopping-shopping-list-entry-to-string
      '(\"Unsalted butter\" nil 0 ((* (float 15 1) (var g var-g)))))

    --> \"150. g Unsalted butter\"
"
  (let* ((ingredient (cl-first entry))
         (output-strings '()))
    (progn
      (if (cl-second entry)
          (add-to-list 'output-strings ingredient))
      (setq n-items (cl-third entry))
      (if (> n-items 0)
          (if (integerp n-items)
          (add-to-list 'output-strings
                       (format "%i %s" n-items ingredient))
        (add-to-list 'output-strings
                       (format "%1.1f %s" n-items ingredient))))
      (if (not (eq nil (cl-fourth entry)))
          (setq output-strings
                (append output-strings
                        (mapcar (lambda (q) (format "%s %s"
                                                    (math-format-value q)
                                                    ingredient))
                         (cl-fourth entry)))))
      (mapconcat 'identity output-strings ", "))))

(defun shopping-get-ingredient-category (ingredient)
  (cl-getf (lax-plist-get shopping-ingredients ingredient) :class))

(defun shopping-shopping-list-by-category (shopping-list)
" Takes a shopping list and returns an association list whose
keys are categories and whose values are lists containing
ingredients in the given category.
"
  (let ((shopping-alist '()))
    (dolist (ingredient-info shopping-list)
      (let* ((ingredient (cl-first ingredient-info))
             (category (shopping-get-ingredient-category ingredient)))
        (if (eq nil category)
            (error "Error: no ingredient information for %s" ingredient))
        (if (eq nil (cdr (assoc category shopping-alist)))
            (setq shopping-alist (cons `(,category . (,ingredient-info))
                                       shopping-alist))
          (nconc (cdr (assoc category shopping-alist)) `(,ingredient-info)))))
    shopping-alist))

(defun shopping-pprint-shopping-list-by-category (shopping-list)
  (let ((by-category (shopping-shopping-list-by-category shopping-list))
        (output-string ""))
    (dolist (category by-category)
      (setq output-string (format "%s\n- %s" output-string (car category)))
      (dolist (ingredient (cdr category))
        (setq output-string
              (format "%s\n    - ☐ %s"
                      output-string
                      (shopping-shopping-list-entry-to-string ingredient)))))
    output-string))

(defun shopping-get-digits-from-user ()
  (let ((digits '())
        (new-digit nil))
    (catch 'break
      (while t
        (setq new-digit (read-from-minibuffer
                         "Recipe number (press enter with no input to finish)? "))
        (if (string= new-digit "")
            (throw 'break nil)
          (setq digits (append digits (list (string-to-number new-digit)))))))
    digits))

(defun shopping-sort-shopping-list (shopping-list)
  "Destructively sort a shopping list by ingredient name."
  (sort shopping-list
        (lambda (ing1 ing2)
          (string< (cl-first ing1) (cl-first ing2)))))

(defun quantity-intersection-unspecified (q1 q2)
     "Intersection of two unspecified quantities, each of which
     should be either nil or 't"
     (if (and (eq q1 't) (eq q2 't)) 't))

(defun quantity-intersection-unitless (q1 q2)
     "Intersection of two unitless quantities, each of which
     should be numbers. This is just the minimum of the two."
     (min q1 q2))

(defun quantity-intersection-unitful (l1 l2)
     "Intersection of two unitful quantity lists."
     (let ((intersection '()))
       (dolist (e1 l1 intersection)
         (dolist (e2 l2)
           (let ((element-intersection (try-min-unitful e1 e2)))
             (if element-intersection
                 (setq intersection
                       (append intersection (list element-intersection)))))))))

(defun ingredient-intersection (ing1 ing2)
  "Intersection of two ingredient quantities. Returns nil if the
ingredients are different. If they are the same, then calculates
a sensible intersection of them. Also returns nil if there's no
sensible intersection of quantities, even if the ingredient is
the same.

The 2nd, 3rd, 4th elements make up a 'composite quantity' as
explained in doc for shopping-add-composite-quantities.

Example 1:

  (ingredient-intersection
   '(\"Unsalted butter\" nil 1 ((* (float 15 1) (var g var-g))))
   '(\"Unsalted butter\" nil 2 ((* (float 1 1) (var kg var-kg)))))

  ---> (nil 1 (* (float 15 1) (var g var-g)))

Example 2 (in which there's no sensible intersection):

 (ingredient-intersection
   '(\"Unsalted butter\" nil 1 ())
   '(\"Unsalted butter\" nil 0 ((* (float 15 1) (var g var-g)))))

  ---> nil
"
  (if (string= (car ing1) (car ing2))
      (let ((intersection
             (list (quantity-intersection-unspecified (cl-second ing1) (cl-second ing2))
                   (quantity-intersection-unitless    (cl-third  ing1) (cl-third  ing2))
                   (quantity-intersection-unitful     (cl-fourth ing1) (cl-fourth ing2)))))
        (if (and (not (cl-first intersection)) (= 0 (cl-second intersection)) (not (cl-third intersection)))
            nil
            (append (list (car ing1)) intersection)))))

(defun example-list ()
  "Return an example shopping list for testing."
     (let (l '())
       (shopping-add-ingredient-to-shopping-list 'l '("Tomatoes" 50 g))
       (shopping-add-ingredient-to-shopping-list 'l '("Tomatoes" 1))
       l))

(defun example-list-2 ()
  "Return an example shopping list for testing."
     (let (l '())
       (shopping-add-ingredient-to-shopping-list 'l '("Tomatoes"))
       (shopping-add-ingredient-to-shopping-list 'l '("Tomatoes" 1))
       (shopping-add-ingredient-to-shopping-list 'l '("Tomatoes" 100 g))
       l))

(defun example-list-3 ()
  "Return an example shopping list for testing."
     (let (l '())
       (shopping-add-ingredient-to-shopping-list 'l '("Tomatoes" 50 g))
       l))

(defun shopping-list-intersection (list-of-lists)
  "Return a new shopping list with the intersection of the given
shopping lists. A shopping list is a list constructed with calls
to shopping-add-ingredient-to-shopping-list."
  (if (= (length list-of-lists) 1)
      (copy-tree (cl-first list-of-lists))
    (if (> (length list-of-lists) 2)
        (shopping-list-intersection
         (append (list (cl-first list-of-lists))
                 (list (shopping-list-intersection (cdr list-of-lists)))))
      ;; if we're here, then list-of-lists has length 2
      (let ((intersection '()))
        (dolist (e1 (cl-first list-of-lists) intersection)
          (dolist (e2 (cl-second list-of-lists))
            (let ((ii (ingredient-intersection e1 e2)))
              (if ii
                  (setq intersection
                        (append intersection (list ii)))))))))))

(defun shopping-lisp-selection ()
  "Create the widgets from the Widget manual."
  (switch-to-buffer "*Recipe selection menu*")
  (kill-all-local-variables)
  (make-local-variable 'widget-example-repeat)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (setq shopping-lisp-recipe-counts
        (make-vector (length shopping-recipes) 0))
  (let ((shopping-lisp-count-widgets
         (make-vector (length shopping-recipes) '())))
    (dotimes (i (length shopping-recipes))
      (widget-create
       'push-button
       :notify (lambda (&rest ignore)
                 (let* ((count (aref shopping-lisp-recipe-counts i))
                        (newcount (+ count 1)))
                   (aset shopping-lisp-recipe-counts i newcount)
                   (widget-value-set
                    (aref shopping-lisp-count-widgets i) newcount)))
       "+")
      (widget-insert " ")
      (widget-create
       'push-button
       :notify (lambda (&rest ignore)
                 (let* ((count (aref shopping-lisp-recipe-counts i))
                        (newcount (- count 1)))
                   (message (format "count: %i" count))
                   (if (> count 0)
                       (progn (aset shopping-lisp-recipe-counts i newcount)
                              (widget-value-set
                               (aref shopping-lisp-count-widgets i) newcount)))))
       "-")
      (aset shopping-lisp-count-widgets i (widget-create 'const :format " %v " 0))
      (widget-insert (format "%s\n" (cl-getf (nth i shopping-recipes) :title)))
      )
    (widget-insert "\n")
    (widget-create 'push-button :notify (lambda (&rest ignore)
                                          (progn
                                            (kill-buffer "*Recipe selection menu*")
                                            (shopping-make-list)))
                   "Create shopping list")
    (use-local-map widget-keymap) ; So that, e.g., the enter key presses buttons
    (widget-setup))
  )

(defun shopping-make-list ()
  "Using the global variable shopping-lisp-recipe-counts, which
  contains a count for each recipe, create a shopping list to
  *Shopping* buffer."
  (let ((shopping-lists '(())))
    (dotimes (i (length shopping-recipes))
      (dotimes (j (aref shopping-lisp-recipe-counts i))
        (setq shopping-lists (shopping-add-recipe-to-shopping-lists
                              shopping-lists
                              (nth i shopping-recipes)))
        ))
    (shopping-display-lists shopping-lists)))

(defun shopping-display-lists (shopping-lists)
  "Create a new buffer and print out recipes, common ingredients
  and optional ingredients from the given shopping-lists
  variable."
  (let* ((common-ingredients-list (shopping-list-intersection shopping-lists))
         (shopping-list-buffer (get-buffer-create "*Shopping*")))
    (set-buffer shopping-list-buffer)
    (erase-buffer)
    (princ "# Recipes\n\n" shopping-list-buffer)
    (dotimes (i (length shopping-recipes))
      (if (> (aref shopping-lisp-recipe-counts i) 0)
          (princ (format "- %s\n" (cl-getf (nth i shopping-recipes) :title))
                 shopping-list-buffer))
      )
    (princ (format "\n# Ingredients\n\n## You must get these\n%s\n"
                   (shopping-pprint-shopping-list-by-category
                    common-ingredients-list))
           shopping-list-buffer)
    (if (> (length shopping-lists) 1)
        (progn
        (princ (format "\n## Choose only one of these\n")
               shopping-list-buffer)
        (dotimes (i (length shopping-lists))
          (princ (format "\n### Option %i\n%s\n"
                         i
                         (shopping-pprint-shopping-list-by-category
                          (shopping-subtract-lists (nth i shopping-lists)
                                                   common-ingredients-list)))
                 shopping-list-buffer))))
    (display-buffer "*Shopping*")
    )
  )

(defun shopping-prepare-list ()
  "Prompt user for recipe choices, calculate shopping list(s),
and print it to *Shopping* buffer in Pandoc Markdown format."
  (interactive)
  (shopping-reload-data)
  (shopping-lisp-selection)
)

(setq shopping-index-card-template-file
      (concat (file-name-directory load-file-name)
              "index_cards.latex.pandoc.template"))

(defun shopping-list-to-pdf ()
  "Save current buffer to pdf as A6 index cards."
  (interactive)
  (let* ((output-file (read-from-minibuffer "Output pdf file? "))
         (pandoc-command
          (format "pandoc --latex-engine xelatex --template %s -o %s"
                  shopping-index-card-template-file output-file)))
    (shell-command-on-region (point-min) (point-max) pandoc-command)))
