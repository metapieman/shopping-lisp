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

;;; Commentary:

;; shopping-lisp is a package designed to make grocery shopping
;; easy. To install it, put shopping-list.el somewhere Emacs can see
;; it (i.e., in one of the directories specified in the load-path
;; variable), and put the following in your .emacs file:
;;
;; (load "shopping-lisp.el")
;;
;; In addition, you must tell shopping-lisp where to find the recipe
;; file and the ingredient information file, by setting the variables
;; shopping-recipe-file and shopping-ingredient-file. For example, put
;; the following in your .emacs:
;;
;; (setq shopping-recipe-file "/home/johnsmith/recipes.el")
;; (setq shopping-ingredient-file "/home/johnsmith/ingredient-information.el")
;;
;; Examples of both files are provided with the package (recipes.el,
;; ingredient-information.el).
;;
;; Once installed, prepare your shopping list by doing:
;;
;; M-x shopping-prepare-list

(require 'calc-ext)

(defun shopping-reload-data ()
  "Reload the recipe and ingredient files."
  (if (eq nil (boundp 'shopping-recipe-file))
      (error "Error: shopping-recipe-file is not defined"))
  (if (eq nil (boundp 'shopping-ingredient-file))
      (error "Error: shopping-ingredient-file is not defined"))
  (load shopping-recipe-file)
  (load shopping-ingredient-file))

(defun shopping-to-calc (shopping-sexp)
  "Convert a shopping-lisp format quantity, e.g., (1.0 kg), to a
calc expression."
  (math-read-expr (format "%f %s" (float (first shopping-sexp))
                          (symbol-name (second shopping-sexp)))))

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
       (numberp (first quantity))
       (symbolp (second quantity))))

(defun shopping-is-unitless-quantity (quantity)
  "Test whether a quantity appearing in the :ingredients list in
a recipe is unitless but not nil. E.g., (750 g) would return
false, (3) would return true, nil would return false."
  (and (= 1 (length quantity))
       (numberp (first quantity))))

(defun shopping-is-unspecified-quantity (quantity)
  "Test whether a quantity appearing in the :ingredients list in
a recipe is nil, i.e., unspecified."
  (eq nil quantity))

(defun shopping-add-composite-quantities (quantity1 quantity2)
  "Composite quantities are lists of length three. The first
element is nil or t and indicates that an unspecified amount was
required in some recipe. The second element is a number
representing a unitless quantity, and the third is an even-length
list of pairs representing unitful quantities, e.g., (2.0 kg 3
g).

Example:
 (setq q1 '(nil 0 (2.0 kg 3.0 g)))
 (setq q2 '(nil 1 (3.0 kg)))
 (shopping-add-composite-quantities q1 q2)

 ->  (nil 1 (2.0 kg 3.0 g 3.0 kg))

Note: the append function is used to concatenate the unitful
quantities. From the Emacs lisp manual: 'All arguments except the
last one are copied, so none of the arguments is altered.' So in
the above example, q1 and q2 are not altered.
"
  (list (or (first quantity1) (first quantity2))
         (+ (second quantity1) (second quantity2))
        (append (third quantity1) (third quantity2))))

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
   --> (nil 0 (10 g))

This function will not affect the input list, but note that for
unitful quantities (third example above), the input list will
become part of the output list (i.e., the input list is not
copied).
"
  (cond ((shopping-is-unspecified-quantity quantity) '(t 0 ()))
        ((shopping-is-unitless-quantity quantity) `(nil ,(first quantity) ()))
        ((shopping-is-unitful-quantity quantity) `(nil 0 ,quantity))))

(defun shopping-add-ingredient-to-shopping-list (shopping-list ingredient-info)
  "Add an ingredient to the given shopping list. Note that
shopping-list is altered by this function.

shopping list is a symbol pointing to an alist from ingredient
name to quantity information. Note: the symbol must be
quoted (see example below). This should probably be changed, it
doesn't feel right.

ingredient-info is a list of the form (name quantity unit),
or (name quantity), or (name), i.e., an element from
the :ingredients list in a recipe,

Note that it doesn't actually sum up the unitful
quantities. Rather, they're just collected into a list. The
summing up comes on string conversion (perhaps this should be
changed?).

Example:
  (setq my-shopping '())
  (shopping-add-ingredient-to-shopping-list 'my-shopping '(\"Unsalted butter\" 50 g))
  (shopping-add-ingredient-to-shopping-list 'my-shopping '(\"Unsalted butter\" 100 g))
  (shopping-add-ingredient-to-shopping-list 'my-shopping '(\"Cashew nuts\"))
    --> ((\"Cashew nuts\" t 0 nil) (\"Unsalted butter\" nil 0 (50 g 100 g)))

Note: there are no '.' symbols in the elements of the resulting
alist. That's because the second value in each element is a
list. E.g., (1 2 3 4) is just another way of writing (1 . (2 3 4)),
which can be verified by evaluating '(1 . (2 3 4))
"
  (let* ((sl-quantity
          (shopping-quantity-to-composite
           (shopping-get-quantity-from-ingredient-expr ingredient-info)))
         (ingredient (first ingredient-info))
         (item (assoc ingredient (eval shopping-list))))
    (if (eq item nil)
        (add-to-list shopping-list `(,ingredient . ,sl-quantity))
      (progn
        (setf (cdr item) (shopping-add-composite-quantities (cdr item) sl-quantity))
        (eval shopping-list)))))

(defun shopping-add-substitutable-ingredients-to-shopping-list
  (shopping-list substitutable-ingredient-list)
  "Given a shopping list and a list of n substitutable
ingredients, returns n new shopping lists, one for each
variation. Does not modify input lists. All the returned lists
use copies of the original shopping-list, produced by copy-tree.

Note that shopping-list input must be a quoted symbol (see
example below). As for shopping-add-ingredient-to-shopping-list,
this doesn't feel right and should probably be changed.

Example:

  (setq my-shopping '((\"Cashew nuts\" t 0 nil)))
  (shopping-add-substitutable-ingredients-to-shopping-list 'my-shopping
      '((\"Black pepper\") (\"Red pepper\")))

   --> (((\"Red pepper\" t 0 nil) (\"Cashew nuts\" t 0 nil))
        ((\"Black pepper\" t 0 nil) (\"Cashew nuts\" t 0 nil)))

"
  (loop with result = '()
        for i from 0
        while (< i (length substitutable-ingredient-list))
        do
        (let ((branched-list (copy-tree (eval shopping-list)))
              (ingredient-info (nth i substitutable-ingredient-list)))
          (progn
            (shopping-add-ingredient-to-shopping-list 'branched-list ingredient-info)
            (add-to-list 'result branched-list)))
        finally
        return result))

(defun shopping-add-recipe-to-shopping-lists (shopping-lists recipe)
  "shopping-lists is a list of shopping lists. We need this
because whenever there's an 'or' ingredient, a shopping list
branches into two or more lists. This function does not modify
the input lists, it creates deep copies and returns those
instead."
  (let ((ingredients (getf recipe ':ingredients)))
    (loop with results = (copy-tree shopping-lists)
          for i from 0
          while (< i (length ingredients))
          do
          (let ((ingredient-info (nth i ingredients)))
            (if (stringp (first ingredient-info)) ; a single
                                                  ; ingredient rather
                                                  ; than a list of
                                                  ; substitutable
                                                  ; ingredients
                (setq results
                      (mapcar (lambda (sl)
                                (shopping-add-ingredient-to-shopping-list
                                 'sl ingredient-info))
                        results))
              (let ((branched-results '())) ; the else clause for
                                            ; above if, so
                                            ; ingredient-info is a
                                            ; list of substitutable
                                            ; ingredients
                (loop for j from 0
                      while (< j (length results))
                      do
                      (setq branched-results (append branched-results
                       (shopping-add-substitutable-ingredients-to-shopping-list
                        '(nth j results)
                        ingredient-info)))
                      finally
                      (setq results branched-results)))))
          finally
          return results)))

(defun shopping-sum-quantities (quantity-list)
  "Sum up quantities in an even-length list.
Example:
  (shopping-sum-quantities '(5 kg 10 lb))
    --> (* (float 95359237 -7) (var kg var-kg))
"
  (if (= 2 (length quantity-list))
      (shopping-to-calc quantity-list)
    (math-simplify-units
     (list '+
       (shopping-to-calc (list (first quantity-list) (second quantity-list)))
       (shopping-sum-quantities (cdr (cdr quantity-list)))))))

(defun shopping-shopping-list-entry-to-string (entry)
  (let* ((ingredient (first entry))
         (output-strings '()))
    (progn
      (if (second entry)
          (add-to-list 'output-strings ingredient))
      (setq n-items (third entry))
      (if (> n-items 0)
          (if (integerp n-items)
          (add-to-list 'output-strings
                       (format "%i %s" n-items ingredient))
        (add-to-list 'output-strings
                       (format "%1.1f %s" n-items ingredient))))
      (if (not (eq nil (fourth entry)))
          (add-to-list 'output-strings
                       (format "%s %s"
                               (math-format-value
                                (shopping-sum-quantities (fourth entry)))
                               ingredient)))
      (mapconcat 'identity output-strings ", "))))

(defun shopping-get-ingredient-category (ingredient)
  (getf (lax-plist-get shopping-ingredients ingredient) :class))

(defun shopping-shopping-list-by-category (shopping-list)
  (let ((shopping-alist '()))
    (dolist (ingredient-info shopping-list)
      (let* ((ingredient (first ingredient-info))
             (category (shopping-get-ingredient-category ingredient)))
        (if (eq nil category)
            (error "Error: no ingredient information for %s" ingredient))
        (if (eq nil (cdr (assoc category shopping-alist)))
            (setq shopping-alist (cons `(,category . (,ingredient-info))
                                       shopping-alist))
          (nconc (cdr (assoc category shopping-alist)) `(,ingredient-info))
          )))
    shopping-alist))

(defun shopping-pprint-shopping-list-by-category (shopping-list)
  (let ((by-category (shopping-shopping-list-by-category shopping-list))
        (output-string ""))
    (dolist (category by-category)
      (setq output-string (format "%s\n%s" output-string (car category)))
      (dolist (ingredient (cdr category))
        (setq output-string
              (format "%s\n  %s"
                      output-string
                      (shopping-shopping-list-entry-to-string ingredient)))))
    output-string))

(defun shopping-pprint-shopping-list (shopping-list)
  (if (not (length shopping-list))
      ""
    (let ((first-entry-string
           (shopping-shopping-list-entry-to-string (car shopping-list))))
      (if (= 1 (length shopping-list))
          first-entry-string
        (format "%s\n%s" first-entry-string
                (shopping-pprint-shopping-list (cdr shopping-list)))))))

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
          (string< (first ing1) (first ing2)))))

;; Before getting this working, makes sense to change the way shopping
;; lists work so that they contain summed-up unitful quantities rather
;; than lists of unitful quantities. Then it's easier to take the
;; difference between two ingredients. Problem: sometimes ingredients
;; are specified by volume, sometimes by weight, and we don't want to
;; maintain density for all ingredients. A unitful quantity should
;; therefore be a pair consisting of amount by weight and amount by
;; volume.

;; (defun shopping-set-intersection (list-of-sets)
;;   "Return a new list with the intersection of the given
;; sets (just lists)."
;;   (if (= (length list-of-sets) 1)
;;       (copy-tree (first list-of-sets))
;;     (if (> (length list-of-sets) 2)
;;         (shopping-set-intersection
;;          (append `((,first list-of-sets))
;;                  (shopping-set-intersection (cdr list-of-sets))))
;;       (let ((intersection '()))
;;         (dolist element (first list-of-sets)
;;                 (if (memq element (second list-of-lists))
;;                     ))))))  ;; up to here

;; Will use this to pull out the common ingredients from a set of
;; shopping lists, and then go through each list removing the common
;; ingredients to get the difference lists.

;; (defun shopping-get-common-ingredients (shopping-lists)
;;   )

(defun shopping-prepare-list ()
  (interactive)
  (shopping-reload-data)
  (let ((recipe-buffer (get-buffer-create "*Recipes*"))
        (shopping-lists '(())))
    (set-buffer recipe-buffer)
    (erase-buffer)
    (display-buffer recipe-buffer)
    (princ "--- List of recipes ---\n\n" recipe-buffer)
    (dotimes (i (length shopping-recipes))
      (princ (format "%i %s\n" i (getf (nth i shopping-recipes) :title))
             recipe-buffer))
    (setq recipe-numbers (shopping-get-digits-from-user))
    (dolist (n recipe-numbers)
      (setq shopping-lists
            (shopping-add-recipe-to-shopping-lists shopping-lists
                                                   (nth n shopping-recipes))))
    (setq shopping-list-buffer (get-buffer-create "*Shopping lists*"))
    (set-buffer shopping-list-buffer)
    (erase-buffer)
    (princ "   *** Recipes ***\n\n" shopping-list-buffer)
    (dolist (n recipe-numbers)
      (princ (format "%s\n"(getf (nth n shopping-recipes) :title))
             shopping-list-buffer))
    (dotimes (i (length shopping-lists))
      (princ (format "\n   *** List %i ***\n%s\n"
                     i
                     (shopping-pprint-shopping-list-by-category
                      (nth i shopping-lists)))
             shopping-list-buffer))
    (display-buffer "*Shopping lists*")))
