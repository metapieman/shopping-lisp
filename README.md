shopping-lisp
=============

Emacs package to create shopping lists from recipes.

This aim of this package is to make grocery shopping easy. The user
selects which recipes to make, and a shopping list is created with the
ingredients nicely sorted into categories (vegetables, fruit, grains,
etc.). It is easy to add recipes and ingredient categories.

Recipes are quite flexible: ingredients can be input with a quantity
(e.g., 100 ml of olive oil), a count (e.g., 10 bay leaves), or neither
(e.g., flour). See recipes.el for examples. Many different units are
supported, thanks to the calc package which is used under the hood.

It is also possible to specify ingredient alternatives (e.g., to
specify either 1 green chilli or 1 red chilli put (("Red chillis"
1) ("Green chillis" 1)) into the recipe's ingredient list). Currently
this will result in multiple shopping lists being produced, one for
each combination of ingredients, which is somewhat cumbersome; this
functionality will hopefully be improved in future.

Installation and usage
======================

To install, put shopping-list.el somewhere Emacs can see it (i.e., in
one of the directories specified in the load-path variable), and put
the following in your .emacs file:

<code>(load "shopping-lisp.el")</code>

In addition, you must tell shopping-lisp where to find the recipe
file and the ingredient information file, by setting the variables
shopping-recipe-file and shopping-ingredient-file. For example, put
the following in your .emacs:

<code>
(setq shopping-recipe-file "/home/johnsmith/recipes.el")
</code>

<code>
(setq shopping-ingredient-file "/home/johnsmith/ingredient-information.el")
</code>

Examples of both files are provided with the package (recipes.el,
ingredient-information.el).

Once installed, prepare your shopping list by doing:

<code>M-x shopping-prepare-list</code>
