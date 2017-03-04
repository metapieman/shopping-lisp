shopping-lisp
=============

Emacs package to create shopping lists from recipes.

This aim of this package is to make grocery shopping easy. You
select which recipes to make, and a shopping list is created with the
ingredients nicely sorted into categories (vegetables, fruit, grains,
etc.). It is easy to add recipes and ingredient categories.

The final shopping list is in Pandoc Markdown format, so it can easily
be converted to many other formats such as PDF or HTML, although it is
perfectly readable as-is.

Recipes are quite flexible: ingredients can be input with a quantity:

<code>("Olive oil" 100 ml)</code>,

a count:

<code>("Bay leaves" 2)</code>,

or neither:

<code>("Flour")</code>.

See recipes.el for more examples. Many different units are
supported, thanks to the calc package which is used under the hood.

It is also possible to specify ingredient alternatives. For example, *either* 1 green chilli or 1 red chilli would be represented like this in the ingredient list:

<code>(("Red chillis" 1) ("Green chillis" 1))</code>.

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
