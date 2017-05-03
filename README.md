shopping-lisp: an Emacs package to create shopping lists from recipes
=====================================================================

This aim of this package is to make grocery shopping easy. You
select which recipes to make, and a shopping list is created with the
ingredients nicely sorted into categories (vegetables, fruit, grains,
etc.). It is easy to add recipes and ingredient categories.

The final shopping list is in Pandoc Markdown format, allowing easy conversion to many other formats (PDF, HTML, etc.), assuming you have Pandoc installed. A function is also provided which creates a PDF of the shopping list, in a format suitable for printing on standard index cards.

Recipes are quite flexible. Ingredients can be specified with a quantity

<code>("Olive oil" 100 ml)</code>,

a count

<code>("Bay leaves" 2)</code>,

or neither

<code>("Flour")</code>.

See recipes.el for more examples. Many different units are
supported, thanks to the calc package which is used under the hood.

It is also possible to specify ingredient alternatives. For example, *either* 1 green chilli or 1 red chilli would be represented like this in the ingredient list:

<code>(("Red chillis" 1) ("Green chillis" 1))</code>.

Installation
============

To install, put shopping-list.el somewhere Emacs can see it (i.e., in
one of the directories specified in the load-path variable), and put
the following in your <code>.emacs</code> file:

<code>(load "shopping-lisp.el")</code>

In addition, you must tell shopping-lisp where to find the recipe
file and the ingredient information file, by setting the variables
shopping-recipe-file and shopping-ingredient-file. For example, put
the following in your <code>.emacs</code> file:

<code>
(setq shopping-recipe-file "~/recipes.el")
</code>

<code>
(setq shopping-ingredient-file "~/ingredient-information.el")
</code>

Examples of both files are provided with the package.

Usage
=====

Prepare your shopping list by doing:

<code>M-x shopping-prepare-list</code>

This will pop up a buffer for recipe selection. Add/remove recipes using the +/- widgets. A count is displayed with each recipe for the number of times it has been added. For instance, if you add a recipe twice, the final ingredients list will include enough ingredients to make the recipe twice.

Once all desired recipes have been added, press the button 'Create shopping list'. This will display the shopping list in a buffer called <code>\*Shopping\*</code>.
From this buffer, it is easy to create a PDF by doing

<code>M-x shopping-list-to-pdf</code>

*NB: For this to work, you must have Pandoc and Latex installed.*

By default, the PDF will be 6 inches wide and 4 inches high. It is easy to buy index cards of this size.

To tweak the look of the PDF, you can edit the Pandoc latex template in the repo (<code>index_cards.latex.pandoc.template</code>), but of course this requires some familiarity with Pandoc and Latex. 
