shopping-lisp: an Emacs package to create shopping lists from recipes
=====================================================================

This aim of this package is to make grocery shopping easy. You
select which recipes to make, like so:

![recipe selection](/create_shopping_list.png?raw=true "Recipe selection buffer")

A shopping list is then created, with the
ingredients nicely sorted into categories (vegetables, fruit, grains,
etc.):

![ingredients](/ingredients.png?raw=true "Ingredients buffer")

It is easy to add recipes and ingredient categories.

The final shopping list is in Pandoc Markdown format, allowing easy conversion to many other formats (PDF, HTML, etc.), assuming you have Pandoc installed. A function is also provided which creates a PDF of the shopping list, in a format suitable for printing on standard index cards.

Recipes are quite flexible. Ingredients can be specified with a quantity

```("Olive oil" 100 ml)```

a count

```("Bay leaves" 2)```

or neither

```("Flour")```

See recipes.el for more examples. Many different units are
supported, thanks to the calc package which is used under the hood.

It is also possible to specify ingredient alternatives. For example, *either* 1 green chilli or 1 red chilli would be represented like this in the ingredient list:

```(("Red chillis" 1) ("Green chillis" 1))```

Installation
============

To install, put shopping-list.el somewhere Emacs can see it (i.e., in
one of the directories specified in the load-path variable), and put
the following in your `.emacs` file:

```(load "shopping-lisp.el")```

In addition, you must tell shopping-lisp where to find the recipe
file and the ingredient information file, by setting the variables
`shopping-recipe-file` and `shopping-ingredient-file`. For example, you might put
the following in your `.emacs` file to set the recipe file:

```
(setq shopping-recipe-file "~/recipes.el")
```

Similarly, for your ingredient file:

```
(setq shopping-ingredient-file "~/ingredient-information.el")
```

Examples of both files are provided with the package.

Usage
=====

Prepare your shopping list by doing:

```M-x shopping-prepare-list```

This will pop up a buffer for recipe selection. Add/remove recipes using the +/- widgets. A count is displayed with each recipe, to indicate the number of times it has been added. For instance, if you add a recipe twice, the final ingredients list will include enough ingredients to make the recipe twice.

Once all desired recipes have been added, press the button 'Create shopping list'. This will display the shopping list in a buffer called `*Shopping*`.
From this buffer, it is easy to create a PDF by doing

```M-x shopping-list-to-pdf```

*NB: For this to work, you must have Pandoc and Latex installed.*

By default, the PDF will be 6 inches wide and 4 inches high. It is easy to buy index cards of this size.

To tweak the look of the PDF, you can edit the Pandoc latex template in the repo (`index_cards.latex.pandoc.template`), but of course this requires some familiarity with Pandoc and Latex.
