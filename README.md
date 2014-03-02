shopping-lisp
=============

Emacs package to create shopping lists from recipes.

This aim of this package is to make grocery shopping easy. The user selects which recipes to make, and a shopping list is created with the ingredients nicely sorted into categories (vegetables, grains, meat, etc.). It is easy to add recipes and ingredient categories.

Recipes are quite flexible: ingredients can be input with a quantity (e.g., 100 ml of olive oil), a count (e.g., 10 bay leaves), or neither (e.g., flour). See recipes.el for examples. Many different units are supported, thanks to the calc package which is used under the hood.

It is also possible to specify ingredient alternatives (e.g., to specify either 1 green chilli or 1 red chilli put (("Red chillis" 1),("Green chillis" 1)) into the recipe's ingredient list). Currently this will result in multiple shopping lists being produced, one for each combination of ingredients, which is somewhat cumbersome; this functionality will hopefully be improved in future.

For installation and usage instructions, see the commentary in shopping-lisp.el.
