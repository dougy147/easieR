Internationalization is the process of easing the use of different languages in a software <sup>[1](https://journal.r-project.org/articles/RN-2005-001/RN-2005-001.pdf)</sup>.

To facilitate translating `easieR`, files in this folder contain *almost* every strings (work in progress) that are to be displayed to the users, associated to specific variables'.
Those variables act like placeholders in easieR code, pointing to the adequat string.

# Placeholders

To avoid collision with future or already existing variables' names in easieR scripts, placeholders must have unique name identifiers.  
At the moment, there are three types of placeholders :

- Interactive : start with `ASK_`. They are asking the user to do something.
- Informative : start with `DESC_`. They describe/indicate something (titles, windows names, how X works, etc.)
- Uncategorized : start with `TXT_`. TODO.

Those categories are not well delimited, but it will be quite easy to modify them in the future.

# For programmers

To continue coding on easieR *with* strings displayed in place of placeholders, one script will help switching between strings and placeholders.

# Limitations (TODO)

When messages need data interpolation, the current method shows limitations.
For example in `chi.R`:

```
msg6 <- paste0("Les effectifs sont insuffisants pour le nombre de combinaisons entre la variable ", comb[trop[i],1], " et la variable ", comb[trop[i],2], ". Cette analyse ne sera pas realisee.")
```

msg6 wraps two variables around. How to proceed ?
