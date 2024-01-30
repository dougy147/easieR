# Flexible translations

Internationalization is the process of facilitating the use of another language than English or American English [1](https://journal.r-project.org/articles/RN-2005-001/RN-2005-001.pdf).

To facilitate the translation of `easieR`, files in this folder are related to scripts in the parent folder (same name, appended with `_i18n`).

To avoid collision with current or future variables in the scripts, language variables have unique name identifiers.

# Interactive : Addressed to user (asking, pointing, etc.)
# Descriptive : Descriptive text (title, window name, etc.)
# Hard coded  : Variables text ("units" (e.g. p.values),

# Limitations (TODO)

When messages need data interpolation, the current method shows limitations.
For example in `chi.R`:

```
msg6 <- paste0("Les effectifs sont insuffisants pour le nombre de combinaisons entre la variable ", comb[trop[i],1], " et la variable ", comb[trop[i],2], ". Cette analyse ne sera pas realisee.")
```

msg6 wraps two variables around. How to proceed ?
