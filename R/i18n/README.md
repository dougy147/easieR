| NOTE        |
|:---------------------------|
| Work in progress: English translation has been generated locally from French with [libretranslate](https://github.com/LibreTranslate/LibreTranslate). It is far from correct, and for testing purposes only. |


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

# Hard coded strings

Some strings needs to be hard coded and should probably stay like that, for example `choix` and `outlier` in `./R/easieR.R`.
To avoid mistakes during translation, I replaced double quotes (`"`) by single quotes (`'`).
Thsi way, double quotes would be reserved for placeholders while single for remaining strings.

# For programmers

To continue coding on easieR *with* strings displayed in place of placeholders, one script will help switching between strings and placeholders.

# Did it break `easieR`?

Testing same analysis on same dataset gave exact similar results for:

- descriptive statistics
- chi squared (adjustement, independance, McNemar)
- correlations (detailed analysis, matrix correlation (except partial without outliers), two correlations)


Not working with:
- t.tests

Not tested :
- correlations > matrix partial correlations removing outliers : just loading the scripts in current and official branch give "unused argument (X = c(X, Y, Z))" in VI.multiples
- correlations > other correlations

## easieR bugs?

- Correlations (detailed analysis with all parameters) outputs an error when asking for `Bayesian Factors`.
```
# outputs this error (on both main and translation versions) :
  invalid class “ddenseModelMatrix” object: superclass "xMatrix" not defined in the environment of the object's class
```

# Limitations

## String interpolation 

When messages need data interpolation, the current method shows limitations.
For example in `chi.R`:

```
msg6 <- paste0("Les effectifs sont insuffisants pour le nombre de combinaisons entre la variable ", comb[trop[i],1], " et la variable ", comb[trop[i],2], ". Cette analyse ne sera pas realisee.")
```

msg6 wraps two variables around. How to proceed ?

# Bugs

## Switch case do not work with placeholders

`switch` does not always allow to use variables for pattern matching.
I have replace some of them, but more testing is necessary


# Fixes

## Reports

When generating reports, placeholders were displayed instead of the string.
This is because assignment with dollar operator `$` refers to its right operand as a string, never has a variable with a value.
This is fixed by checking the type of `titres` and adjust given it is or not refering to a string from de LANG dictionnary.
