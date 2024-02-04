| NOTE        |
|:---------------------------|
| Work in progress: English translation has been generated locally from French with [libretranslate](https://github.com/LibreTranslate/LibreTranslate). It is far from correct, and for testing purposes only. |

Internationalization is the process of easing the use of different languages in a software <sup>[1](https://journal.r-project.org/articles/RN-2005-001/RN-2005-001.pdf)</sup>.

To facilitate `easieR` translation, files in this folder contain *almost* every strings (work in progress) visible to easieR's users.
Each of the strings they contain is associated to a variable's name (its specific identifier).
Those variables act like placeholders in easieR code, pointing to their matching string.
Files in this folder can therefore be seen as mutable dictionnaries.
Any number of dictionnaries can be created for different languages.

# Placeholders

To avoid collision with future or already existing variables' names in easieR scripts, placeholders must have unique name identifiers.  
I chose to make them start with recognizable patterns, and at the moment, there are three types of placeholders :

- Interactive : start with `ask_`. They are asking the user to do something.
- Informative : start with `desc_`. They describe/indicate something (titles, windows names, how X works, etc.)
- Uncategorized : start with `txt_`. TODO.

Those categories are not well delimited, but it will be quite easy to modify them in the future.

# Hard coded strings

Some strings needs to be hard coded and must stay in place in easieR code, for example packages names (`BayesFactor`, `outliers`,...) or singular variables like `choix` or `outlier` in `./R/easieR.R`.
To avoid mistakes during translation, I surrounded them with single quotes (`'`) instead of double quotes (`"`) (work in progress).
Double quotes are now reserved for placeholders (i.e. replacable strings).

# For programmers

To continue coding on easieR *with* strings displayed in place of placeholders, one script will help switching between strings and placeholders (TODO).

# Did it break `easieR`?

Testing same analysis on same dataset gave exact similar results for:

- descriptive statistics
- chi squared (adjustement, independance, McNemar)
- correlations (detailed analysis, matrix correlation (except partial without outliers), two correlations)
- t.tests (comparison to norm, paired, independant(except with bootstraps and bayesian))
- ANOVA (independant, repeated, mixt)
- regressions (classic)


Not working with:
- _

Not tested on :
- correlations > matrix partial correlations removing outliers : just loading the scripts in current and official branch give "unused argument (X = c(X, Y, Z))" in VI.multiples
- correlations > other correlations
- t.test independant full analysis (robusts + bayesian not working) 
- ancova
- regressions (logistics, mediation effect)
- factor component analysis
- reliability and agreement

## easieR bugs?

- Correlations (detailed analysis with all parameters) outputs an error when asking for `Bayesian Factors`.
```
# outputs this error (on both main and translation versions) :
  invalid class “ddenseModelMatrix” object: superclass "xMatrix" not defined in the environment of the object's class
```

To fix this : `install.packages("MatrixModels", type = "source")`. See [here](https://stackoverflow.com/questions/77530214/error-dsparsemodelmatrix-object-superclass-xmatrix-not-defined/77530730#77530730) or [here](https://stat.ethz.ch/pipermail/r-package-devel/2023q4/010054.html) for an explaination.


- t test independant (full analysis) => `Error in ksties.crit(x, y, alpha = alpha) : object 'n1' not found`.
works when unselect bayesian and bootstrap.

# Limitations

## Data interpolation 

When messages need data interpolation, the current method for method for i18n shows limitations.
For example in `./R/chi.R`:

```
msg6 <- paste0("Les effectifs sont insuffisants pour le nombre de combinaisons entre la variable ", comb[trop[i],1], " et la variable ", comb[trop[i],2], ". Cette analyse ne sera pas realisee.")
```

msg6 wraps around two variables. How to proceed ?

## `switch` doesn't accept variables 

`switch` does not allow variables to pattern match against `EXPR`.
I replaced every `switch` by `if..else` conditions.

## Placeholders variables' names are easily visible on the command line

Potentially annoying. Moreove those variables are made global (might not be good practise)

# Testing 

To install this branch use `R --vanilla` and `remotes::install_github("dougy147/easieR",ref="translation_one_file")`.
I had to remove my previous easieR install in my library to make this work.

Use those function to switch languages:
```
load_en_EN()
load_fr_FR()
```

# Bugs

- Redundancy in reports : t.tests always include "without outliers" section even when none was found.

# Fixes

- Reports correctly display placeholders

# TODO

- Find the right place to detect user's language and load dictionnary accordingly.
- Add "choose language" item in easieR
- Make placeholders variables names less visible to command line users
