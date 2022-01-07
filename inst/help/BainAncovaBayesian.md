Bain ANCOVA
==========================

Bain (Bayesian informative hypotheses evaluation) ANCOVA allows you to evaluate (informative) hypotheses using the Bayes factor. A simple example would be the Bayesian evaluation of H0: m1 = m2 = m3 versus H1: m1 > m2 > m3 versus Hu: no restrictions on the three adjusted means. Read Hoijtink, Mulder, van Lissa, and Gu (2019) for an introduction to bain. Bain has been thoroughly debugged, if nevertheless problems are detected they will be posted at https://informative-hypotheses.sites.uu.nl/software/bain/.

### Specification of the bain ANCOVA

- Choose the dependent variable from the variable list and move it to the Dependent Variable box. Note that, the name of the dependent variable has to start with a letter and may further consist of letters, numbers and _
- Choose the factor from the variable list and move it to the Fixed Factors box. Note that, the factor name has to start with a letter and may further consist of letters, numbers and _. Note furthermore that, all groups have to be collected in ONE factor. If you have, for example, a factor with the levels young-old and a factor with the levels female-male, you have to create ONE new factor with the levels youngfemale, oldfemale, youngmale, oldmale. The levels are indicated by numbers or have names that start with a letter and may further consist of letters, numbers and _.
- Set the seed equal to an integer number to create a repeatable random number sequence. It is recommended to run analyses with two different seeds to ensure stability of the results.
- The default value for fraction is equal to 1. It renders a Bayes factor that somewhat favors the null-hypothesis. If, additionally, values of 2 and 3 are used, you execute a so-called sensitivity analysis (see the tutorial by Hoijtink, Mulder, van Lissa, and Gu, 2019).
- Choose the covariate(s) from the variable list and move it/them to the Covariates box. Note that, the name of the covariate(s) has to start with a letter and may further consist of letters, numbers and _.
- When you execute bain ANCOVA for the first time tick both additional statistics and both plots. When you return to bain ANCOVA you will know what each of these four options renders and you can tick only the options you need.
- By default 95% credible intervals will be presented in the results. If desired the degree of belief (by default 95%) can be changed.
- When you tick model constraints a box opens in which you can specify the hypotheses you want to evaluate. You need to adhere to the following specification rules:

1. Place each hypothesis on a separate line.
2. The levels of the ONE factor are referred to as follows: `factorlevelname`. If, for example, there is a factor age with levels y, m, o. They are reffered to using `agey`, `agem`, and `ageo`, respectively.
3. Linear combinations of parameters must be specified adhering to the following rules:
  - Each parameter name is used at most once.
  - Each parameter name may or may not be pre-multiplied with a number.
  - A constant may be added or subtracted from each parameter name.
  - A linear combination can also be a single number.
  - Examples are: `3 * agey + 5`; `agey + 2 * agem + 3 * ageo - 2`; `agey - ageo`; and `5`.
4. (Linear combinations of) parameters can be constrained using <, >, and =. For example, `agey > 0` or `agey > agem = 0` or `2 * agey < agem + ageo > 5`.
5. The ampersand & can be used to combine different parts of a hypothesis. For example, `agey > agem & agem > ageo` which is equivalent to `agey > agem > ageo` or `agey > 0 & agem > 0 & ageo > 0`.
6. Sets of (linear combinations of) parameters subjected to the same constraints can be specified using (). For example,`agey > (agem,ageo)` which is equivalent to `agey > agem & agey > ageo`.

Hypotheses have to be compatible, non-redundant and possible. What these terms mean will be elaborated below.

*The set of hypotheses has to be compatible*. For the statistical background of this requirement see Gu, Mulder, Hoijtink (2018). Usually the sets of hypotheses specified by researchers are compatible, and if not, bain will return an error message. The following steps can be used to determine if a set of hypotheses is compatible:

- Replace a range constraint, e.g., `1 < agey < 3`, by an equality constraint in which the parameter involved is equated to the midpoint of the range, that is, `agey = 2`.
- Replace in each hypothesis the < and > by =. For example, `agey = agem > ageo` becomes `agey = agem = ageo`.
- The hypotheses are compatible if there is at least one solution to the resulting set of equations. For the two hypotheses considered above, the solution is `agey = agem = ageo = 2`. An example of two non-compatible hypotheses is `agey = 0` and `agey > 2` because there is no solution to the equations `agey=0` and `agey=2`.

*Each hypothesis in a set of hypotheses has to be non-redundant.* A hypothesis is redundant if it can also be specified with fewer constraints. For example, `agey = agem & agey > 0 & agem > 0` is redundant because it can also be specified as `agey = agem & agey > 0`. bain will work correctly if hypotheses specified using only < and > are redundant. bain  will return an error message if hypotheses specified using at least one = are redundant.

*Each hypothesis in a set of hypotheses has to be possible.* An hypothesis is impossible if estimates in agreement with the hypothesis do not exist. For example: values for `agey` in agreement with `agey = 0 & agey > 2` do not exist. It is the responsibility of the user to ensure that the hypotheses specified are possible. If not, bain will either return an error message or render an output table containing `Inf`'s.

### Results obtained after running bain ANCOVA

- To be able to properly interpret the results of a bain ANCOVA, you are required to read the tutorial by Hoijtink, Mulder, van Lissa, and Gu (2019) that can be retrieved from the Psychological Methods website or from the bain website at https://informative-hypotheses.sites.uu.nl/software/bain/
- If you want to understand the technical background of bain you should read Gu, Mulder, and Hoijtink (2018) and Hoijtink, Gu, and Mulder (2019) that can be retrieved from the British Journal of Mathematical and Statistical Psychology website or from the bain website at https://informative-hypotheses.sites.uu.nl/software/bain/
- Five pieces of results are obtained after running a bain ANCOVA:

1. The table in which the Bayes factor of each hypothesis specified versus the unconstrained hypothesis and its complement (that is, not the hypothesis), respectively, is presented. This table contains three sets of posterior model probabilities (each) based on equal prior model probabilities: PMPa, the posterior model probabilities of the hypotheses specified; PMPb, hypotheses specified plus Hu, the unconstrained hypothesis; and, PMPc, hypothesis specified plus Hc, that is, the joint complement of all hypotheses specified, that is, "not the hypotheses specified".
2. The Bayes factor matrix in which the mutual Bayes factors of the hypotheses specified in the Model Constraints box are presented.
3. A coefficients table containing for each group in the ANCOVA the sample size, the adjusted mean, standard error (se) and 95% credible interval. Furthermore, this table contains the same information for the covariate(s).
4. A plot of the pmp's (excluding and including the unconstrained hypothesis) visually highlighting the support in the data for each hypothesis entertained.
5. A plot of the adjusted means and their credible intervals.

### References

- Gu, X., Mulder, J., and Hoijtink, H. (2018). Approximate adjusted fractional Bayes factors: A general method for testing informative hypotheses. British Journal of Mathematical and Statistical Psychology, 71, 229-261. DOI: 10.1111/bmsp.12110
- Hoijtink, H., Mulder, J., van Lissa, C., and Gu, X. (2019). A tutorial on testing hypotheses using the Bayes factor. Psychological Methods, 24, 539-556. DOI: 10.1037/met0000201 
- Hoijtink, H., Gu, X., and Mulder, J. (2019). Bayesian evaluation of informative hypotheses for multiple populations. British Journal of Mathematical and Statistical Psychology, 72, 219-243. DOI: 10.1111/bmsp.12145
