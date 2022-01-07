Bain Linear Regression
==========================

Bain (Bayesian informative hypotheses evaluation)  linear regression allows you to evaluate (informative) hypotheses using the Bayes factor. A simple example would be the Bayesian evaluation of H0: b1 = b2 = b3 versus H1: b1 > b2 > b3 versus Hu: no restrictions on the three regression coefficients. Read Hoijtink, Mulder, van Lissa, and Gu (2019) for an introduction to bain. Bain has been thoroughly debugged, if nevertheless problems are detected they will be posted at https://informative-hypotheses.sites.uu.nl/software/bain/.

### Specification of the bain Linear Regression

- Choose the dependent variable from the variable list and move it to the Dependent Variable box. Note that, the name of the dependent variable has to start with a letter and may further consist of letters, numbers and _.
- Choose the predictors from the variable list and move them to the Covariates box. Note that, the name of the predictors has to start with a letter and may further consist of letters, numbers and _.
- Set the seed equal to an integer number to create a repeatable random number sequence. It is recommended to run analyses with two different seeds to ensure stability of the results.
- The default value for fraction is equal to 1. It renders a Bayes factor that somewhat favors the null-hypothesis. If, additionally, values of 2 and 3 are used, you execute a so-called sensitivity analysis (see the tutorial by Hoijtink, Mulder, van Lissa, and Gu, 2019).
- When you execute bain Linear Regression for the first time tick both additional statistics and the plot. When you return to bain Linear Regression you will know what each of these four options renders and you can tick only the options you need.
- If your hypotheses involve the comparison of regression coefficients, for example, b1 = b2 = b3, then the regression coefficients have to be on the same scale. You can achieve this by ticking the Standardize box under additional options, which implies that your hypotheses and the results are in terms of standardized regression coefficients.
- When you tick model constraints a box opens in which you can specify the hypotheses you want to evaluate. You need to adhere to the following specification rules:

1. Place each hypothesis on a separate line.
2. The regression coefficients are referred to using the names of the predictors to which they correspond, for example, age, weight and size, if those are the names of the predictors placed in the Covariates box.
3. Linear combinations of parameters must be specified adhering to the following rules:
  - Each parameter name is used at most once.
  - Each parameter name may or may not be pre-multiplied with a number.
  - A constant may be added or subtracted from each parameter name.
  - A linear combination can also be a single number.
  - Examples are: `3 * age + 5`; `age + 2 * weight + 3 * size - 2`; `age - weight`; and `5`.
4. (Linear combinations of) parameters can be constrained using <, >, and =. For example, `age > 0` or `age > weight = 0` or `2 * age < weight + size > 5`.
5. The ampersand & can be used to combine different parts of a hypothesis. For example, `age > weight & weight > size` which is equivalent to `age > weight > size` or `age > 0 & weight > 0 & size > 0`.
6. Sets of (linear combinations of) parameters subjected to the same constraints can be specified using (). For example,`age > (weight,size)` which is equivalent to `age > weight & age > size`.

Hypotheses have to be compatible, non-redundant and possible. What these terms mean will be elaborated below.

*The set of hypotheses has to be compatible*. For the statistical background of this requirement see Gu, Mulder, Hoijtink (2018). Usually the sets of hypotheses specified by researchers are compatible, and if not, bain will return an error message. The following steps can be used to determine if a set of hypotheses is compatible:

- Replace a range constraint, e.g., `1 < age < 3`, by an equality constraint in which the parameter involved is equated to the midpoint of the range, that is, `age = 2`.
- Replace in each hypothesis the < and > by =. For example, `age = weight > size > ses` becomes `age = weight = size = ses`.
- The hypotheses are compatible if there is at least one solution to the resulting set of equations. For the two hypotheses considered above, the solution is `age = weight = size = ses = 2`. An example of two non-compatible hypotheses is `age = 0` and `age > 2` because there is no solution to the equations `age=0` and `age=2`.

*Each hypothesis in a set of hypotheses has to be non-redundant.* A hypothesis is redundant if it can also be specified with fewer constraints. For example, `age = size & age > 0 & size > 0` is redundant because it can also be specified as `age = size & age > 0`. bain will work correctly if hypotheses specified using only < and > are redundant. bain  will return an error message if hypotheses specified using at least one = are redundant.

*Each hypothesis in a set of hypotheses has to be possible.* An hypothesis is impossible if estimates in agreement with the hypothesis do not exist. For example: values for `age` in agreement with `age = 0 & age > 2` do not exist. It is the responsibility of the user to ensure that the hypotheses specified are possible. If not, bain will either return an error message or render an output table containing `Inf`'s.

### Results obtained after running bain Linear Regression

- To be able to properly interpret the results of a bain Linear Regression, you are required to read the tutorial by Hoijtink, Mulder, van Lissa, and Gu (2019) that can be retrieved from the Psychological Methods website or from the bain website at https://informative-hypotheses.sites.uu.nl/software/bain/
- If you want to understand the technical background of bain you should read Gu, Mulder, and Hoijtink (2018) and Hoijtink, Gu, and Mulder (2019) that can be retrieved from the British Journal of Mathematical and Statistical Psychology website or from the bain website at https://informative-hypotheses.sites.uu.nl/software/bain/
- Four pieces of results are obtained after running a bain Linear Regression:

1. The table in which the Bayes factor of each hypothesis specified versus the 
unconstrained hypothesis and its complement (that is, not the hypothesis),
respectively, is presented. This table contains three sets of posterior model
probabilities (each) based on equal prior model probabilities: PMPa, the posterior
model probabilities of the hypotheses specified; PMPb, hypotheses specified plus
Hu, the unconstrained hypothesis; and, PMPc, hypothesis specified plus Hc, that
is, the joint complement of all hypotheses specified, that is, "not the
hypotheses specified".
2. The Bayes factor matrix in which the mutual Bayes factors of the hypotheses specified in the Model Constraints box are presented.
3. A descriptives table containing the estimates of the regression coefficients, their standard error (se) and 95% credible interval.
4. A plot of the pmp's (excluding and including the unconstrained hypothesis) visually highlighting the support in the data for each hypothesis entertained.

### References

- Gu, X., Mulder, J., and Hoijtink, H. (2018). Approximate adjusted fractional Bayes factors: A general method for testing informative hypotheses. British Journal of Mathematical and Statistical Psychology, 71, 229-261. DOI: 10.1111/bmsp.12110
- Hoijtink, H., Mulder, J., van Lissa, C., and Gu, X. (2019). A tutorial on testing hypotheses using the Bayes factor. Psychological Methods, 24, 539-556. DOI: 10.1037/met0000201 
- Hoijtink, H., Gu, X., and Mulder, J. (2019). Bayesian evaluation of informative hypotheses for multiple populations. British Journal of Mathematical and Statistical Psychology, 72, 219-243. DOI: 10.1111/bmsp.12145