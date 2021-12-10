bain Structural Equation Modeling
=================================

bain (Bayesian informative hypotheses evaluation) structural equation modeling allows you to evaluate (informative) hypotheses using the Bayes factor. A simple example would be the Bayesian evaluation of `H0: A=~x1 = A=~x2 = A=~x3` versus `H1: A=~x1 > A=~x2 > A=~x3` versus `Hu:` no restrictions on the three factor loadings, where `A=~x1` denote the loading of observed variable `x1` on latent factor `A` in lavaan (https://lavaan.ugent.be/) notation. Read the lavaan tutorial for further information about the specification of structural equation models in lavaan and specifically the sections: Model syntax 1, A SEM example, and Multiple groups. Read van Lissa et al. (2020) for an introduction and illustration of bain Structural Equation Modeling. This paper also highlights what can and cannot be done with bain structural equation modeling. Notably, bain can only  process multiple group models if the lavaan model is specified without between group restrictions. Read Hoijtink, Mulder, van Lissa, and Gu (2019) for an introduction to bain. Bain has been thoroughly debugged, if nevertheless problems are detected they will be posted at https://informative-hypotheses.sites.uu.nl/software/bain/.

### Specification of the Structural Equation Model Using lavaan

- Specify your structural equation model in box at the top of the input screen. Note that, the name of variables has to start with a letter and may further consist of letters, numbers and `_`. The model specified will be processed with the `sem()` command from the lavaan package. Using this command confirmatory factor models, structural equation models, and growth curve models can be specified.
- If your model contains latent variables its parameters can only be estimated if either the latent variables are standardized (choose "Factor Variance" from the "Factor Scaling" box) or some of the factor loadings are fixed to 1 (choose "Factor Loading" from the "Factor Scaling" box).
- In case of a multiple group model choose the variable specifying the groups by opening the "Grouping variable" box.

Once you have arrived here, go back to the box containing your model specification. Press ctrl + enter on a PC or cmd + enter on an Apple to estimate the parameters of your model. Next tick the coefficients box, a table will appear containing the names of the parameters in your model, their estimates, and credible intervals. The parameter names presented in this table can be used to specify informative hypotheses in the box that opens when you click on "Model Constraints". This will be elaborated in the next section. In case of multiple group models, parameter names end with ".groupname" to denote the groups to which they apply. Tick the "Plots" boxes if you want to see a Path diagram, the estimates and a legend.

### Specification of the hypotheses to be evaluated

Before you specify hypotheses, you have to arrange the following:

- Set the seed equal to an integer number to create a repeatable random number sequence. It is recommended to run analyses with two different seeds to ensure stability of the results.
- The default value for fraction is equal to 1. It renders a Bayes factor that somewhat favors the null-hypothesis. If, additionally, values of 2 and 3 are used, you execute a so-called sensitivity analysis (see the tutorial by Hoijtink, Mulder, van Lissa, and Gu, 2019).
- If your hypotheses involve the comparison of parameters, for example, `y~x1 = y~x2`, that is, the regression coefficients of `x1` and `x2` on `y` are equal, then the parameters have to be on the same scale. You can achieve this by ticking the Standardize box under additional options, which implies that your hypotheses and the results are in terms of standardized model parameters.
- When you execute bain Structural Equation Modeling for the first time tick Tables and Plots. When you return to bain Structural Equation Modeling you will know what each of these four options renders and you can tick only the options you need.

When you tick "Model Constraints" a box opens in which you can specify the hypotheses you want to evaluate.
You need to adhere to the following specification rules:

1. Place each hypothesis on a separate line (just keep typing, a new line is only made after you press enter).
2. The parameters of you structural equation model can be referred to using the names from the table that appears when you tick the "Coefficients" box. For example, `A=~x1` denotes the factor loading of observed variable `x1` on factor `A`, `x1~1` denotes the intercept of `x1`, and `y~x1` denotes the regression coefficient of `x1` on `y`. DO NOT USE SPACES IN THE PARAMETER NAMES. Note that, in case of multiple group models parameters names are of the form `y~x1.boy` or `y~x1.1`, where the last part of each name denotes that the  parameter at hand belong to the boys and group 1, respectively.
3. Linear combinations of parameters must be specified adhering to the following rules:
  - Each parameter name is used at most once.
  - Each parameter name may or may not be pre-multiplied with a number.
  - A constant may be added or subtracted from each parameter name.
  - A linear combination can also be a single number.
  - Examples are: `3 * y~x1 + 5; y~x1 + 2 * y~x2 + 3 * y~x3 - 2; y~x1 - y~x2;` and `5`.
4. (Linear combinations of) parameters can be constrained using `<`, `>`, and `=`. For example, `y~x1 > 0` or
`y~x2 > y~x3 = 0` or `2 * y~x1 < y~x2 + y~x3 > 5`.
5. The ampersand & can be used to combine different parts of a hypothesis. For example, `y~x1 > y~x2
& y~x2 > y~x3` which is equivalent to `y~x1 > y~x2 > y~x3` or `y~x1 > 0 & y~x2 > 0 & y~x3 > 0`.
6. Sets of (linear combinations of) parameters subjected to the same constraints can be specified using `()`.
For example,`y~x1 > (y~x2,y~x3)` which is equivalent to `y~x1 > y~x2 & y~x1 > y~x3`.

Hypotheses have to be compatible, non-redundant and possible. What these terms mean will be elaborated
below.

*The set of hypotheses has to be compatible*. For the statistical background of this requirement see Gu, Mulder, Hoijtink (2018). Usually the sets of hypotheses specified by researchers are compatible, and if not, bain will return an error message. The following steps can be used to determine if a set of hypotheses is compatible:
- Replace a range constraint, e.g., `1 < y~x1 < 3`, by an equality constraint in which the parameter
involved is equated to the midpoint of the range, that is, `y~x1 = 2`.
- Replace in each hypothesis the `<` and `>` by `=`. For example, `y~x1 = y~x2 > y~x3 > y~x4` becomes `y~x1 = y~x2 = y~x3 = y~x4`.
- The hypotheses are compatible if there is at least one solution to the resulting set of equations. For the two hypotheses considered above, the solution is `y~x1 = y~x2 = y~x3 = y~x4`. An example of two non-compatible hypotheses is `y~x1 = 0` and `y~x1 > 2` because there is no solution to the equations `y~x1=0` and `y~x1=2`.

*Each hypothesis in a set of hypotheses has to be non-redundant*. A hypothesis is redundant if it can also be specified with fewer constraints. For example, `y~x1 = y~x2 & y~x1 > 0 & y~x2 > 0` is redundant because it can also be specified as `y~x1 = y~x2 & y~x1 > 0`. bain will work correctly if hypotheses specified using only `<` and `>` are redundant. bain will return an error message if hypotheses specified using at least one `=` are redundant.

*Each hypothesis in a set of hypotheses has to be possible.* An hypothesis is impossible if estimates in agreement with the hypothesis do not exist. For example: values for `y~x1` in agreement with `y~x1 = 0 & y~x1 > 2` do not exist. It is the responsibility of the user to ensure that the hypotheses specified are possible. If not, bain will either return an error message or render an output table containing Inf's.

### Results obtained after running bain Structural Equation Modelling

- To be able to properly interpret the results of a bain Linear Regression, you are required to read the turorials by Hoijtink, Mulder, van Lissa, and Gu (2019) and van Lissa et al. (2020) that can be retrieved from the Psychological Methods website and Structual Equation Modeling website, respectively, or from the bain website at https://informative-hypotheses.sites.uu.nl/software/bain/.
- If you want to understand the technical background of bain you should read Gu, Mulder, and Hoijtink (2018) and Hoijtink, Gu, and Mulder (2019) that can be retrieved from the British Journal of Mathematical and Statistical Psychology website or from the bain website at https://informativehypotheses.sites.uu.nl/software/bain/.
- The following results are obtained after running a bain Structural Equation Modeling:
1. The table in which the Bayes factor of each hypothesis specified versus the  unconstrained hypothesis and its complement (that is, not the hypothesis), respectively, is presented. This table contains three sets of posterior model probabilities (each) based on equal prior model probabilities: PMPa, the posterior model probabilities of the hypotheses specified; PMPb, hypotheses specified plus Hu, the unconstrained hypothesis; and, PMPc, hypothesis specified plus Hc, that is, the joint complement of all hypotheses specified, that is, "not the hypotheses specified".
2. The Bayes factor matrix in which the mutual Bayes factors of the hypotheses specified in the Model
Constraints box are presented.
3. A coefficients table containing the estimates of the regression coefficients, their standard error (se) and
95% credible interval.
4. A plot of the pmp's (excluding and including the unconstrained hypothesis) visually highlighting the
support in the data for each hypothesis entertained.
5. A visual representation of your model in the form of a path diagram.

### References

- Gu, X., Mulder, J., and Hoijtink, H. (2018). Approximate adjusted fractional Bayes factors: A general method for testing informative hypotheses. British Journal of Mathematical and Statistical Psychology, 71, 229-261. DOI: 10.1111/bmsp.12110
- Hoijtink, H., Mulder, J., van Lissa, C., and Gu, X. (2019). A tutorial on testing hypotheses using the Bayes factor. Psychological Methods, 24, 539-556. DOI: 10.1037/met0000201 
- Hoijtink, H., Gu, X., and Mulder, J. (2019). Bayesian evaluation of informative hypotheses for multiple populations. British Journal of Mathematical and Statistical Psychology, 72, 219-243. DOI: 10.1111/bmsp.12145
- Van Lissa, C., Gu, X., Mulder, J., Rosseel, Y., van Zundert, C., and Hoijtink, H. (2020). Evaluating Informative Hypotheses Using the Bayes Factor in Structural Equation Models. Structural Equation Modelling. https://doi.org/10.1080/10705511.2020.1745644
