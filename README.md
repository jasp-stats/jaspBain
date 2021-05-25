# The Bain Module

Compute approximated adjusted fractional Bayes factors for equality, inequality, and about equality constrained hypotheses.

## Core Functionality

The Bain module is an graphical interface around the `R` package `Bain`, which implements approximated adjusted fractional Bayes factors for equality, inequality, and about equality constrained hypotheses. The module offers a variety of standard statistical analyses, in which contraints can be formulated for the relevant parameters under investigation.

## Module Structure

The analyses in the Machine Learning module are structured in JASP in the following way:

```
--- Bain
    -- T-Tests
       - Welch's T-Test
       - Paired Samples T-Test
       - One Sample T-Test
    -- ANOVA
       - ANOVA
       - ANCOVA
    -- Regression
       - Linear Regression
       - Structural Equation Modeling
```