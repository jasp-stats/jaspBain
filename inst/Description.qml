import QtQuick
import JASP.Module

Description
{
	title			: qsTr("Bain")
	description		: qsTr("A module for computing approximated adjusted fractional Bayes factors for equality, inequality, and about equality constrained hypotheses.")
	icon			: "bain-module"	
	hasWrappers		: true

	GroupTitle
	{
		title: 		"T-Tests"
		icon: 		"analysis-bain-ttest.svg"
	}
	Analysis
	{
		menu:		"Welch's T-Test"
		title:		"Bain Welch's T-Test"
		func:		"BainTTestBayesianIndependentSamples"
	}
	Analysis
	{
		menu: 		"Paired Samples T-Test"
		title:		"Bain Paired Samples T-Test"
		func:		"BainTTestBayesianPairedSamples"
	}
	Analysis
	{
		menu:  		"One Sample T-Test"
		title:		"Bain One Sample T-Test"
		func:		"BainTTestBayesianOneSample"
	}

	GroupTitle
	{
		title: 		"ANOVA"
		icon: 		"analysis-bain-anova.svg"
	}	
	Analysis
	{
		menu:   	"ANOVA"
		title:		"Bain ANOVA"
		func:		"BainAnovaBayesian"
	}
	Analysis
	{
		menu:   	"ANCOVA"
		title:		"Bain ANCOVA"
		func:		"BainAncovaBayesian"
	}

	GroupTitle
	{
		title: 		"Regression"
		icon: 		"analysis-bain-regression.svg"
	}	
	Analysis
	{
		menu:   	"Linear Regression"
		title:		"Regression"
		func:		"BainRegressionLinearBayesian"
	}
	Analysis
	{
		menu:   	"Structural Equation Modeling"
		title:		"Bain Structural Equation Modeling"
		func:		"BainSemBayesian"
		preloadData: false
	}
}
