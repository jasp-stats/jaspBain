//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

Form
{

	VariablesForm
	{

		AvailableVariablesList
		{
			name: 						"variablesList"
		}

		AssignedVariablesList
		{
			name: 						"dependent"
			title: 						qsTr("Dependent Variable")
			singleVariable: 			true
			allowedColumns: 			["scale"]
		}

		AssignedVariablesList
		{
			name: 						"fixedFactors"
			title: 						qsTr("Fixed Factors")
			singleVariable: 			true
			allowedColumns: 			["ordinal", "nominal"]
		}

		AssignedVariablesList
		{
			name: 						"covariates"
			title: 						qsTr("Covariates")
			allowedColumns: 			["ordinal", "nominal", "scale"]
		}
	}

	Group
	{
		title: 							qsTr("Tables")

		CheckBox
		{
			name: 						"bayesFactorMatrix"
			text: 						qsTr("Bayes factor matrix")
		}

		CheckBox
		{
			name: 						"descriptives"
			text: 						qsTr("Coefficients")

			CIField
			{
				name: 					"credibleInterval"
				text: 					qsTr("Credible interval")
			}
		}
	}

	Group
	{
		title: 							qsTr("Plots")

		CheckBox
		{
			name: 						"bayesFactorPlot"
			text: 						qsTr("Posterior probabilities")
		}

		CheckBox
		{
			name: 						"descriptivesPlot"
			text: 						qsTr("Adjusted means")
		}
	}

	Group
	{
		title: 							qsTr("Additional Options")

		DoubleField
		{
			name: 						"seed"
			text: 						qsTr("Seed")
			defaultValue: 				100
			min: 						-999999
			max: 						999999
			fieldWidth: 				60 * preferencesModel.uiScale
		}

		DoubleField
		{
			name: 						"fraction"
			text: 						qsTr("Fraction")
			defaultValue: 				1
			min: 						0.01
			max: 						100
			fieldWidth: 				60 * preferencesModel.uiScale
		}

		CheckBox
		{
			name: 						"standardized"
			checked:					false
			visible:					false
		}
	}

	Section
	{
		text: 							qsTr("Model Constraints")
		columns: 						1

		Text
		{
			text: 						qsTr("Place each hypothesis on a new line. For example:\n\nfactorLow = factorMed = factorHigh\nfactorLow < factorMed < factorHigh\n\nwhere factor is the factor name and Low/Med/High are the factor level names.\nRead the help file for further instructions.")
		}

		TextArea
		{
			name: 						"model"
			text: 						""
			textType: 					JASP.TextTypeModel
			trim: 						true
			implicitHeight:				200 * preferencesModel.uiScale
		}
	}
}
