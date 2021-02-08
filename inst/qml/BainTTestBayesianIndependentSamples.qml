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

import QtQuick							2.8
import QtQuick.Layouts					1.3
import QtQuick.Controls 				2.12
import JASP.Controls					1.0
import JASP								1.0

Form
{
	VariablesForm
	{
		preferredHeight: 				jaspTheme.smallDefaultVariablesFormHeight
		
		AvailableVariablesList 
		{ 
			name: 						"variablesList" 
		}

		AssignedVariablesList
		{
            name: 						"variables"
            title: 						qsTr("Dependent Variables")
            singleVariable: 			false
			allowedColumns: 			["scale"]
        }

		AssignedVariablesList
		{
            name: 						"groupingVariable"
            title: 						qsTr("Grouping Variable")
            singleVariable: 			true
			allowedColumns: 			["nominal", "nominalText"]
        }
    }

	ColumnLayout
	{
		RadioButtonGroup
		{
			name: 						"hypothesis"
			title: 						qsTr("Hypothesis Test")

			RadioButton 
			{
				name: 					"equalNotEqual"
				text: 					qsTr("Equal vs. not equal")
				checked: 				true
			}

			RadioButton 
			{ 
				name: 					"equalBigger" 
				text: 					qsTr("Equal vs. bigger")
			}

			RadioButton 
			{ 
				name: 					"equalSmaller"
				text: 					qsTr("Equal vs. smaller") 
			}

			RadioButton 
			{ 
				name: 					"biggerSmaller"
				text: 					qsTr("Bigger vs. smaller")
			}
			RadioButton 
			{ 
				name: 					"equalBiggerSmaller"
				text: 					qsTr("Equal vs. bigger vs. smaller")
			}
		}

		RadioButtonGroup
		{
			title: 						qsTr("Bayes Factor")
			name: 						"bayesFactorType"

			RadioButton 
			{ 
				name: 					"BF01"
				text: 					qsTr("BF\u2080\u2081: Equal vs. other")
				checked: 				true
			}

			RadioButton 
			{ 
				name: 					"BF10"
				text: 					qsTr("BF\u2081\u2080: Other vs. equal") 
			}
		}

		Group
		{
			title: 						qsTr("Additional Options")

			DoubleField  
			{
				name: 					"seed"
				text: 					qsTr("Seed")
				defaultValue: 			100
				min: 					-999999
				max: 					999999
				fieldWidth: 			60 * preferencesModel.uiScale 
			}

			DoubleField  
			{ 
				name: 					"fraction"
				text: 					qsTr("Fraction")
				defaultValue: 			1
				min: 					0.01
				max: 					100
				fieldWidth: 			60 * preferencesModel.uiScale 
			}
		}
	}

	ColumnLayout
	{
		Group
		{
			title: 						qsTr("Tables")

			CheckBox 
			{ 
				name: 					"descriptives"
				text: 					qsTr("Descriptives")

				CIField 
				{ 
					name: 				"credibleInterval"
					text: 				qsTr("Credible interval")
				}
			}
		}

		Group
		{
			title: 						qsTr("Plots")

			CheckBox 
			{ 
				name: 					"bayesFactorPlot"
				text: 					qsTr("Posterior probabilities") 
			}

			CheckBox 
			{ 
				name: 					"descriptivesPlot"
				text: 					qsTr("Descriptives plots") 
			}
		}
	}
}
