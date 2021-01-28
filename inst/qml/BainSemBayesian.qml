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
	columns: 							2

	TextArea 
	{ 
		name: 							"syntax"
		text:							""
		textType: 						JASP.TextTypeLavaan 
		Layout.columnSpan: 				2
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
			name: 						"pathDiagram"
			text: 						qsTr("Path diagram")	

			CheckBox
			{
				name:					"pathDiagramEstimates"
				text:					qsTr("Show parameter estimates")
			}	

			CheckBox
			{
				name:					"pathDiagramLegend"
				text:					qsTr("Show legend")
			}		
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
			min: 						1
			max: 						100
			fieldWidth: 				60 * preferencesModel.uiScale 
		}

		CheckBox 
		{ 
			name: 						"standardized"
			text: 						qsTr("Standardize")	
		}
	}

	Section
	{
		title:							qsTr("Multigroup SEM")

		VariablesForm
		{
			implicitHeight:				150 * preferencesModel.uiScale

			AvailableVariablesList 
			{
				name: 					"variablesList"
			}

			AssignedVariablesList 
			{
				name: 					"fixedFactors"
				title: 					qsTr("Grouping variable")
				singleVariable: 		true
				allowedColumns: 		["ordinal", "nominal", "nominalText"]
			}
		}
	}

	Section
	{
		text: 							qsTr("Model Constraints")
		columns:						1

		Text 
		{ 
			text: 						qsTr("Hypotheses can be formulated using the parameter names that appear if the 'Coefficients' box is ticked.\nDo NOT use spaces in the parameter names. Place each hypothesis on a new line. For example:\n\nA=~x1 = A=~x2 = A=~x3 = A=~x4\nA=~x1 > A=~x2 > A=~x3 = A=~x4\nA=~x1 > 0 & A=~x2 > 0 & A=~x3 > 0 & A=~x4 > 0\n\nRead the help file for further instructions about formulating hypotheses and what can and cannot be done\nwith Bain Structural Equation Modeling.")
		}

		TextArea
		{
				name: 					"model"
				text: 					""
				textType: 				JASP.TextTypeModel
				trim: 					true
		}
	}
}
