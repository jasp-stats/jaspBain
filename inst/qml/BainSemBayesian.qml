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
import JASP.Controls					1.0
import JASP								1.0

Form
{
	columns: 							2
	
	Text
	{
		text:							qsTr("Lavaan syntax")
	}

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

		Group
		{
			title:						qsTr("Data used for prior")

			DoubleField
			{
				name:					"fraction"
				afterLabel:				qsTr("times the minimal fraction")
				defaultValue:			1
				min:					1
			}
		}
	}

	Section
	{
		text: 							qsTr("Model Constraints")
		columns:						1

		Text 
		{ 
			text: 						qsTr("Place each hypothesis on a new line. For example:\n\nfactorLow = factorMed = factorHigh\nfactorLow < factorMed < factorHigh\n\nwhere factor is the factor name and Low/Med/High are the factor level names.\nRead the help file for further instructions.") 
		}

		TextArea
		{
				name: 					"model"
				text: 					""
				textType: JASP.TextTypeLavaan//textType: 				JASP.TextTypeModel
				//trim: 					true
		}
	}
}
