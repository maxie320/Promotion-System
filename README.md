# Promotion-System
Analytics Wizard: WNS Hack 
Code Describes how the promotion system in a demo multi national company with 9 sub verticals can make use of this model.
This model aids to provide results for people who could potentially be promoted, and reduce the onboarding time for promotion.
Code describes various parameters that are used to predict the promotion of an employee.
Variables are chosen via stepAIC procedure
The ideal model with lowest AIC value is our target model
We target this model and cross check the variables in terms of significance levels with p values.
We Check the multicollinearity of the variables with VIF values to identify highly correlated variables.
Once the model is decided we predict the model on in-sample and outer-sample test data.
We calculate the confusion matrix which in turn helps us identify the F1 score of the model.
Outer sample testing is measured by the mean of the predicted and actual outcome.
F1 value : 98.54, Outer Sample mean correctness value : 89.93
Model Scoring as per Analytics Vidhya : 0.38
