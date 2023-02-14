# House-sales-price
House sales price

Report summary:

This report describes the basic steps of carrying out the analysis done on the Home sales prices dataset. One has a Train on which the evaluation of different models will have to be performed, with which one will then return the predictions of the variable Price for the Test set at hand. The metric of interest that will be attempted to minimize is the Mean Absolute Error (MAE).
It was decided to split the Train set to also obtain a Validation set on which to go to evaluate the best model.

An exploratory analysis was conducted aimed at identifying possible problems in the variables, then the different problems were solved through feature engineering, which is intended to
then allow the models to be able to use the most informative and well-specified covariates possible.
In the modeling part, several models were trained including Linear Regression, Random Forest and XGBoost, then evaluated on the Validation set left aside before modeling, with the purpose of getting the metrics as general, and therefore comparable, as possible.

An ensemble of the different methods was then tried with the aim of minimizing the metric of interest. Finally, the best model obtained on the complete Train set was reestimated and P rice predictions were returned for the Test set.
