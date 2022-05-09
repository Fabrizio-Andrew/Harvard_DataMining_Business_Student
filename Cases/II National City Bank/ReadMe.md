# Readme

# EDA
- All EDA work can be found in EDA.R.  (It's fairly messy.)
- notes.txt also contains some of my observations.

# MODELS
3 Models were tested for this problem:

## KNN
Achieved only a 55% accuracy on training data.  So, this model was abandoned.  It can be found in KNN_Model.R

## Decision Tree
Achieved 72% accuracy on test data.  Can be found in Decision_Tree_Model.R.

## Logistic Regression
Achieved 76% accuracy on test data.  Can be ound in Logistic_Regression_Model.R.

Since the Logistic Regression Model is the most accurate, I extended that script to calculate the probability that prospective customers will accept the offer and return the top 100 prospects.

# RESULTS
The Top 100 customers list can be found in top_100_customers.txt.