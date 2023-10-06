import numpy as np
import pandas as pd
import missingno as msno
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import cross_val_score, KFold
from sklearn.pipeline import make_pipeline

import matplotlib.pyplot as plt

# Set paths
dataPath = '/Users/djw/Documents/pCloud_synced/Academics/Projects/2020_thesis/thesis_experiments/3_experiments/3_1_gap_measure/3_1_2_processed_data/ml_data/'

# Load data
df = pd.read_csv(dataPath + 'goalDimensions_outcomes.csv')

# Specify outcome columns
outcomes = ['flourishing_score',
            'harmony_score',
            'qol_score',
            'sat_life_score',
            'sub_happy_score',
            'DASS_overall',
            'perceived_stress_score',
            'rosenberg_SES_score']

# Separate features and target
X = df[['success', 'time', 'import', 'effort']]
y = df[outcomes[0]]

# Split the data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=42)

# Define a pipeline. We standardize the data first, then apply linear regression
pipeline = make_pipeline(StandardScaler(), LinearRegression())

# Define a cross-validation strategy
kfold = KFold(n_splits=5, shuffle=True, random_state=42)

# Use the cross_val_score function to perform cross-validation
cv_results = cross_val_score(pipeline, X_train, y_train, cv=kfold, scoring='neg_mean_squared_error')

# Since sklearn's cross_val_score function returns negative values for the scores,
# we negate them to get the actual mean squared errors
mse_scores = -cv_results

print(f'Mean cross-validated MSE: {mse_scores.mean()}')
print(f'Standard deviation of cross-validated MSE: {mse_scores.std()}')
