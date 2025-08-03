"""Create demo submission

Usage: python -m create_demo_submission
"""


import os

import pandas as pd
from sklearn.tree import DecisionTreeClassifier


INPUT_DIR = 'data/'  # should contain the three datasets
OUTPUT_DIR = 'submissions/Marco_Heyden_uxxxx_123/'  # emulate ILIAS' submission folder name


if __name__ == '__main__':
    # Load data
    X_train = pd.read_csv(INPUT_DIR + 'train_features.csv')
    y_train = pd.read_csv(INPUT_DIR + 'train_target.csv', squeeze=True)
    X_test = pd.read_csv(INPUT_DIR + 'test_features.csv')

    # Prediction pipeline
    model = DecisionTreeClassifier(random_state=25)
    model.fit(X=X_train, y=y_train)
    y_test_pred = pd.Series(data=model.predict(X=X_test), name='verification_result')

    # Save output
    os.makedirs(name=OUTPUT_DIR, exist_ok=True)
    y_test_pred.to_csv(OUTPUT_DIR + 'Marco_Heyden_prediction.csv', index=False)
