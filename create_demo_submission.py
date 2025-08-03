"""Create demo submission

Usage:
    1. create and activate a python 3.12 virtual environment
        the python version is not that important for this script, but let's stick to the best practices
    2. install the requirements from requirements.txt
    3. run with: python -m create_demo_submission

If you need help, you can refer to the Python_Setup.pdf file, which describes how to set up your virtual enviroments.
This file is from the DS1 WS23/24 exercises, you can find it in the corresponding ILIAS page.
"""

import pandas as pd

from pathlib import Path
from sklearn.tree import DecisionTreeClassifier

BASE_DIR = Path("")  # your project's directory
INPUT_DIR = BASE_DIR / "data"  # should contain the three datasets
OUTPUT_DIR = BASE_DIR / Path("submissions")


if __name__ == '__main__':
    # Load data
    X_train = pd.read_csv(INPUT_DIR / "train_features.csv")
    y_train = pd.read_csv(INPUT_DIR / "train_target.csv")
    X_test = pd.read_csv(INPUT_DIR / "test_features.csv")

    # Prediction pipeline
    model = DecisionTreeClassifier(random_state=25)
    model.fit(X=X_train, y=y_train)
    y_test_pred = pd.Series(data=model.predict(X=X_test), name='verification_result')

    # Save output
    OUTPUT_DIR.mkdir(exist_ok=True)
    y_test_pred.to_csv(OUTPUT_DIR / 'Matteucci_Federico_prediction.csv', index=False)
