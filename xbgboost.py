import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler, OneHotEncoder
from sklearn.model_selection import StratifiedKFold
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score, confusion_matrix
from xgboost import XGBClassifier
from skopt import BayesSearchCV
import joblib

# Step 1: Data Loading
df = pd.read_csv('balanced_hiphop_corpus.csv')

# Rename columns to lowercase for consistency
df.columns = df.columns.str.lower()

# Step 2: Data Cleaning
# Drop identifier columns
identifiers = ['Track URI', 'Track Name', 'Album Name', 'Artist_Name', 'Release Date', 'Added By', 'Added At', 'Genres', 'Record Label']
df = df.drop(columns=identifiers, errors='ignore')

# Drop other irrelevant columns if any (e.g., Duration (ms), Popularity, Explicit)
df = df.drop(columns=['Duration (ms)', 'Popularity', 'Explicit'], errors='ignore')

# Handle missing values (drop rows with NaN for simplicity)
df = df.dropna()

# Step 3: Feature Engineering
df['energy_danceability'] = df['energy'] * df['danceability']
df['valence_energy'] = df['valence'] * df['energy']
df['mood_score'] = (df['valence'] + df['energy']) / 2

# Step 4: Preprocessing
# Encode target
df['region'] = df['region'].map({'east': 0, 'west': 1})

# Separate features and target
X = df.drop('region', axis=1)
y = df['region']

# Identify continuous features for scaling
continuous_features = ['danceability', 'energy', 'loudness', 'speechiness', 'acousticness', 'instrumentalness', 'liveness', 'valence', 'tempo', 'energy_danceability', 'valence_energy', 'mood_score']

# One-hot encode categorical features
categorical_features = ['key', 'mode', 'time signature']
encoder = OneHotEncoder(sparse_output=False, drop='first')  # drop first to avoid multicollinearity
encoded_cats = encoder.fit_transform(X[categorical_features])
encoded_cat_df = pd.DataFrame(encoded_cats, columns=encoder.get_feature_names_out(categorical_features))

# Combine continuous and encoded categorical
X_continuous = X[continuous_features]
X_processed = pd.concat([X_continuous.reset_index(drop=True), encoded_cat_df.reset_index(drop=True)], axis=1)

# Scale continuous features
scaler = StandardScaler()
X_scaled = X_processed.copy()
X_scaled[continuous_features] = scaler.fit_transform(X_processed[continuous_features])

# Step 5: Model Selection and Hyperparameter Tuning with Bayesian Search
xgb = XGBClassifier(objective='binary:logistic', random_state=42)

# Define search space
search_spaces = {
    'max_depth': (3, 10),
    'subsample': (0.7, 0.9),
    'colsample_bytree': (0.7, 0.9),
    'reg_alpha': (0.1, 1.0),
    'reg_lambda': (1.0, 10.0),
    'learning_rate': (0.01, 0.1),
    'n_estimators': (100, 500)
}

# Stratified K-Fold
skf = StratifiedKFold(n_splits=5, shuffle=True, random_state=42)

# Bayesian Search
bayes_search = BayesSearchCV(
    estimator=xgb,
    search_spaces=search_spaces,
    n_iter=50,  # Number of iterations
    cv=skf,
    scoring='f1',  # Optimize for F1 score
    random_state=42,
    n_jobs=-1,
    verbose=True
)

bayes_search.fit(X_scaled, y)

# Best parameters
best_params = bayes_search.best_params_
print("Best Parameters:", best_params)

# Step 6: Final Model Training
final_model = XGBClassifier(**best_params, objective='binary:logistic', random_state=42)
final_model.fit(X_scaled, y)

# Save the model
joblib.dump(final_model, 'xgb_hiphop_classifier.pkl')
joblib.dump(scaler, 'scaler.pkl')
joblib.dump(encoder, 'encoder.pkl')
final_model.save_model('xgb_hiphop_model.json')
# Step 7: Evaluation (on the full data for demonstration; in practice, use a holdout set)
y_pred = final_model.predict(X_scaled)

print("Accuracy:", accuracy_score(y, y_pred))
print("Precision:", precision_score(y, y_pred))
print("Recall:", recall_score(y, y_pred))
print("F1 Score:", f1_score(y, y_pred))
print("Confusion Matrix:\n", confusion_matrix(y, y_pred))

# Function for prediction
def predict_region(track_features):
    """
    Predict East Coast (0) or West Coast (1) for a new track.
    track_features: dict with keys for all features (continuous + categorical)
    """
    # Create DataFrame
    input_df = pd.DataFrame([track_features])
    
    # Feature engineering
    input_df['energy_danceability'] = input_df['energy'] * input_df['danceability']
    input_df['valence_energy'] = input_df['valence'] * input_df['energy']
    input_df['mood_score'] = (input_df['valence'] + input_df['energy']) / 2
    
    # Encode categoricals
    encoded_cats = encoder.transform(input_df[categorical_features])
    encoded_cat_df = pd.DataFrame(encoded_cats, columns=encoder.get_feature_names_out(categorical_features))
    
    # Scale continuous
    input_continuous = input_df[continuous_features]
    input_scaled = pd.concat([input_continuous.reset_index(drop=True), encoded_cat_df.reset_index(drop=True)], axis=1)
    input_scaled[continuous_features] = scaler.transform(input_scaled[continuous_features])

    # Predict
    prediction = final_model.predict(input_scaled)[0]
    return 'West Coast' if prediction == 1 else 'East Coast'

