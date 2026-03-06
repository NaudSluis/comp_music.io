from sklearn.preprocessing import StandardScaler
import numpy as np
import pandas as pd

# Read data
X = pd.read_csv('balanced_hiphop_corpus.csv')

# Keep a copy of identifiers so we can report which songs are which
id_cols = ["Track URI", "Track Name", "Artist_Name", "Album Name", "region"]
meta = X[id_cols].copy()

# Separate out region (what you use to split east / west)
region = X["region"].copy()

# Columns you *don't* want to scale (non-numeric / labels / metadata)
cols_to_drop = [
    "Track URI", "Track Name", "Album Name", "Artist_Name",
    "Release Date", "Duration (ms)", "Popularity", "Explicit",
    "Added By", "Added At", "Genres", "Record Label",
    "region"
]

# Drop these as *columns* (axis=1 or columns=...)
X_num = X.drop(columns=cols_to_drop)

# Scale only numeric features (Danceability, Energy, Tempo, etc.)
scaler = StandardScaler()
X_scaled_array = scaler.fit_transform(X_num)

# Put back into a DataFrame for convenience
X_scaled = pd.DataFrame(X_scaled_array, columns=X_num.columns)
X_scaled["region"] = region.values

# Split into east and west (features only)
east_features = X_scaled[X_scaled["region"] == "east"].drop(columns="region")
west_features = X_scaled[X_scaled["region"] == "west"].drop(columns="region")

# Also get the corresponding meta rows so we can map indices -> tracks
east_meta = meta[X_scaled["region"] == "east"].reset_index(drop=True)
west_meta = meta[X_scaled["region"] == "west"].reset_index(drop=True)

# Centroids
east_centroid = east_features.mean(axis=0)
west_centroid = west_features.mean(axis=0)

# Distances to own centroid
east_distances = np.linalg.norm(east_features - east_centroid, axis=1)
west_distances = np.linalg.norm(west_features - west_centroid, axis=1)

# Distances to *other* coast’s centroid
east_to_west_dist = np.linalg.norm(east_features - west_centroid, axis=1)
west_to_east_dist = np.linalg.norm(west_features - east_centroid, axis=1)

# --- East: typical / extreme / distinctive from west ---
most_typical_east_idx = np.argmin(east_distances)
most_extreme_east_idx = np.argmax(east_distances)
most_distinctive_east_from_west_idx = np.argmax(east_to_west_dist)

# --- East: distinctive but still representative ---
# score = distance to other centroid minus distance from own centroid
east_score = east_to_west_dist - east_distances
most_distinctive_representative_east_idx = np.argmax(east_score)

most_typical_east_meta = east_meta.iloc[most_typical_east_idx]
most_extreme_east_meta = east_meta.iloc[most_extreme_east_idx]
most_distinctive_east_from_west_meta = east_meta.iloc[most_distinctive_east_from_west_idx]
most_distinctive_representative_east_meta = east_meta.iloc[most_distinctive_representative_east_idx]

# --- West: typical / extreme / distinctive from east ---
most_typical_west_idx = np.argmin(west_distances)
most_extreme_west_idx = np.argmax(west_distances)
most_distinctive_west_from_east_idx = np.argmax(west_to_east_dist)

# --- West: distinctive but still representative ---
west_score = west_to_east_dist - west_distances
most_distinctive_representative_west_idx = np.argmax(west_score)

most_typical_west_meta = west_meta.iloc[most_typical_west_idx]
most_extreme_west_meta = west_meta.iloc[most_extreme_west_idx]
most_distinctive_west_from_east_meta = west_meta.iloc[most_distinctive_west_from_east_idx]
most_distinctive_representative_west_meta = west_meta.iloc[most_distinctive_representative_west_idx]

# --- Print results ---
print("===================================== East =============================================")
print(f"Most typical east-coast song:\n{most_typical_east_meta}\n")
print(f"Most extreme east-coast song:\n{most_extreme_east_meta}\n")
print(f"East-coast song most distinctive from west-coast songs:\n{most_distinctive_east_from_west_meta}\n")
print(f"East-coast song most distinctive but still representative:\n{most_distinctive_representative_east_meta}\n")

print("===================================== West =============================================")
print(f"Most typical west-coast song:\n{most_typical_west_meta}\n")
print(f"Most extreme west-coast song:\n{most_extreme_west_meta}\n")
print(f"West-coast song most distinctive from east-coast songs:\n{most_distinctive_west_from_east_meta}\n")
print(f"West-coast song most distinctive but still representative:\n{most_distinctive_representative_west_meta}\n")