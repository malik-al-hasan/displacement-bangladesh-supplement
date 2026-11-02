"""
04b_cv_interactions.py
Where does the random forest's advantage over the pooled logistic model come from?

Compares, under the same repeated stratified 10-fold cross-validation:
  (1) unpenalised additive logistic regression
  (2) ridge-penalised additive logistic regression (penalty tuned by inner CV)
  (3) ridge logistic with every predictor interacted with district
  (4) random forest
If (3) closes most of the gap between (2) and (4), the structure the additive
model misses is mainly district heterogeneity rather than generic non-linearity.
"""
import warnings
from pathlib import Path

import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestClassifier
from sklearn.linear_model import LogisticRegression, LogisticRegressionCV
from sklearn.metrics import brier_score_loss, roc_auc_score
from sklearn.model_selection import RepeatedStratifiedKFold
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler

from importlib import import_module

ml = import_module("04_ml_validation")           # reuse the design-matrix builder
warnings.filterwarnings("ignore", category=FutureWarning)
ROOT = Path(__file__).resolve().parents[1]
SEED = ml.SEED

d = pd.read_csv(ROOT / "outputs" / "v4" / "analysis_data_v4_primary.csv")
y = d["displaced"].to_numpy()
X, groups = ml.design(d, list(ml.LABELS))
base_cols = [c for c in X.columns if not c.startswith("district=")]
Xi = X.copy()
for c in base_cols:                                # district x predictor interactions
    Xi[f"Kurigram*{c}"] = X["district=Kurigram"] * X[c]

ridge = lambda: make_pipeline(StandardScaler(), LogisticRegressionCV(
    Cs=np.logspace(-3, 2, 20), cv=5, scoring="roc_auc", max_iter=5000))
models = {
    "Logistic, additive (unpenalised)": (X, lambda: make_pipeline(StandardScaler(), LogisticRegression(C=np.inf, max_iter=5000))),
    "Logistic, additive (ridge)": (X, ridge),
    "Logistic, district interactions (ridge)": (Xi, ridge),
    "Random forest": (X, lambda: RandomForestClassifier(n_estimators=1000, min_samples_leaf=5,
                                                        max_features="sqrt", n_jobs=-1, random_state=SEED)),
}

n_rep, n_fold = 10, 10
rskf = RepeatedStratifiedKFold(n_splits=n_fold, n_repeats=n_rep, random_state=SEED)
oof = {k: np.zeros((n_rep, len(y))) for k in models}
for i, (tr, te) in enumerate(rskf.split(X, y)):
    r = i // n_fold
    for name, (M, make) in models.items():
        m = make().fit(M.iloc[tr], y[tr])
        oof[name][r, te] = m.predict_proba(M.iloc[te])[:, 1]

rows = []
for name, P in oof.items():
    a = [roc_auc_score(y, P[r]) for r in range(n_rep)]
    b = [brier_score_loss(y, P[r]) for r in range(n_rep)]
    rows.append({"model": name, "cv_auc_mean": np.mean(a), "cv_auc_sd": np.std(a, ddof=1),
                 "cv_brier_mean": np.mean(b), "cv_brier_sd": np.std(b, ddof=1)})
out = pd.DataFrame(rows)
out.to_csv(ROOT / "outputs" / "v4" / "ml" / "TableS_cv_interaction_check.csv", index=False)
print(out.round(3).to_string(index=False))
