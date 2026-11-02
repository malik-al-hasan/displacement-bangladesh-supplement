"""
04_ml_validation.py
Out-of-sample check on the regression results.

1. Repeated stratified cross-validation (10 folds x 10 repeats) comparing the
   pooled logistic specification with two flexible learners (random forest,
   gradient boosting). If the flexible learners do not discriminate better, the
   additive logistic specification is not missing large non-linearities or
   interactions inside the pooled sample.
2. Grouped permutation importance (all dummy columns of a variable permuted
   together) on held-out folds, estimated in the pooled sample and separately
   within each district.
"""
import warnings
from pathlib import Path

import numpy as np
import pandas as pd
from sklearn.ensemble import GradientBoostingClassifier, RandomForestClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import brier_score_loss, roc_auc_score
from sklearn.model_selection import RepeatedStratifiedKFold
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler

warnings.filterwarnings("ignore", category=FutureWarning)
ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "outputs" / "v4" / "ml"
OUT.mkdir(parents=True, exist_ok=True)
SEED = 20240531

d = pd.read_csv(ROOT / "outputs" / "v4" / "analysis_data_v4_primary.csv")

FACTORS = {
    "gender": ["Male", "Female"],
    "age": ["18-35", "36-65", "65 or older"],
    "edu": ["No Education", "Below Primary", "High school or less", "SSC", "HSC or above"],
    "occ": ["Manual labour", "Student", "Other"],
    "res": ["Up to 5 years", "6-20 years", "Above 20 years"],
    "wdm": ["No", "Not sure", "Yes"],
    "district": ["Barishal", "Kurigram"],
    "imp_rapid": ["No impacts", "Some bad impacts", "Recoverable impacts",
                  "A lot of recoverable impacts", "A lot of irrecoverable impacts"],
    "imp_slow": ["No impacts", "Some bad impacts", "Recoverable impacts",
                 "A lot of recoverable impacts", "A lot of irrecoverable impacts"],
    "support": ["Food assistance", "Immediate Relief", "Micro-credit",
                "Vocational Training", "Others", "None"],
    "provider": ["Local communities", "National governments",
                 "Non-governmental organizations (NGOs)", "Others"],
    "autonomy": ["No", "Yes"],
}
NUMERIC = ["exposure", "rapid", "slow", "neld"]
LABELS = {
    "gender": "Sex", "age": "Age group", "edu": "Education", "occ": "Occupation",
    "res": "Residence duration", "wdm": "Women's decision-making", "district": "District",
    "exposure": "Exposure score", "rapid": "Rapid-onset hazard score",
    "slow": "Slow-onset hazard score", "neld": "NELD score",
    "imp_rapid": "Impacts, rapid-onset", "imp_slow": "Impacts, slow-onset",
    "support": "Support type", "provider": "Support provider", "autonomy": "Autonomy over support",
}


def design(df, variables):
    """Treatment-coded design matrix plus a map from variable to its columns."""
    cols, groups = [], {}
    for v in variables:
        if v in FACTORS:
            block = []
            for lev in FACTORS[v][1:]:
                name = f"{v}={lev}"
                cols.append(pd.Series((df[v] == lev).astype(float), name=name))
                block.append(name)
            groups[v] = block
        else:
            cols.append(df[v].astype(float).rename(v))
            groups[v] = [v]
    X = pd.concat(cols, axis=1)
    X = X.loc[:, X.std() > 0]                         # drop constant columns in a stratum
    groups = {k: [c for c in v if c in X.columns] for k, v in groups.items()}
    return X, {k: v for k, v in groups.items() if v}


def learners():
    return {
        "Logistic regression": make_pipeline(StandardScaler(),
                                             LogisticRegression(C=np.inf, max_iter=5000)),
        "Random forest": RandomForestClassifier(n_estimators=1000, min_samples_leaf=5,
                                                max_features="sqrt", n_jobs=-1,
                                                random_state=SEED),
        "Gradient boosting": GradientBoostingClassifier(n_estimators=300, learning_rate=0.05,
                                                        max_depth=2, subsample=0.8,
                                                        random_state=SEED),
    }


def cross_validate(X, y, n_splits=10, n_repeats=10):
    """Out-of-fold AUC and Brier score for each repeat (pooled across folds)."""
    rskf = RepeatedStratifiedKFold(n_splits=n_splits, n_repeats=n_repeats, random_state=SEED)
    oof = {name: np.zeros((n_repeats, len(y))) for name in learners()}
    for i, (tr, te) in enumerate(rskf.split(X, y)):
        r = i // n_splits
        for name, model in learners().items():
            model.fit(X.iloc[tr], y[tr])
            oof[name][r, te] = model.predict_proba(X.iloc[te])[:, 1]
    rows = []
    for name, P in oof.items():
        aucs = [roc_auc_score(y, P[r]) for r in range(n_repeats)]
        briers = [brier_score_loss(y, P[r]) for r in range(n_repeats)]
        rows.append({"model": name, "cv_auc_mean": np.mean(aucs), "cv_auc_sd": np.std(aucs, ddof=1),
                     "cv_auc_min": np.min(aucs), "cv_auc_max": np.max(aucs),
                     "cv_brier_mean": np.mean(briers), "cv_brier_sd": np.std(briers, ddof=1)})
    return pd.DataFrame(rows)


def grouped_permutation_importance(X, y, groups, n_splits=5, n_repeats=5, n_perm=5):
    """Mean drop in held-out AUC when all columns of a variable are permuted jointly."""
    rng = np.random.default_rng(SEED)
    rskf = RepeatedStratifiedKFold(n_splits=n_splits, n_repeats=n_repeats, random_state=SEED)
    drops = {g: [] for g in groups}
    for tr, te in rskf.split(X, y):
        model = RandomForestClassifier(n_estimators=500, min_samples_leaf=5, max_features="sqrt",
                                       n_jobs=-1, random_state=SEED).fit(X.iloc[tr], y[tr])
        Xte, yte = X.iloc[te].copy(), y[te]
        base = roc_auc_score(yte, model.predict_proba(Xte)[:, 1])
        for g, cols in groups.items():
            d_g = []
            for _ in range(n_perm):
                Xp = Xte.copy()
                idx = rng.permutation(len(Xp))
                Xp[cols] = Xte[cols].values[idx]
                d_g.append(base - roc_auc_score(yte, model.predict_proba(Xp)[:, 1]))
            drops[g].append(np.mean(d_g))
    return pd.DataFrame({"variable": list(drops),
                         "label": [LABELS[g] for g in drops],
                         "importance_auc_drop": [np.mean(v) for v in drops.values()],
                         "sd_across_folds": [np.std(v, ddof=1) for v in drops.values()]})


if __name__ == "__main__":
    variables = list(LABELS)
    y = d["displaced"].to_numpy()

    # 1. pooled cross-validation
    X, groups = design(d, variables)
    cv = cross_validate(X, y)
    cv.insert(0, "sample", "Pooled")
    cv_rows = [cv]
    for k in ["Barishal", "Kurigram"]:
        s = d[d["district"] == k].reset_index(drop=True)
        Xk, _ = design(s, [v for v in variables if v != "district"])
        cvk = cross_validate(Xk, s["displaced"].to_numpy(), n_splits=5)
        cvk.insert(0, "sample", k)
        cv_rows.append(cvk)
    cv_all = pd.concat(cv_rows, ignore_index=True)
    cv_all.to_csv(OUT / "TableS_ml_crossvalidation.csv", index=False)
    print(cv_all.round(3).to_string(index=False))

    # 2. grouped permutation importance, pooled and by district
    imp = grouped_permutation_importance(X, y, groups)
    imp.insert(0, "sample", "Pooled")
    imps = [imp]
    for k in ["Barishal", "Kurigram"]:
        s = d[d["district"] == k].reset_index(drop=True)
        Xk, gk = design(s, [v for v in variables if v != "district"])
        ik = grouped_permutation_importance(Xk, s["displaced"].to_numpy(), gk)
        ik.insert(0, "sample", k)
        imps.append(ik)
    imp_all = pd.concat(imps, ignore_index=True)
    imp_all.to_csv(OUT / "FigData_permutation_importance.csv", index=False)
    print(imp_all.sort_values(["sample", "importance_auc_drop"], ascending=[True, False])
          .round(4).to_string(index=False))
