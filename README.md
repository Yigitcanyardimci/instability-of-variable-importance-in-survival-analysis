# An Empirical Investigation of Temporal Instability in Variable Importance Under Varying Correlation Structures in Survival Analysis

This repository contains the R scripts, datasets, and analysis workflow used to investigate the temporal instability of time-dependent Variable Importance (VI) in survival analysis models under varying correlation structures.

The study focuses on analyzing how the correlation structure of datasets influences the temporal instability of variable importance explanations and the predictive performance of survival models.

The analyses were conducted using 36 healthcare survival datasets with different correlation structures, censoring rates, and variable characteristics.

The following survival analysis models were used in this study:

- Cox Proportional Hazards (Cox)
- Random Survival Forest (RSF)
- Conditional Inference Forest (CForest)
- Component-Based Gradient Boosting (BlackBoost)
- Extreme Gradient Boosting (XGBoost)

---

# Objective of the Study

Time-dependent VI curves provide information about how variable importance changes over time. However, temporal fluctuations in these curves make interpretation difficult and reduce comparability across datasets and models.

To address this limitation, two complementary quantitative measures were used in this study:

- Area Under the VI Curve
- Temporal Instability Metric

The area under the curve summarizes the overall contribution of variables across time, while the instability metric quantifies temporal fluctuations in variable importance explanations.

This framework enables simultaneous evaluation of:

- Overall variable importance across time
- Temporal stability of explanations
- Relationship between correlation structure and explanation instability

In addition, correlation-related measures were computed to investigate whether dataset correlation structures influence both temporal instability and predictive performance.

---

# Repository Structure

```text
├── datasets
│   ├── veteran.csv
│   ├── breast.csv
│   ├── heart.csv
│   ├── melanoma.csv
│   ├── pbc.csv
│   └── ...
│
├── scripts
│   ├── install_packages.R
│   ├── import_datasets.R
│   ├── prepare_data.R
│   ├── task.R
│   ├── data_split.R
│   ├── modeling.R
│   ├── performance.R
│   ├── correlation_metrics.R
│   ├── global_explain.R
│   ├── vimp_analysis.R
│   ├── plots.R
│   └── analyses.R
│
├── results
│   ├── tables
│   ├── figures
│   └── metrics
│
└── run.R
```

---

# File Descriptions

## `datasets/`

Contains the healthcare survival datasets used in the analyses.

## `scripts/`

Contains all R scripts used throughout the analysis pipeline.

- `install_packages.R` → Installs and loads required R packages.
- `import_datasets.R` → Imports datasets used in the study.
- `prepare_data.R` → Performs preprocessing and data cleaning procedures.
- `task.R` → Creates survival analysis task objects.
- `data_split.R` → Splits datasets into training and test sets.
- `modeling.R` → Trains survival analysis models.
- `performance.R` → Calculates predictive performance metrics.
- `correlation_metrics.R` → Computes correlation structure measures for datasets.
- `global_explain.R` → Computes time-dependent VI explanations.
- `vimp_analysis.R` → Performs VI area and instability analyses.
- `plots.R` → Generates figures and visualizations.
- `analyses.R` → Combines all analysis steps and produces final outputs.

## `results/`

Contains generated tables, figures, and performance metrics obtained from the analyses.

## `run.R`

Main script used to execute the complete analysis pipeline automatically.

When executed, this file sequentially performs:

- Data preparation
- Model training
- Performance evaluation
- Correlation analysis
- Time-dependent VI analysis
- Instability calculations
- Visualization generation

---

# Performance Metrics

The survival models were evaluated using the following performance metrics:

- Concordance Index (C-index)
- Brier Score

In addition, the temporal behavior of variable importance explanations was evaluated using:

- VI Curve Area
- Temporal Instability Metric

Correlation-related measures were also computed to investigate the relationship between dataset correlation structures and temporal instability.

---

# Contribution of the Study

This study demonstrates that predictive performance alone is insufficient for evaluating survival models. Models with similar predictive performance may produce substantially different levels of explanation stability.

The findings show that dataset correlation structure is significantly associated with the temporal instability of variable importance explanations, while no significant relationship was observed between correlation structure and predictive performance.

The proposed framework contributes to survival explainability research by jointly evaluating predictive performance, temporal explanation stability, and correlation structure effects within a unified analysis framework.
