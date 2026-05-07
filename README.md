# An Empirical Investigation of Temporal Instability in Variable Importance Under Varying Correlation Structures in Survival Analysis

This repository contains the R scripts, datasets, and analysis workflow used to investigate the temporal instability of time-dependent Variable Importance (VI) in survival analysis models under different correlation structures.

The study focuses on understanding how the correlation structure of datasets influences the temporal behavior of variable importance curves, the stability of model explanations over time, and the predictive performance of survival models.

The analyses were conducted using healthcare survival datasets with varying correlation structures, censoring rates, and variable characteristics.

The following survival analysis models were used in this study:

- Cox Proportional Hazards (Cox)
- Random Survival Forest (RSF)
- Conditional Inference Forest (CForest)
- Component-Based Gradient Boosting (BlackBoost)
- Extreme Gradient Boosting (XGBoost)

---

# Objective of the Study

Time-dependent VI curves provide information about how the importance of variables changes over time. However, direct interpretation of these curves can be difficult due to temporal fluctuations and instability patterns.

To address this issue, two summary measures were used in this study:

- Area Under the VI Curve
- Temporal Instability Metric

This approach allows simultaneous evaluation of the overall effect of variables across time and the temporal fluctuation behavior of explanations.

In addition, the relationship between dataset correlation structures and explanation stability was investigated. For this purpose, correlation-related measures were calculated and compared with both model performance and VI instability results.

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

- `install_packages.R` → Installs and loads the required R packages.
- `import_datasets.R` → Imports all datasets used in the study.
- `prepare_data.R` → Performs preprocessing and data cleaning steps.
- `task.R` → Creates survival analysis task objects.
- `data_split.R` → Splits datasets into training and test sets.
- `modeling.R` → Trains survival analysis models.
- `performance.R` → Calculates predictive performance metrics.
- `correlation_metrics.R` → Computes dataset correlation structure measures.
- `global_explain.R` → Computes time-dependent VI explanations.
- `vimp_analysis.R` → Performs area and instability analyses on VI curves.
- `plots.R` → Generates plots and visualizations.
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

---

# Contribution of the Study

This study emphasizes that predictive performance alone is insufficient for evaluating survival models. Even models with similar predictive performance may produce substantially different explanation stability patterns.

The proposed framework contributes to survival explainability research by jointly evaluating predictive performance, temporal explanation stability, and correlation structure effects under a unified analysis framework.
