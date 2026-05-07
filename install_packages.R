install.packages(c(
  "readr", "dplyr", "tidyr",
  "survival", "mlr3", "mlr3proba", "mlr3extralearners",
  "randomForestSRC", "janitor", "ggplot2", "cowplot",
  "survex", "DALEX", "remotes", "devtools",
  "patchwork", "plotly", "ggrepel",
  "partykit", "coin", "mboost", "xgboost", "mlr3pipelines"
))

library(readr)
library(dplyr)
library(tidyr)
library(survival)
library(mlr3)
library(mlr3pipelines)
library(randomForestSRC)
library(janitor)
library(ggplot2)
library(cowplot)
library(survex)
library(DALEX)
library(remotes)
library(devtools)
library(patchwork)
library(plotly)
library(ggrepel)
library(partykit)
library(coin)
library(mboost)
library(xgboost)

remotes::install_github("mlr-org/mlr3proba", force = TRUE)
remotes::install_github("mlr-org/mlr3extralearners", force = TRUE)

library(mlr3proba)
library(mlr3extralearners)

install.packages("survAUC")
library(survAUC)
