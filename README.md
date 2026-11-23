# Credit Risk Prediction With Statistical Learning
## Comparing New Methods Using Italian Financial Reports

Master's Thesis in Finance and Risk Management (University of Parma).

Thesis supervisor: Prof. Fabrizio Laurini.

## Purpose
This project compares four classification methods for estimating the 
Probability of Default (PD) of Italian companies:

- Logistic Regression with Lasso (L1) penalty
- Linear Discriminant Analysis (LDA)
- Quadratic Discriminant Analysis (QDA)
- Classification Trees

These models are estimated on three cross-sections, 2016, 2020 (COVID-19 pandemic)
and 2023 (post-pandemic and inflation surge), to compare their behaviour under
different economic conditions.

The inputs are raw balance-sheet and income statement items rather than 
financial ratios: the latter are just ratios of random variables that could
result in new random variables, which could violate model assumptions and 
complicate data transformations.

## Data
Source: AIDA (Bureau van Dijk), accessed through the University of Parma
licence. **The data are not included in this repo** due to licence restrictions.

Sample selection:

- limited companies (listed and unlisted); partnerships, cooperatives and 
consortia excluded.
- companies located in Northern Italy
- agriculture, banks, insurance companies and public administration excluded
- legal status: active (solvent), active (insolvent), bankrupt, ceased (bankruptcy);
companies in liquidation excluded since liquidation is a voluntary procedure.
- abridged financial statements used as common reference to pick variables;
values in thousands of euros.

|Year | Companies| Defaults| Default rate|
|:----|---------:|--------:|------------:|
|2016 |    89,235|    2,423|       0.0272|
|2020 |   104,680|      704|       0.0067|
|2023 |   126,111|      130|       0.0010|


**Default encoding:** 1 if AIDA legal status is active (insolvent), bankrupt,
or ceased (bankruptcy); 0 if active (solvent). Legal status as reported by AIDA 
at download, May 2026.

**Predictors:** 16 balance-sheet and income-statement items selected with 
`caret::findCorrelation` (cutoff = 0.8, estimated on 2016 data and applied to 
all years), plus sector dummies built from 2-digits NACE REV.2 codes.

## Repository structure








