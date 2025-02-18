# Confounder-Selection-in-High-Dimensional-Causal-Mediation-Analysis

# High-Dimensional Confounding in Causal Mediation: A Comparison Study

This repository contains the code and data used in the following paper:

> **Chen, M., Nguyen, T. T., & Liu, J. (2024).**  
> *High-dimensional confounding in causal mediation: a comparison study of double machine learning and regularized partial correlation network.*  
> *bioRxiv*, 2024-10.

The code is organized into five folders, each corresponding to a major step in the analysis pipeline. By following the numbered order, you can reproduce the primary results and figures from the paper.

---

## Repository Structure

1. **01_Partial_Correlation_Network_Fitting**  
   - **Purpose:** Fits high-dimensional partial correlation networks for covariate selection.  
   - **Contents:** Scripts for loading data, running network estimation (e.g., EBICglasso), and exporting the resulting adjacency matrices or selected confounders.

2. **02_Missing_Data_Imputation**  
   - **Purpose:** Performs missing data imputation (e.g., hot-deck or multiple imputation).  
   - **Contents:** Scripts for reading raw data, applying imputation methods, and saving imputed datasets.

3. **03_DML_w_NetworkSelection**  
   - **Purpose:** Implements Double Machine Learning (DML) using covariates selected by the partial correlation network.  
   - **Contents:** Scripts that load imputed data, use network-selected covariates, and run DML to estimate exposure–outcome or exposure–mediator relationships.

4. **04_DML_wo_NetworkSelection**  
   - **Purpose:** Runs DML without using network-selected confounders (e.g., using all covariates) for comparison.  
   - **Contents:** Scripts that illustrate the difference between using all covariates vs. network-selected covariates.

5. **05_Mediation_w_NetworkSelection**  
   - **Purpose:** Performs causal mediation analysis using DML, incorporating the network-selected covariates.  
   - **Contents:** Scripts that estimate mediation effects (natural direct/indirect effects) under high-dimensional confounding.

---

## How to Reproduce

1. **Clone the Repository**

   ```bash
   git clone https://github.com/YourUsername/YourRepoName.git
   cd YourRepoName
