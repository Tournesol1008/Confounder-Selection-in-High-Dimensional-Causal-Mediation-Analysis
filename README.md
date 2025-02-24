# High-Dimensional Confounding in Causal Mediation: A Comparison Study

This repository contains the code and data used in the following paper:

> **Chen, M., Nguyen, T. T., & Liu, J. (2024).**  
> *High-dimensional confounding in causal mediation: a comparison study of double machine learning and regularized partial correlation network.*  
> *bioRxiv*, 2024-10.

The code is organized into four folders, each corresponding to a major step in the analysis pipeline. By following the numbered order, you can reproduce the primary results and figures from the paper.

---

## Repository Structure

1. **01_Partial_Correlation_Network_Fitting**  
   - **Purpose:** Fits baseline dataset partial correlation networks for covariate selection.  

2. **02_DML_w_NetworkSelection**  
   - **Purpose:** Implements Double Machine Learning (DML) using covariates selected by the partial correlation network.  

3. **03_DML_wo_NetworkSelection**  
   - **Purpose:** Runs DML without using network-selected confounders (e.g., using all covariates) for comparison.  

4. **04_Mediation_w_NetworkSelection**  
   - **Purpose:** Performs causal mediation analysis using DML, incorporating the network-selected covariates.  

---

For questions or contributions, please reach out to talliechen108@gmail.com