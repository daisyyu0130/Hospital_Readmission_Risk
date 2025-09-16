# Physician Financial Incentives to Reduce Unplanned Hospital Readmissions: A Propensity Score Weighted Cohort Study

📌 Overview
This repository contains R code and analysis scripts for the study:
“Physician Financial Incentives to Reduce Unplanned Hospital Readmissions: A Propensity Score Weighted Cohort Study.”

The project evaluates the impact of physician financial incentives on 30-day unplanned hospital readmission rates using linked administrative health data and propensity score weighting methods.

📊 Data Sources
This study uses de-identified patient-level data from:

- DAD (Discharge Abstract Database)
- NARCS (National Ambulatory Care Reporting System)
- MSP (Medical Services Plan)
- PharmaNet

⚠️ Note: Raw data are not included in this repository due to privacy and data-sharing agreements.

⚙️ Methods
1. Data import
2. Data linkage (DAD, NARCS, MSP)
3. Extract Table A variables (e.g., most responsible diagnosis, medical history, medications)
4. Create Table A (patient characteristics)
5. Define response variable
6. Apply propensity score weighting
7. calculate intra-class correlation
8. Perform backward selection
9. Conduct primary and secondary analyses
10. Conduct subgroup analyses
11. Create Table E (sensitivity analysis)
12. Create Table B (physician characteristics)
13. Conduct cost analysis
14. Sensitivity analysis: discharge to long-term care
15. Sensitivity analysis: episode of care
16. Cite relevant R packages
