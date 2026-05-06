# The Gender Gap in Household Tasks and Child Care Over Time
### by Mira Chandriani
#### Wellesley College
#### Data Science Capstone Spring 2026 Final Project

---

## Project Overview
Women continue to bear a disproportionate share of unpaid household labor and child care, despite gender equality progress in the workplace. In this project, I examine how the gender gap in time spent on unpaid household tasks and primary child care changed between 2010 and 2019, specifically within heterosexual two-parent households with at least one child under age 18. 

---

## Raw Data
I analyze ATUS respondent, activity, roster, and CPS datasets from 2010-2019. The raw datasets are too large to include in GitHub but can be found [here]([url](https://www.bls.gov/tus/data.htm)). 

---

## Files
| File | Description |
|------|-------------|
| `FinalProjectDataCleaning.ipynb` | Python code to clean, filter, and merge 40 ATUS datasets |
| `atus_all.csv` | Cleaned dataset produced by running FinalProjectDataCleaning.ipynb code |
| `FinalProjectVisualizations.R` | R code to create 11+ visualizations addressing research question |
| `FinalProjectModels.R` | R code to run 8 multiple linear regressions, evaluate models, generate predictions, and create model visualization |
| `FinalProjectPoster.pdf` | Poster summarizing final project process and findings |

---

## Running Code

1. Run `FinalProjectDataCleaning.ipynb` to produce cleaned, merged dataset for visualizations + analyses
2. Run `FinalProjectVisualizations.R` to produce visualizations
3. Run `FinalProjectModels.R` to run multiple linear regressions

---

## Key Findings

- Gender alone is a meaningful predictor of time spent on household tasks and child care, but the interaction between gender and time is not significant. 
- The gender gaps in time spent on household tasks and child care remained relatively steady throughout the 2010s, but the gaps are negligible. 
- On average, women spend 3.80 more minutes daily on household tasks than men, controlling for covariates. The largest gender gap over time occurs for Asian respondents. 
- On average, women spend 2.26 more minutes daily on child care than men, controlling for covariates. The largest gender gap over time occurs for unemployed respondents. 
- There is a slight narrowing of the gender gap beginning in mid-2018. 
