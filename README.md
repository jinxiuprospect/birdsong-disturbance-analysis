# Birdsong Disturbance Analysis

## Project Overview
This project analyzes birdsong data collected in three urban greenspaces in Portland, Oregon. The main goal is to understand how recreational user groups—particularly humans and dogs—affect birdsong activity, and to provide insights for urban park management.

Key tasks include:

- Exploratory Data Analysis (EDA)  
- Statistical inference on the effects of human and dog presence  
- Logistic regression and generalized linear mixed-effects modeling (GLMM)  
- Species-specific analysis, with a focus on Song Sparrows  
- Interpretation of hazard ratios and effect sizes  
- Visualization of birdsong activity and user group interactions  
- Recommendations for urban park management based on findings  

---

## Data Description

- **Source:** Field observations from urban parks in Portland, Oregon  
- **Parks:** Oaks Bottom, Sellwood Park, Smith & Bybee Wetlands  
- **File:** `Birdsong.csv`  
- **Observations:** 381 structured observations collected during Fall 2024  
- **Variables:**  
  - **Response variables:** Singing presence/cessation (binary), chirping presence/cessation (binary), indicator species status (Song Sparrows)  
  - **Predictors:**  
    - Site location (Oaks, Sellwood, Smith & Bybee)  
    - Human group size, noise level, speed of movement  
    - Dog presence, leash status, dog noise level  
    - Trail behavior (on/off path), observation duration  
    - Focal bird species
