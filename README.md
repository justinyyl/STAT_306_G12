
<h1 align="center">🩺 Associations of Healthcare Costs and Early Detection with Colorectal Cancer Mortality in Canada</h1>


<h2 align="center">Contributors: Yulin Yuan, Ruohan Sun, Tony Lee</h2>

---

## 📌 Project Overview

This project investigates the associations between healthcare costs, early detection, and mortality outcomes among Canadian colorectal cancer patients. By applying logistic regression and feature selection techniques, we aimed to identify key predictors of mortality and provide insights into the effectiveness of screening programs and resource allocation.

---

## 📊 Dataset

- **Source**: [Colorectal Cancer Global Dataset & Predictions](https://www.students.cs.ubc.ca/~cs-304/resources/javascript-oracle-resources/node-setup.html)
- **Subset**: Canadian patients only  
- **Variables include**:
  - **Response**: Mortality status (Alive/Dead)
  - **Continuous**: Age, Tumor Size, Healthcare Costs
  - **Categorical**: Cancer Stage, Screening History, Early Detection, Obesity, Physical Activity, Insurance Status, etc.

---

## 📈 Methodology

### 🔍 Exploratory Data Analysis (EDA)
- Addressed class imbalance via random under-sampling
- Visualized distributions for continuous and categorical variables

### 🧮 Modeling Approach
- **Model**: Logistic Regression  
- **Feature Selection**: Backward selection using AIC  
- **Assumptions Checked**:
  - Binary response
  - Independence of observations
  - Variance structure aligned with theory
  - Linearity between logit and continuous variables

### 🔬 Statistical Testing
- Conducted Likelihood Ratio Test (Wilks' Theorem) to assess interaction between `Healthcare_Costs` and `Early_Detection`
- Found no significant interaction effect (p = 0.4586)

---

## ✅ Key Findings

- **Significant predictors**:
  - `Cancer Stage (Regional)` (lower mortality risk)
  - `Healthcare Costs` (higher spending associated with lower mortality)
- **Retained for theoretical importance**:
  - `Early Detection` (not statistically significant, but relevant)
- **Family History** and other covariates excluded after backward selection

---

## ⚠️ Limitations

- Observational data limits causal claims
- Broad definitions of some variables (e.g., "Early Detection")
- Missing socioeconomic and regional healthcare system variables

---

## 🔭 Further Research

- Use time-to-event analysis (e.g., Cox regression)
- Include more granular healthcare access and socioeconomic variables
- Explore intervention-based studies to validate causal pathways

---

## 📚 References

- Balkhi, B. et al. (2023). *Colorectal cancer-related resource utilization and healthcare costs in Saudi Arabia.*  
- Collett, D. (2015). *Modelling survival data in medical research.*  
- Canadian Cancer Society. (2024). *Colorectal cancer statistics.*  
- McPhail, S. et al. (2015). *Stage at diagnosis and early mortality from cancer in England.*
