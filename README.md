# Tutorial for Advanced Demographic Methods (UNIVIE)

Welcome to the **Tutorial for Advanced Demographic Methods** repository! This collection of R scripts and materials accompanies the 2024–2025 offering of the SE 234001 “Advanced Methods of Demographic Analysis” seminar at the University of Vienna (UNIVIE). It is designed to guide Master’s students through both fundamental and advanced demographic techniques in a clear, reproducible, and hands-on fashion.

---

## Course Overview

Each week, you will learn and practice any of the following:

1. **Foundations of R and Data Wrangling**  
   - Basic data structures (vectors, data frames, lists)  
   - Importing and inspecting datasets (`readr`, `readxl`, `here`)  
   - Tidy data principles with **`dplyr`** and **`tidyr`**

2. **Descriptive Demography & Visualization**  
   - Calculating aggregate rates (fertility, mortality)  
   - Building population pyramids with **`ggplot2`**

3. **Age‐Specific Rates & Life Tables**  
   - Computing `mx`, `qx`, `lx`, `dx`, `Lx`, `Tx`, and life expectancy (`ex`)  
   - Using **`demography`** or **`MortalityLaws`**

4. **Lexis Diagrams & APC Analysis**  
   - Constructing Lexis surfaces  
   - Understanding age–period–cohort distinctions

5. **Projection & Forecasting**  
   - Leslie matrix population projections  
   - Lee–Carter mortality forecasting  
   - Stable population theory and Kitagawa decomposition

---

## 📂 Repository Structure

```text
├── scripts/                  # Weekly tutorial scripts
│   ├── 01_Intro_to_R.R
│   ├── 02_Population_Pyramids.R
│   ├── 03_Age_Specific_Rates.R
│   ├── 04_Life_Table_Construction.R
│   ├── 05_Lexis_Diagrams_APC.R
│   └── 06_Parameterization of Mortality (Mortality laws).R
│   └── 07_Projections_and_Forecasting.R
│   └── 08_Fertility Quantum and Tempo.R
│   └── 09_Fertility models and relational models.R
│   └── 10_Stable population theory.R
│   └── 11_Leslie Matrix.R
│   └── 12_Kitagawas Decomposition.R
├── outputs/                  # Example figures and tables
├── exercises/                # Homework assignments
└── README.md                 # This file
