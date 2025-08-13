# ICU Mortality Risk Prediction Model

<p align="center">
  <a href="https://adeebatak11.github.io/icu_mortality/">
    <img src="https://img.shields.io/badge/_Clinician_Insights_Dashboard-Open_Interactive_App-2ea44f?style=for-the-badge&logoColor=white" 
         alt="Clinician Insights Dashboard" 
         height="60">
  </a>
</p>

<p align="center" style="color:red; font-weight:bold;">
  Disclaimer: Not for clinical use — educational purposes only.
</p>

A prognostic predictive modeling project developing an ICU mortality risk prediction model using the eICU Collaborative Research Database. This project compares traditional logistic regression with LASSO penalized regression to predict in-hospital mortality for ICU patients. The final model prioritizes interpretability over algorithmic complexity.

### Project Structure

```         
icu_mortality/
├── scripts/                    # Analysis workflow
│   ├── 01_data_load_and_connect.R    
│   ├── 02_clean_data.R               
│   ├── 03_feature_selection.R        
│   ├── 04_run_glm_model.R            
│   ├── 05_run_lasso_model.R         
│   ├── 06_compare_models.R           
│   ├── 07_validate_model.R           
│   ├── Appendix.R                    
│   └── sql/                          
├── data/                    # Data                    
│   └── README.md        
├── docs/                                          
│   ├── annotated_bibliography.md     
│   ├── images/                       
└── renv/                       
```

### Running the Analysis

1.  **Clone and setup:**

    ``` r
    # Restore package environment
    renv::restore()
    ```

2.  **Execute analysis pipeline:**

    ``` r
    # Run scripts in order:
    source("scripts/01_data_load_and_connect.R")
    source("scripts/02_clean_data.R")
    source("scripts/03_feature_selection.R")
    source("scripts/04_run_glm_model.R")
    source("scripts/05_run_lasso_model.R")
    source("scripts/06_compare_models.R")
    source("scripts/07_validate_model.R")
    ```

### References

This project follows established clinical prediction modeling guidelines and draws from key literature in ICU mortality prediction. See `docs/annotated_bibliography.md` for detailed references.

### License

This project uses the eICU Collaborative Research Database under the PhysioNet Credentialed Health Data License 1.5.0. The analysis code is available for research and educational purposes.
