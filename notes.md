## Checklist

1.  [x] Data loading via connection with SQLite3 file.

Tables in the SQLite database:

admissiondrug \| admissiondx \| allergy \| apacheapsvar \| ***apachepatientresult*** \| ***apachepredvar*** \| careplancareprovider \| careplaneol \| careplangeneral \| careplangoal \| careplaninfectiousdisease\| customlab \| diagnosis \| hospital \| infusiondrug \| intakeoutput \| lab \| medication \| microlab \| note \| nurseassessment \| nursecare \| nursecharting \| pasthistory \| ***patient*** \| physicalexam \| respiratorycare \| respiratorycharting \| treatment \| vitalaperiodic \| vitalperiodic

Tables I am using for developing the model:\
[apachepredvar]{.underline}

2.  [x] Cohort Selection (Inclusion/Exclusion criteria):

    -   Exclude patients: (1) under 18 years old, (2) missing the target variable (mortality indicator (alive/dead)), (3) with repeated ICU admissions (keep the first admission only), (4) with more than 80% of personal data was missing, & (5) with ICU stays shorter than 4 hours.
    -   **NOTE:** exclusion criterion 4 has **not** been implemented yet.
    -   **NOTE**: "if a patient was admitted multiple times, each admission (that is, each unique stay) was included as a separate row in the dataset."
    -   Inclusion criteria for patient admissions was such that each admission record of predicted and actual length of stay and mortality must contain real values.

3.  Data Cleaning:

    -   <div>

        9.  "To handle the **sparsity** of the data, we imputed the missing values using the modal value along its respective axis. We applied this method of imputation with the assumption that the missing data elements are missing at random [8]."

        </div>

4.  Feature selection -\> table selection

    -   keep patientunitstayid as that is the identifying variable.

    -   using these apache IV prediction variables:\
        use these: graftcount, age, admitdx, thrombolytics, aids, hepaticfailure, age, admitdiagnosis, thrombolytics, aids, hepaticfailure, lymphoma, metastaticcancer, leukemia, immunosuppression, cirrhosis, electivesurgery, readmit, midur, diabetes, amilocation

    -   should there be an interaction between: amilocation, midur, thrombolytics?

    -   drop aids as no one in the cohort has AIDS.

    -   The right number of featrues to be selected further follows how many observation we have! so read paper #6 for this.

        1.  as per criteria 1, 1599

        2.  as per criteria 2, 3221

        3.  as per criteria 3 (the available sample size can precisely estimate the overall risk in the population by key time-points of interest.

    -   Ended up with these:\
        icumortality, aps, age, gender, admitdx_grouped, hepaticfailure, metastaticcancer, leukemia, immunosuppression, cirrhosis, diabetes, thrombolytics, ima, readmit, hosp_to_icu_admit_hours\
        \
        Variables were dropped for various reasons during model collaboration & best practices(?).

        -   lymphoma, midur: Variables were too sparse

        -   admitdx_grouped: admit diagnosis were placed in broader categories through (from data-driven & frequency analysis(?) went from 220 categories to 11. best practices as per TRIPOD as well.

    -   Right before feedback from running first logistic model:\
        sample size = 1718, no. of events = 79, no. of predictors & predictor parameters = 14 & 23.

    -   <div>

        7.  Focus on **clinically-based model prespecification** and use data reduction (unsupervised learning) if the sample size does not allow you to use all the clinically pre-specified variables as single predictors.

        </div>

    -   How do I pick the right features for my model? (Table selection follows feature selection.)

        -   According to Table S1 in (2), choose [these](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0262895#pone.0262895.s001) tables.

    -   Which of the vendor submodule *scripts* do I need for my tables?

    -   Which of the vendor submodule *engineered features* do I need for my tables?

    -   (11): APS variables: "APACHE II, III and IV are based on the APS or acute physiology score (which uses 12 physiologic values), age, and chronic health status within one of 56 disease groups."

    -   

        (11) APACHE prediction variables: "Provides variables underlying the APACHE predictions."

    -   

        (11) APACHE IVa score: "APS score + age points + chronic health points

    -   which one of these is better to use: p_apache_vars(apacheadmissiondx), p_apache_vars(admitdiagnosis)

5.  Address the questions of:

    -   Missingness -\> at what %age of missingness do I drop a feature? Should I make exceptions for critical variables?

    -   Multicollinearity

    -   Outliers

    2.  Model of choice: Prognosis Prediction Model

        (1) Penalized logistic regression (2) Logistic regression + XGBoost + SHAP
            -   I chose a LASSO penalized logistic regression with tuning parameter with the lowest cross-validated error (\\lambda = lambda.min = 0.006078998).
            -   LASSO is a tool to find the best subset of variables for risk stratification and prediction—not classic statistical significance, but actual, practical “who’s at high risk” classification.
            -   The optimal cutoff for predicted ICU mortality risk was determined using the threshold that maximized the Youden Index on the ROC curve (threshold = 0.052). other thresholds were tried as well: 0.01, 0.025, 0.10, 0.20 but Youden's statistic generated the best prediction (wrt sensitivity and specififcity and cautioning on the side of err)
            -   because of bad PPV (16.39%), risk tiers rather than just binary conclusions were evalauted.
            -   limitation: My model has no external validation: Prediction model development studies without external validation aim to develop a prognostic or diagnostic prediction model from the dataset at hand: the development set. Such studies commonly aim to identify important predictors for the outcome under study, assign mutually adjusted weights per predictor in a multivariable analysis, develop a final prediction model, and quantify the predictive performance (e.g., discrimination, calibration, classification) of that model in the development set. As model overfitting may occur, particularly in small datasets, development studies ideally include internal validation using some form of data re-sampling techniques, such as bootstrapping, jack-knife, or cross-validation, to quantify any optimism in the predictive performance of the developed model.
            -   Calibration plots are often supplemented by a formal statistical test, the Hosmer-Lemeshow test for logistic regression.
            -   “While both models achieved nearly identical discrimination (AUC), the LASSO model automatically dropped statistically uninformative predictors, reducing the risk of overfitting and making the model more interpretable and robust.”: LASSO gives parsimony and interpretability, with no loss in predictive power.
            -   Despite class imbalance, the unweighted LASSO model stratifies patients effectively: High-risk group had a 41.4% observed mortality, compared to 1.5% in the Low-risk group. This risk gradient demonstrates strong clinical utility even with modest PR AUC.”
        (2) Bootstrapping: Bootstrapping provides robust internal validation for predictive models, especially when sample size is limited.
            -   “The GLM model achieved a mean AUC of 0.878, with a trimmed (5th–95th percentile) confidence interval of 0.844–0.905 based on 1,000 bootstrap resamples.

                The LASSO model achieved a mean AUC of 0.855, with a trimmed 90% confidence interval of 0.828–0.894.

                Because of class imbalance and limited outcome events (n = 79), some bootstrap resamples yielded unstable AUC estimates for the LASSO model. To present a more representative range of model performance, we report a trimmed AUC confidence interval. This provides a clearer picture of typical model discrimination while acknowledging variability.

            -   Risk Group Performance

                Using bootstrap resampling (1,000 iterations), we evaluated the model’s ability to stratify patients into clinically meaningful risk groups based on predicted mortality probabilities:

                	•	Low Risk (\<10%): 2.3% mortality (95% CI: 1.4%–3.3%)

                	•	Moderate Risk (10–30%): 19.6% mortality (95% CI: 9.7%–30.0%)

                	•	High Risk (\>30%): 51.3% mortality (95% CI: 3.5%–80.0%)

                These results demonstrate strong separation between risk groups. The wide confidence interval for the high-risk group reflects instability in bootstrap samples where few patients were classified as high-risk. In 26 of 1,000 resamples, this group was empty and excluded from calculations. Despite this, the trend across risk strata remained consistent, with increasing predicted risk corresponding to observed mortality.

            -   “Bootstrapped calibration analysis revealed a median intercept of 0.49 and a median slope of 1.21, suggesting that the model systematically under-predicted risk and produced narrower-than-ideal probability distributions. While calibration was generally consistent across bootstrap resamples, 27 iterations failed due to separation or lack of variability in predictions, reflecting occasional instability in model fitting.”

        -   How do you minimize bias?

        -   How well does your model discriminate?

        -   Is your model calibrated?

        -   #1 helps with sparse predictors

        -   

        1.  unclear or biased validation of model performance

        2.  difference in whether data-driven variable selection was performed (yes/no) before applying LR and ML algorithms,

        3.  difference in handling of continuous variables before applying LR and ML algorithms,

        4.  different predictors considered for LR and ML algorithms,

        5.  whether corrections for imbalanced outcomes where used only for LR or only for ML algorithms\

6.  Make something like (15) to culminate project.

## Aside

1.  I am building a model that mimics early decision-making, not retrospective diagnosis. Hence, **use first day data only.** (Find sources that supplement why this is important.)

2.  There is a difference between my own model and ICU scoring systems (including APACHE IV/IVa).

    "The baseline models were divided into non-gradient boosting baseline models, gradient boosting ensemble models, and illness severity scoring systems."

3.  

    (7) Selection of variables should not be done using statistical methods on the same data you intend to use to develop your prediction model unless your sample size is huge. Consider it 'double dipping".

4.  Sparsity of data

5.  Model Calibration

6.  Model Discrimination

7.  Parsimonious model

    ### APACHE: Acute Physiology and Critical Health Evaluation IV & IVa

    1.  The three tables from the eICU database used in the study were Acute Physiology Score (APS) variables table, APACHE prediction variables table, and the APACHE patient results table. The APS variables and the prediction variables tables contain the inputs used for calculating the overall APACHE scores and prediction values. The patient results table contains the resulting predictions as well as actual patient outcomes.
    2.  Type I Error: APACHE incorrectly predicting in-hospital death: False positive.\
        Type II Error: APACHE incorrectly predicting in-hospital survival: False negative. [More serious]
    3.  
        (9) our concern about performance stems from the utilization of a regressor trained on binary outcomes to output continuous value estimates.

## Questions

1.  [x] Should I divide patients into disease groups?

Given that I will probably have 1700 entries or less, no point in doing this. Keep the general cohort as is.

2.  How do I use an ML-based approach without losing model interpretability?

    (Why is this impoprtant? (2) "Without any doubt, for clinical applications, model interpretability is a significant boon due to the complexity of the phenomena being analyzed and the potential repercussions of wrong decisions.")

3.  What is a good model?

4.  

5.  "It is essential to develop mortality prediction models that are both accurate and interpretable. Model interpretability can either be pursued by developing intrinsically interpretable models (e.g., Logistic Regression and Decision Tree) or by using interpretable surrogate models as post-hoc explanation tools (e.g., SHapley Additive exPlanations (SHAP) and Local Interpretable Model-Agnostic Explanations (LIME)) to explain the black box models."

1-page EDA summary (tables + plots)

```         
•   Model performance table: (AUC, sensitivity, specificity at chosen cutoffs)

•   Calibration plot (and stats)

•   Odds ratio table with CIs for predictors

•   Short discussion of clinical implications and limitations
```

I built a live early-warning score with MIMIC/​eICU vitals, REST endpoint, color-coded dashboard, and an FDA-ready validation plan.”
