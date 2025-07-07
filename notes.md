## Checklist

1.  [x] Data loading via connection with SQLite3 file.

Tables in the SQLite database:

admissiondrug \| admissiondx \| allergy \| apacheapsvar \| ***apachepatientresult*** \| ***apachepredvar*** \| careplancareprovider \| careplaneol \| careplangeneral \| careplangoal \| careplaninfectiousdisease\| customlab \| diagnosis \| hospital \| infusiondrug \| intakeoutput \| lab \| medication \| microlab \| note \| nurseassessment \| nursecare \| nursecharting \| pasthistory \| ***patient*** \| physicalexam \| respiratorycare \| respiratorycharting \| treatment \| vitalaperiodic \| vitalperiodic

Tables I am using for developing the model:\
[apachepredvar]{.underline}

2.  [ ] Cohort Selection (Inclusion/Exclusion criteria):

    -   Exclude patients: (1) under 18 years old, (2) missing the target variable (mortality indicator (alive/dead)), (3) with repeated ICU admissions (keep the first admission only), (4) with more than 80% of personal data was missing, & (5) with ICU stays shorter than 4 hours.
    -   **NOTE:** exclusion criterion 4 has **not** been implemented yet.
    -   **NOTE**: "if a patient was admitted multiple times, each admission (that is, each unique stay) was included as a separate row in the dataset."
    -   Inclusion criteria for patient admissions was such that each admission record of predicted and actual length of stay and mortality must contain real values.

3.  Data Cleaning:

    -   

        (9) "To handle the **sparsity** of the data, we imputed the missing values using the modal value along its respective axis. We applied this method of imputation with the assumption that the missing data elements are missing at random [8]."

4.  Feature selection -\> table selection

    -   

        (7) Focus on **clinically-based model prespecification** and use data reduction (unsupervised learning) if the sample size does not allow you to use all the clinically pre-specified variables as single predictors.

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

    2.  Model of choice: (1) Penalized logistic regression (2) Logistic regression + XGBoost + SHAP

        -   How do you minimize bias?

        -   How well does your model discriminate?

        -   Is your model calibrated?

        -   From each article, we defined five signaling items to indicate potential bias. We elaborate on these items in Table A.2:

        1.  unclear or biased validation of model performance

            -   

                (4) It should be clear that models are developed using training data only.

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
