## Checklist

1.  [x] Data loading via connection with SQLite3 file.

Tables in the SQLite database:

admissiondrug \| admissiondx \| allergy \| apacheapsvar \| apachepatientresult \| apachepredvar \| careplancareprovider \| careplaneol \| careplangeneral \| careplangoal \| careplaninfectiousdisease\| customlab \| diagnosis \| hospital \| infusiondrug \| intakeoutput \| lab \| medication \| microlab \| note \| nurseassessment \| nursecare \| nursecharting \| pasthistory \| patient \| physicalexam \| respiratorycare \| respiratorycharting \| treatment \| vitalaperiodic \| vitalperiodic

2.  [x] Cohort Selection (Inclusion/Exclusion criteria):

    -   Exclude patients: (1) under 18 years old, (2) missing the target variable (mortality indicator (alive/dead)), (3) with repeated ICU admissions (keep the first admission only), (4) with more than 80% of personal data was missing, & (5) with ICU stays shorter than 4 hours.
    -   **NOTE:** exclusion criterion 4 has **not** been implemented yet.

3.  Feature selection -\> table selection

    -   

        (7) Focus on **clinically-based model prespecification** and use data reduction (unsupervised learning) it the sample size does not allow you to use all the clinically pre-specified variables as single predictors.

    -   How do I pick the right features for my model? (Table selection follows feature selection.)

        -   According to Table S1 in (2), choose [these](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0262895#pone.0262895.s001) tables.

    -   Which of the vendor submodule *scripts* do I need for my tables?

    -   Which of the vendor submodule *engineered features* do I need for my tables?

    -   

4.  Address the questions of:

    -   Missingness - at what %age of missingness do I drop a feature? Should I make exceptions for critical variables?

    -   Multicollinearity

    -   Outliers

5.  Model of choice: (1) Penalized logistic regression (2) Logistic regression + XGBoost + SHAP

    How do you minimize bias?\
    From each article, we defined five signaling items to indicate potential bias. We elaborate on these items in Table A.2:

    (1) unclear or biased validation of model performance,

    (2) difference in whether data-driven variable selection was performed (yes/no) before applying LR and ML algorithms,

    (3) difference in handling of continuous variables before applying LR and ML algorithms,

    (4) different predictors considered for LR and ML algorithms,

    (5) whether corrections for imbalanced outcomes where used only for LR or only for ML algorithms\

6.  

## Aside

1.  I am building a model that mimics early decision-making, not retrospective diagnosis. Hence, **use first day data only.** (Find sources that supplement why this is important.)

2.  There is a difference between my own model and ICU scoring systems (including APACHE IV/IVa).

    "The baseline models were divided into non-gradient boosting baseline models, gradient boosting ensemble models, and illness severity scoring systems."

3.  

    (7) Selection of variables should not be done using statistical methods on the same data you intend to use to develop your prediction model unless your sample size is huge. Consider it 'double dipping".

## Questions

1.  [x] Should I divide patients into disease groups?

Given that I will probably have 1700 entries or less, no point in doing this. Keep the general cohort as is.

2.  How do I use an ML-based approach without losing model interpretability?

    (Why is this impoprtant? (2) "Without any doubt, for clinical applications, model interpretability is a significant boon due to the complexity of the phenomena being analyzed and the potential repercussions of wrong decisions.")

3.  What is a good model?

4.  

5.  "It is essential to develop mortality prediction models that are both accurate and interpretable. Model interpretability can either be pursued by developing intrinsically interpretable models (e.g., Logistic Regression and Decision Tree) or by using interpretable surrogate models as post-hoc explanation tools (e.g., SHapley Additive exPlanations (SHAP) and Local Interpretable Model-Agnostic Explanations (LIME)) to explain the black box models."
