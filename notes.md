## Checklist

1.  [x] Data loading via connection with SQLite3 file.

Tables in the SQLite database:

admissiondrug \| admissiondx \| allergy \| apacheapsvar \| apachepatientresult \| apachepredvar \| careplancareprovider \| careplaneol \| careplangeneral \| careplangoal \| careplaninfectiousdisease\| customlab \| diagnosis \| hospital \| infusiondrug \| intakeoutput \| lab \| medication \| microlab \| note \| nurseassessment \| nursecare \| nursecharting \| pasthistory \| patient \| physicalexam \| respiratorycare \| respiratorycharting \| treatment \| vitalaperiodic \| vitalperiodic

2.  [x] Cohort Selection (Inclusion/Exclusion criteria):

    -   Exclude patients: (1) under 18 years old, (2) missing the target variable (mortality indicator (alive/dead)), (3) with repeated ICU admissions (keep the first admission only), (4) with more than 80% of personal data was missing, & (5) with ICU stays shorter than 4 hours.
    -   **NOTE:** exclusion criterion 4 has **not** been implemented yet.

3.  Feature selection -\> table selection

    -   How do I pick the right features for my model? (Table selection follows feature selection.)
        -   According to Table S1 in (2), choose [these](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0262895#pone.0262895.s001) tables.
    -   Which of the vendor submodule *scripts* do I need for my tables?
    -   Which of the vendor submodule *engineered features* do I need for my tables?
    -   Address the question of missingness - at what %age of missingness do I drop a feature? Should I make exceptions for critical variables?

4.  Model of choice:\
    Logistic regression + XGBoost + SHAP

5.  

## Aside

1.  I am building a model that mimics early decision-making, not retrospective diagnosis. Hence, **use first day data only.** (Find sources that supplement why this is important.)

2.  There is a difference between my own model and ICU scoring systems (including APACHE IV/IVa).

3.  

    (2) "The baseline models were divided into non-gradient boosting baseline models, gradient boosting ensemble models, and illness severity scoring systems."

## Questions

1.  [x] Should I divide patients into disease groups?

Given that I will probably have 1700 entries or less, no point in doing this. Keep the general cohort as is.

2.  How do I use an ML-based approach without losing model interpretability?

    (Why is this impoprtant? (2) "Without any doubt, for clinical applications, model interpretability is a significant boon due to the complexity of the phenomena being analyzed and the potential repercussions of wrong decisions.")

3.  What is a good model?

4.  

5.  "It is essential to develop mortality prediction models that are both accurate and interpretable. Model interpretability can either be pursued by developing intrinsically interpretable models (e.g., Logistic Regression and Decision Tree) or by using interpretable surrogate models as post-hoc explanation tools (e.g., SHapley Additive exPlanations (SHAP) and Local Interpretable Model-Agnostic Explanations (LIME)) to explain the black box models."
