1.  [x] Data loading via connection with SQLite3 file.

Tables in the SQLite database:

admissiondrug \| admissiondx \| allergy \| apacheapsvar \| apachepatientresult \| apachepredvar \| careplancareprovider \| careplaneol \| careplangeneral \| careplangoal \| careplaninfectiousdisease\| customlab \| diagnosis \| hospital \| infusiondrug \| intakeoutput \| lab \| medication \| microlab \| note \| nurseassessment \| nursecare \| nursecharting \| pasthistory \| patient \| physicalexam \| respiratorycare \| respiratorycharting \| treatment \| vitalaperiodic \| vitalperiodic

2.  Cohort Selection (Inclusion/Exclusion criteria):

    -   Exclude patients: (1) under 18 years old, (2) missing the target variable (mortality indicator (alive/dead)), (3) with repeated ICU admissions, (4) with more than 80% of personal data was missing, & (5) with ICU stays shorter than 4 hours.

3.  I am building a model that mimics early decision-making, not retrospective diagnosis. Hence, **use first day data only.** (Find sources that supplement why this is important.)

4.  Feature selection -\> table selection

    -   How do I pick the right features for my model? (Table selection follows feature selection.)
        -   Find scientific literature to guide.
    -   Which of the vendor submodule scripts do I need for my tables?
    -   Which of the vendor submodule engineered features do I need for my tables?

5.  Model of choice:\
    Logistic regression + XGBoost + SHAP

6.  
