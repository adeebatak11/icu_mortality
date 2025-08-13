# Annotated Bibliography: ICU Mortality Prediction Model Development


This bibliography informed the development of an ICU mortality prediction model using the eICU Collaborative Research Database. The resulting model achieved clinically meaningful risk stratification while maintaining interpretability and following established reporting guidelines.


**1. Huang T, Le D, Yuan L, Xu S, Peng X (2023)** Machine learning for prediction of in-hospital mortality in lung cancer patients admitted to intensive care unit. *PLoS ONE* 18(1): e0280606. https://doi.org/10.1371/journal.pone.0280606

*Used for:* **Cohort selection criteria and exclusion standards**
- Applied their exclusion criteria: patients <18 years, missing mortality data, ICU stays <4 hours
- Informed feature selection with laboratory values (albumin, creatinine, BUN, bilirubin)

**2. Pollard TJ, Johnson AEW, Raffa JD, Celi LA, Mark RG, Badawi O (2018)** The eICU Collaborative Research Database, a freely available multi-center database for critical care research. *Scientific Data* 5:180178. https://doi.org/10.1038/sdata.2018.178

*Used for:* **Database documentation and citation**
- Primary citation for eICU-CRD dataset
- Referenced for database structure and table relationships
- Informed ethical considerations for de-identified data use

**3. Christodoulou E, Ma J, Collins GS, Steyerberg EW, Verbakel JY, Van Calster B (2019)** A systematic review shows no performance benefit of machine learning over logistic regression for clinical prediction models. *Journal of Clinical Epidemiology* 110:12-22. https://doi.org/10.1016/j.jclinepi.2018.10.003

*Used for:* **Model selection justification**
- Supported decision to use penalized logistic regression (LASSO) over complex ML algorithms
- Informed comparative analysis between GLM and LASSO approaches
- Justified focus on interpretability over algorithmic complexity

**4. Collins GS, Reitsma JB, Altman DG, Moons KG (2015)** Transparent reporting of a multivariable prediction model for individual prognosis or diagnosis (TRIPOD): the TRIPOD statement. *Circulation* 131(2):211-219. https://doi.org/10.1161/CIRCULATIONAHA.114.014508

*Used for:* **Reporting standards and manuscript structure**
- Guided model development and validation reporting
- Informed calibration assessment methodology
- Structured results presentation and performance metrics

**5. Zimmerman JE, Kramer AA, McNair DS, Malila FM (2006)** Acute Physiology and Chronic Health Evaluation (APACHE) IV: hospital mortality assessment for today's critically ill patients. *Critical Care Medicine* 34(5):1297-1310. https://doi.org/10.1097/01.CCM.0000215112.84523.bA

*Used for:* **APACHE IV understanding and baseline comparison**
- Referenced for cohort restriction to APACHE IVa patients only
- Informed feature selection from apachePredVar table
- Used as benchmark for model performance comparison

**6. Hand DJ (2006)** Classifier Technology and the Illusion of Progress. *Statistical Science* 21(1):1-14. https://doi.org/10.1214/088342306000000060

*Used for:* **Model evaluation philosophy**
- Informed focus on clinical utility over pure algorithmic performance
- Guided interpretation of AUC limitations in clinical context