SELECT
    pt.patientunitstayid,
    pt.patienthealthsystemstayid,
    pt.age,
    pt.apacheadmissiondx,
    CASE 
        WHEN pt.gender = 'Male' THEN 1 
        WHEN pt.gender = 'Female' THEN 2 
        ELSE NULL 
    END AS gender,
    CASE 
        WHEN pt.hospitaldischargestatus = 'Alive' THEN 0
        WHEN pt.hospitaldischargestatus = 'Expired' THEN 1 
        ELSE NULL 
    END AS hosp_mortality,
    ROUND(pt.unitdischargeoffset / 60.0) AS icu_los_hours,
    ROUND((-pt.hospitaladmitoffset) / 60.0) AS hosp_to_icu_admit_hours,
    ROW_NUMBER() OVER (
        PARTITION BY pt.patienthealthsystemstayid 
        ORDER BY pt.hospitaladmitoffset
    ) AS icu_stay_sequence -- identifies first, second ICU stays etc.
FROM patient pt
ORDER BY pt.patienthealthsystemstayid, icu_stay_sequence;
