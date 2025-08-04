SELECT
    pt.patientunitstayid,
    pt.age,
    pt.apacheadmissiondx,
    CASE 
        WHEN pt.gender = 'Male' THEN 1 
        WHEN pt.gender = 'Female' THEN 2 
        ELSE NULL 
    END AS gender,
    ROUND(pt.unitdischargeoffset / 60.0) AS icu_los_hours,
    ROUND((-pt.hospitaladmitoffset) / 60.0) AS hosp_to_icu_admit_hours
FROM patient pt
ORDER BY pt.patienthealthsystemstayid;
