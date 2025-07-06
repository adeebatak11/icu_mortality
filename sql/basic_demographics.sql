SELECT pt.patientunitstayid, pt.uniquepid, pt.age, pt.apacheadmissiondx,
       CASE WHEN pt.gender = 'Male' THEN 1
            WHEN pt.gender = 'Female' THEN 2
            ELSE NULL END AS gender,
       CASE WHEN pt.hospitaldischargestatus = 'Alive' THEN 0
            WHEN pt.hospitaldischargestatus = 'Expired' THEN 1
            ELSE NULL END AS hosp_mortality,
       ROUND(pt.unitdischargeoffset/60) AS icu_los_hours
FROM patient pt
JOIN (
    SELECT uniquepid,
           MIN(unitadmittime24) AS first_admit,
           MIN(patientunitstayid) AS first_stay_id
    FROM patient
    GROUP BY uniquepid
) first_stay
  ON pt.uniquepid = first_stay.uniquepid
 AND pt.unitadmittime24 = first_stay.first_admit
 AND pt.patientunitstayid = first_stay.first_stay_id
ORDER BY pt.patientunitstayid;
