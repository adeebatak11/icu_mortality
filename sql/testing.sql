SELECT uniquepid, COUNT(*)
FROM patient
WHERE unitadmittime24 = (
    SELECT MIN(unitadmittime24)
    FROM patient p2
    WHERE p2.uniquepid = patient.uniquepid
)
GROUP BY uniquepid
HAVING COUNT(*) > 1