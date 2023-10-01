
This code is associated with the manuscript:
"Public Health-Focused Use of COVID-19 Rapid Antigen and PCR Tests".

The attached datasets were synthetically generated to mimic real data which is unavaialble
due to privacy issues.




The folder contains two .R files, each containing one table:




ctdemo.R contains a dataframe named "ct", which includes the Ct value of patients with an AG test:

"patient_ID":  a unique integer identifying the patient
"ct":   ct-value, an integer
"labname":  lab serial number, 1-5
"cohort":    vaccination status
"AgResult":   AG test result, positive(1) or negative(0)
"AgTakeMinusPcrTake":  time elapsed from PCR sample to Ag sample (days)
"PCRresultMinusPcrTake":  time elapsed from PCR sample to PCR test result (days)
"vsex":  F/M
"age_category": age group 
"AgTakeMinusPcrTakeBeforePOS":    not in use
"AgTakeMinusPcrTakeBeforeNEG":    not in use 
"month"
"epiweek": the two-weeks time interval from Jan 1st, 2022



nonAgdemo.R containes a dataframe named "nonAg", which includes the Ct value of patients without an AG tests.
All columns included here are defined as previously.



Code files:
libraries.R loads the necessary libraries
create_figures.R  plots the figures appearing in the manuscript
create_tables.R   creates the tables

