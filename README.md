# Clinical-Adverse-Events-of-CTCAE-SOC-PT-
Clinical programming project for generating a CTCAE Grade 3+ adverse event table using ADaM datasets.

This project demonstrates clinical programming for generating a clinical safety table of CTCAE Grade 3 or higher adverse events by System Organ Class (SOC) and Preferred Term (PT) using both SAS and R.



The analysis uses ADaM-style datasets and follows a structured clinical programming workflow from data preparation through table generation.

Analysis

The program:
Reads ADAE and ADSL datasets
Merges subject-level treatment and safety information
Defines the adverse event analysis window
Identifies CTCAE Grade 3 or higher events
Restricts the analysis to the Safety Analysis Set
Deduplicates subjects by SOC and Preferred Term
Calculates treatment-specific subject counts
Derives safety population denominators
Calculates percentages
Produces a reporting-ready clinical table
Generates a PDF output using PROC REPORT

Programming Techniques Demonstrated
DATA step programming
PROC SORT
PROC SQL
PROC TRANSPOSE
PROC REPORT
SAS date processing
Character-to-numeric conversion
Subject-level deduplication
Treatment-group denominators
Clinical analysis-window derivation

ODS PDF reporting
Clinical table formatting
Dataset Structure
The program expects ADaM-style datasets:

ADAE

Adverse Events dataset containing variables such as:
USUBJID
AESEQ
AESOC
AEDECOD
AETOXGR
AESTD
AESTDTC
ADSL

Subject-Level Analysis Dataset containing variables such as:
USUBJID
SAFFL
TRT01A
TRT01AN
TRTSDT
TRTEDT

Output

The program generates:AE_ctcae123SAF11_project.pdf

R

The R implementation provides an equivalent analysis workflow using R-based data manipulation and reporting techniques.

R Programming Skills Demonstrated


ADaM-style clinical data processing
ADAE and ADSL dataset integration
Safety Analysis Set filtering
CTCAE Grade 3+ adverse event identification
Treatment and analysis-window derivation
Subject-level deduplication
SOC and Preferred Term summarization
Treatment-group denominator calculations
Patient count and percentage derivation
Clinical table generation and formatting
Tidyverse-based data manipulation
Reproducible clinical programming
SAS-to-R analytical workflow translation



The output contains adverse events of CTCAE Grade 3 or higher summarized by System Organ Class and Preferred Term for the Safety Analysis Set.

DATA PRIVACY

This repository is intended for portfolio and educational purposes.

SKILLS DEMONSTRATED
SAS | ADaM | Clinical Programming | Clinical Safety Analysis | ODS Reporting|R

Author

Javan Mukunzi

