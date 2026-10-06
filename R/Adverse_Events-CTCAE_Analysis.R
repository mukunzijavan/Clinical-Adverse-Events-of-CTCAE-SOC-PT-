
# ============================================================================
# ADVERSE EVENTS OF SPECIAL INTEREST (AESI)
# ============================================================================
# Purpose:
#   Create a clinical trial safety table showing adverse events of special
#   interest (AESI), by grouped term and CTCAE grade, for the Safety Analysis
#   Set.
#
# Output:
#   Treatment-arm summary showing:
#     - Number and percentage of subjects with Grade 3+ AESIs
#     - System Organ Class (SOC)
#     - Preferred Term (PT)
#     - Treatment-group denominators
#     - Formatted output using gt and docorator
#
# Programmer: Javan Mukunzi
# Date: 21JUL2026
#
# Notes:
#   - Treatment names have been anonymized for portfolio/GitHub use.
#   - No sponsor-specific or proprietary treatment names are included.
#   - Local file paths should be replaced with project-relative paths before use.
# ============================================================================


# ============================================================================
# 1. LOAD REQUIRED PACKAGES
# ============================================================================

library(dplyr)
library(haven)
library(tidyr)
library(gt)
library(stringr)
library(forcats)


# ============================================================================
# 2. CLEAR WORKSPACE
# ============================================================================

rm(list = ls())


# ============================================================================
# 3. IMPORT ADaM DATASETS
# ============================================================================
# ADSL = Subject-Level Analysis Dataset
# ADAE = Adverse Events Analysis Dataset

# The paths below are examples only.

adsl <- read_sas("data/adam/adsl.sas7bdat")
adae <- read_sas("data/adam/adae.sas7bdat")


# ============================================================================
# 4. DEFINE SAFETY ANALYSIS SET
# ============================================================================
# SAFFL = "Y" identifies subjects included in the Safety Analysis Set.
#
# The Safety Analysis Set is used to determine the treatment-group denominator
# for calculating percentages.

adsl_saf <- adsl %>%
  filter(SAFFL == "Y")


# ============================================================================
# 5. CALCULATE TREATMENT-GROUP DENOMINATORS
# ============================================================================
# Count unique subjects in each treatment arm.
#
# TRT01P = Planned Treatment
# USUBJID = Unique Subject Identifier
#
# These denominators are later used to calculate percentages.

trt_denom <- adsl_saf %>%
  group_by(TRT01P) %>%
  summarise(
    N_trt = n_distinct(USUBJID),
    .groups = "drop"
  )


# ============================================================================
# 6. PREPARE ADVERSE EVENT DATA
# ============================================================================
# Select treatment-emergent adverse events with CTCAE Grade 3 or higher.
#
# TRTEMFL = Treatment-Emergent Flag
# AETOXGR = CTCAE Toxicity Grade
#
# The inner join ensures that only events belonging to subjects in the Safety
# Analysis Set are retained.

adae_data <- adae %>%
  filter(TRTEMFL == "Y") %>%
  filter(AETOXGR >= 3) %>%
  inner_join(
    adsl_saf %>%
      select(USUBJID, TRT01P),
    by = c("USUBJID", "TRT01P")
  ) %>%
  select(
    USUBJID,
    TRT01P,
    AEBODSYS,
    AEBDSYCD,
    AEDECOD,
    AETOXGR
  ) %>%
  distinct()


# ============================================================================
# 7. COUNT SUBJECTS WITH AESIs
# ============================================================================
# Count unique subjects within each treatment arm, SOC and PT.
#
# Using n_distinct(USUBJID) prevents multiple occurrences of the same event
# from being counted as multiple subjects.

ae_count <- adae_data %>%
  group_by(
    TRT01P,
    AEBODSYS,
    AEBDSYCD,
    AEDECOD
  ) %>%
  summarise(
    n_subjects = n_distinct(USUBJID),
    .groups = "drop"
  ) %>%
  rename(
    SOC = AEBODSYS,
    PT = AEDECOD
  )


# ============================================================================
# 8. CALCULATE SUBJECT COUNTS AND PERCENTAGES
# ============================================================================
# Merge treatment-group denominators and calculate the percentage of subjects
# with each AESI.
#
# Display format:
#   n (xx.x%)
#
# Example:
#   12 (15.8%)

ae_1 <- ae_count %>%
  left_join(
    trt_denom,
    by = "TRT01P"
  ) %>%
  mutate(
    pct = round((n_subjects / N_trt) * 100, 1),
    n_pct = sprintf(
      "%d (%.1f%%)",
      n_subjects,
      pct
    )
  ) %>%
  select(
    TRT01P,
    AEBDSYCD,
    SOC,
    PT,
    n_pct
  )


# ============================================================================
# 9. TRANSPOSE TREATMENT GROUPS TO COLUMNS
# ============================================================================
# Convert treatment arms from rows into columns so that each treatment group
# becomes a column in the final table.

ae_fmt1 <- ae_1 %>%
  pivot_wider(
    names_from = TRT01P,
    values_from = n_pct,
    values_fill = "0"
  )


# ============================================================================
# 10. DEFINE SOC ORDER
# ============================================================================
# Retain the order of System Organ Classes as they appear in the source data.
#
# If no SOC ordering is available, use alphabetical ordering as a fallback.

soc_order <- adae %>%
  filter(!is.na(AEBODSYS)) %>%
  distinct(AEBODSYS, AEBDSYCD) %>%
  pull(AEBODSYS)

if (length(soc_order) == 0) {
  soc_order <- sort(unique(ae_fmt1$SOC))
}


# ============================================================================
# 11. SORT SOC AND PT
# ============================================================================
# SOC is ordered according to the defined hierarchy.
# Preferred Terms are ordered alphabetically within SOC.

ae_sorted <- ae_fmt1 %>%
  mutate(
    SOC = factor(
      SOC,
      levels = soc_order
    ),
    PT = factor(
      PT,
      levels = sort(unique(PT))
    )
  ) %>%
  arrange(SOC, PT) %>%
  mutate(
    SOC = as.character(SOC),
    PT = as.character(PT)
  ) %>%
  mutate(
    # Section rows have indent level 0; PT rows have indent level 1.
    indent_level = ifelse(
      is.na(PT),
      0,
      1
    ),
    
    # Create display label for the table stub.
    stub_label = case_when(
      is.na(PT) ~ SOC,
      TRUE ~ paste0(
        "\u00A0\u00A0\u00A0",
        PT
      )
    )
  ) %>%
  group_by(SOC) %>%
  arrange(
    desc(indent_level),
    .by_group = TRUE
  ) %>%
  mutate(
    
    # Display SOC only on the first row of each SOC section.
    SOC_display = if_else(
      row_number() == 1,
      SOC,
      NA_character_
    ),
    
    # Add additional indentation for preferred terms.
    display_stub = paste0(
      strrep(
        "\u00A0",
        4 * indent_level
      ),
      stub_label
    )
  ) %>%
  ungroup()


# ============================================================================
# 12. CREATE SUMMARY ROW
# ============================================================================
# Calculate the number and percentage of subjects with at least one Grade 3+
# treatment-emergent AESI in each treatment group.

summary_row <- adae_data %>%
  group_by(TRT01P) %>%
  summarise(
    n_subjects = n_distinct(USUBJID),
    .groups = "drop"
  ) %>%
  left_join(
    trt_denom,
    by = "TRT01P"
  ) %>%
  mutate(
    pct = (n_subjects / N_trt) * 100,
    n_pct = sprintf(
      "%d (%.1f%%)",
      n_subjects,
      pct
    )
  ) %>%
  select(
    TRT01P,
    n_pct
  ) %>%
  pivot_wider(
    names_from = TRT01P,
    values_from = n_pct
  ) %>%
  mutate(
    SOC = "Patients with at least one AE",
    PT = NA_character_,
    indent_level = 0,
    stub_label = SOC,
    AEBDSYCD = 0
  ) %>%
  select(
    SOC,
    PT,
    indent_level,
    stub_label,
    everything()
  )


# ============================================================================
# 13. PREPARE FINAL ANALYSIS DATASET
# ============================================================================
# Add ordering variables and convert the source treatment names to generic
# treatment labels for GitHub/portfolio use.
#
# Original sponsor/product names are intentionally NOT included.

ae_ctcae <- bind_rows(
  summary_row,
  ae_sorted
) %>%
  mutate(
    col2 = SOC,
    name = PT,
    ord = AEBDSYCD
  )


# ============================================================================
# 14. ANONYMIZE TREATMENT NAMES
# ============================================================================
# IMPORTANT:
# Treatment names are deliberately changed to generic labels.
#
# The mapping is based on the treatment order in TRT01PN:
#   TRT01PN = 1 -> TREATMENT A
#   TRT01PN = 2 -> TREATMENT B
#


treatment_levels <- c(
  "TREATMENT A",
  "TREATMENT B"
)


# ============================================================================
# 15. CREATE TREATMENT COLUMNS
# ============================================================================
# Identify treatment columns generated during the pivot step.
#
# The treatment columns are renamed to generic QC column names so that the
# downstream reporting code is independent of the actual treatment names.

ae_ctcae <- ae_ctcae %>%
  rename(
    tt_ac001 = `TREATMENT A`,
    tt_ac002 = `TREATMENT B`
  ) %>%
  select(
    starts_with("tt_"),
    name,
    col2,
    ord
  ) %>%
  mutate(
    # Display SOC names in uppercase for rows representing sections.
    col2 = if_else(
      ord > 1,
      toupper(col2),
      col2
    )
  )


# ============================================================================
# 16. IDENTIFY TREATMENT COLUMNS
# ============================================================================

treatment_cols <- names(ae_ctcae)[
  !(names(ae_ctcae) %in% c(
    "SOC",
    "PT",
    "stub_label"
  ))
]

print(treatment_cols)


# ============================================================================
# 17. CREATE DEMONSTRATION DATASET
# ============================================================================
# This object is retained for the reporting/QC section.

demo_xstics <- ae_ctcae


# ============================================================================
# 18. DEFINE EXPECTED TREATMENT ORDER
# ============================================================================

expected_trt <- c(
  "TREATMENT A",
  "TREATMENT B"
)


# ============================================================================
# 19. CREATE TOTAL POPULATION FOR DENOMINATOR CALCULATION
# ============================================================================
# A "Total" treatment group is added to the subject-level dataset so that
# treatment-specific and overall denominators can be derived using the same
# framework.
#
# TRT01PN = Numeric treatment code.

aa <- adsl_saf %>%
  mutate(
    TRT01P = "Total",
    TRT01PN = 99
  )

bb <- bind_rows(
  adsl_saf,
  aa
)


# ============================================================================
# 20. CALCULATE BIG N FOR TABLE HEADERS
# ============================================================================
# Count unique subjects within each treatment group.
#
# BIG_N will be displayed in the column header:
#
#   TREATMENT A (N=XXX)
#   TREATMENT B (N=XXX)

bigN_hdr <- bb %>%
  distinct(
    USUBJID,
    TRT01P
  ) %>%
  count(
    TRT01P,
    name = "BIG_N"
  ) %>%
  mutate(
    TRT01P = as.character(TRT01P)
  )


# Create a named vector for easy lookup of treatment denominators.

denom_vec <- bigN_hdr$BIG_N

names(denom_vec) <- bigN_hdr$TRT01P


# ============================================================================
# 21. IDENTIFY QC TREATMENT COLUMNS
# ============================================================================
# Treatment columns follow the naming convention:
#
#   tt_ac001
#   tt_ac002
#   ...
#
# The regular expression identifies only columns following this pattern.

qc_trt_cols <- names(demo_xstics)[
  grepl(
    "^tt_ac\\d{3}$",
    names(demo_xstics)
  )
]

trt_qc_cols <- qc_trt_cols

if (length(qc_trt_cols) == 0) {
  stop(
    "No treatment columns matching tt_ac### were found."
  )
}


# ============================================================================
# 22. CREATE TREATMENT MAPPING
# ============================================================================
# Map numeric treatment codes to treatment labels.
#
# TRT01PN provides a stable ordering mechanism for treatment columns.

trt_map <- bb %>%
  distinct(
    TRT01P,
    TRT01PN
  ) %>%
  filter(!is.na(TRT01PN)) %>%
  distinct(
    TRT01P,
    .keep_all = TRUE
  ) %>%
  arrange(TRT01PN) %>%
  mutate(
    TRT01PN = ifelse(
      TRT01P == "Total",
      99,
      TRT01PN
    )
  )


# ============================================================================
# 23. MAP QC COLUMNS TO TREATMENT GROUPS
# ============================================================================
# Extract the numeric treatment identifier from the QC column name.
#
# Example:
#   tt_ac001 -> TRT01PN = 1
#   tt_ac002 -> TRT01PN = 2
#
# The mapping allows treatment labels to be assigned programmatically rather
# than relying on hard-coded column positions.

qc_col_map <- tibble(
  qc_col = qc_trt_cols
) %>%
  mutate(
    TRT01PN = as.integer(
      str_extract(
        qc_col,
        "\\d{3}$"
      )
    )
  ) %>%
  left_join(
    trt_map %>%
      mutate(
        TRT01PN = as.integer(TRT01PN)
      ),
    by = "TRT01PN"
  ) %>%
  select(
    qc_col,
    TRT01P,
    TRT01PN
  )


# ============================================================================
# 24. QC CHECK — TREATMENT MAPPING
# ============================================================================
# Stop the program if any treatment column cannot be mapped to a treatment
# group.

if (any(is.na(qc_col_map$TRT01P))) {
  
  stop(
    "Some treatment columns could not be mapped to TRT01P. ",
    "Missing TRT01PN: ",
    paste(
      qc_col_map$TRT01PN[
        is.na(qc_col_map$TRT01P)
      ],
      collapse = ", "
    )
  )
}


# ============================================================================
# 25. QC CHECK — EXPECTED TREATMENTS
# ============================================================================
# Confirm that both expected treatment groups are present.

if (!all(expected_trt %in% qc_col_map$TRT01P)) {
  
  stop(
    "Expected treatment names were not found after mapping.\n",
    "Expected: ",
    paste(
      expected_trt,
      collapse = " | "
    ),
    "\nMapped: ",
    paste(
      unique(qc_col_map$TRT01P),
      collapse = " | "
    )
  )
}


# ============================================================================
# 26. ORDER TREATMENT GROUPS
# ============================================================================
# Ensure the treatment groups appear in the predefined order.

qc_col_map <- qc_col_map %>%
  mutate(
    TRT01P = factor(
      TRT01P,
      levels = expected_trt
    )
  ) %>%
  arrange(TRT01P)


# ============================================================================
# 27. CREATE TABLE BODY ROWS
# ============================================================================
# Create the displayed stub and retain the treatment columns.
#
# "name" contains the Preferred Term.
# "col2" contains the SOC.
# "ord" controls the section ordering.

indent <- "  "

body_rows <- demo_xstics %>%
  mutate(
    STUB = paste0(
      indent,
      name
    ),
    seg = ord
  ) %>%
  select(
    seg,
    col2,
    STUB,
    all_of(qc_col_map$qc_col)
  )


# ============================================================================
# 28. CREATE SOC HEADER ROWS
# ============================================================================
# Create separate section/header rows for each SOC.

header_rows <- demo_xstics %>%
  distinct(
    ord,
    col2
  ) %>%
  rename(
    seg = ord
  ) %>%
  arrange(seg) %>%
  transmute(
    seg,
    col2,
    STUB = col2
  )


# Populate treatment columns with blank values for SOC header rows.

for (cc in qc_col_map$qc_col) {
  header_rows[[cc]] <- ""
}


# ============================================================================
# 29. COMBINE HEADER AND BODY ROWS
# ============================================================================
# Each SOC header is followed by its corresponding Preferred Terms.

report_df <- bind_rows(
  header_rows %>%
    mutate(row_id = 0L),
  
  body_rows %>%
    group_by(seg) %>%
    mutate(
      row_id = row_number()
    ) %>%
    ungroup()
) %>%
  arrange(
    seg,
    row_id
  ) %>%
  select(
    STUB,
    all_of(qc_col_map$qc_col)
  )


# ============================================================================
# 30. PRESERVE LEADING SPACES
# ============================================================================
# Convert normal leading spaces to non-breaking spaces.
#
# This is useful when rendering the table because some output formats may
# collapse regular whitespace.

make_leading_spaces_visible <- function(x) {
  
  x <- ifelse(
    is.na(x) | x == "",
    "\u00A0",
    x
  )
  
  m <- regexpr(
    "^\\s+",
    x
  )
  
  has <- m > 0
  
  lead <- ifelse(
    has,
    regmatches(x, m),
    ""
  )
  
  rest <- ifelse(
    has,
    substring(
      x,
      attr(m, "match.length") + 1
    ),
    x
  )
  
  lead_nbsp <- vapply(
    lead,
    function(s) {
      paste(
        rep(
          "\u00A0",
          nchar(s)
        ),
        collapse = ""
      )
    },
    character(1)
  )
  
  paste0(
    lead_nbsp,
    rest
  )
}


# ============================================================================
# 31. FORMAT TABLE VALUES
# ============================================================================

report_df <- report_df %>%
  mutate(
    STUB = make_leading_spaces_visible(STUB),
    
    across(
      all_of(qc_col_map$qc_col),
      ~ ifelse(
        is.na(.x) | .x == "",
        "\u00A0",
        as.character(.x)
      )
    )
  ) %>%
  mutate(
    across(
      everything(),
      ~ gsub(
        "\\bNA\\b",
        "",
        as.character(.)
      )
    )
  )


# ============================================================================
# 32. CREATE COLUMN LABELS
# ============================================================================
# Treatment headers are displayed as:
#
#   TREATMENT A (N=XXX)
#   TREATMENT B (N=XXX)

hdr_labels <- setNames(
  paste0(
    as.character(qc_col_map$TRT01P),
    " (N=",
    as.integer(
      denom_vec[
        as.character(qc_col_map$TRT01P)
      ]
    ),
    ")"
  ),
  qc_col_map$qc_col
)

# Replace missing denominators with zero.

hdr_labels <- setNames(
  gsub(
    "N=NA",
    "N=0",
    hdr_labels
  ),
  names(hdr_labels)
)


# ============================================================================
# 33. BUILD GT TABLE
# ============================================================================

trt_qc_cols <- qc_col_map$qc_col

DC_T01 <- gt(report_df) %>%
  
  # Define column labels.
  cols_label(
    .list = c(
      list(
        STUB = "\u00A0"
      ),
      as.list(hdr_labels)
    )
  ) %>%
  
  # Align table stub to the left.
  cols_align(
    align = "left",
    columns = "STUB"
  ) %>%
  
  # Center treatment results.
  cols_align(
    align = "center",
    columns = all_of(trt_qc_cols)
  ) %>%
  
  # Center treatment column headers.
  tab_style(
    style = cell_text(
      align = "center"
    ),
    locations = cells_column_labels(
      columns = all_of(trt_qc_cols)
    )
  ) %>%
  
  # Reduce header font size to help retain one-line headers.
  tab_style(
    style = cell_text(
      size = gt::px(14)
    ),
    locations = cells_column_labels(
      columns = all_of(trt_qc_cols)
    )
  ) %>%
  
  # Define overall table font size.
  tab_options(
    table.font.size = gt::px(9)
  ) %>%
  
  # Define column widths.
  cols_width(
    STUB ~ gt::px(240),
    all_of(trt_qc_cols) ~ gt::px(190)
  )


# ============================================================================
# 34. EXTRACT TABLE DATA FOR QC
# ============================================================================

mm <- DC_T01[["_data"]]


# ============================================================================
# 35. LOAD PDF REPORTING PACKAGES
# ============================================================================

library(tinytex)
library(docorator)


# ============================================================================
# 36. RENDER FINAL PDF
# ============================================================================
# docorator is used to add a clinical-trial-style header and footer.
#
# Sponsor-specific protocol information has been replaced with generic
# portfolio values.

DC_T01 |>
  as_docorator(
    
    display_name = "AE OF SPECIAL INTEREST",
    
    display_loc = "output",
    
    header = fancyhead(
      
      fancyrow(
        left = "Protocol: XXXX-XXXX",
        center = NA,
        right = doc_pagenum()
      ),
      
      fancyrow(
        left = "Population: Safety Analysis Set",
        center = NA,
        right = "Data as of DDMMMYYYY"
      ),
      
      fancyrow(
        left = NA,
        center = "Table 1.001",
        right = NA
      ),
      
      fancyrow(
        left = NA,
        center =
          "Adverse events of special interest, by grouped term and CTCAE grade",
        right = NA
      )
    ),
    
    footer = fancyfoot(
      
      fancyrow(
        left = "",
        center = NA,
        right = NA
      ),
      
      fancyrow(
        left = "Clinical Programming Portfolio",
        right = toupper(
          format(
            Sys.time(),
            "%d%b%Y %H:%M"
          )
        )
      )
    )
    
  ) |>
  render_pdf()


# ============================================================================
# END OF PROGRAM
# ============================================================================
