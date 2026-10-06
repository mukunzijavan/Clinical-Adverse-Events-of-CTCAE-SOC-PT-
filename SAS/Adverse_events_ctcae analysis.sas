/******************************************************************************
* PROGRAM      : adverse_events_ctcae.sas
*
* PURPOSE      : Generate a table of adverse events of CTCAE Grade 3 or higher
*                by System Organ Class (SOC) and Preferred Term (PT).
*
* ANALYSIS SET : Safety Analysis Set (SAF)
*
* AUTHOR       : Javan Mukunzi
*
* INPUTS       : ADaM ADAE - Adverse Events
*                ADaM ADSL - Subject-Level Analysis Dataset
*
* OUTPUT       :AE_ctcae123SAF11_project.pdf


/*=============================================================================
  1. SET UP ENVIRONMENT
=============================================================================*/

%let outpath = ./output;

/* Use a local project-relative path for public portfolio work. */
libname adam './data' access=readonly;


/*=============================================================================
  2. READ SOURCE ADaM DATASETS
=============================================================================*/

/* ADAE contains one or more records per adverse event. */
data adae;
    set adam.adae;
run;

/* ADSL contains one record per subject and provides treatment and analysis flags. */
data adsl;
    set adam.adsl;
run;
/*=============================================================================
  3. SORT DATASETS FOR SUBJECT-LEVEL MERGE
=============================================================================*/

proc sort data=adae;
    by USUBJID;
run;

proc sort data=adsl;
    by USUBJID;
run;


/*=============================================================================
  4. MERGE ADAE WITH ADSL
=============================================================================
  Purpose:
    Bring subject-level treatment information and Safety Analysis Set
    variables into the adverse-event dataset.

  Variables obtained from ADSL:
    - SAFFL   : Safety Analysis Set flag
    - TRT01A  : Actual treatment
    - TRT01AN : Numeric treatment code
    - TRTSDT  : Treatment start date
    - TRTEDT  : Treatment end date
=============================================================================*/

data adae_m;

    merge
        adae(in=_a)
        adsl(
            keep=USUBJID
                 SAFFL
                 TRT01A
                 TRT01AN
                 TRTSDT
                 TRTEDT);
by USUBJID;

    /* Retain only records originating from ADAE. */
    if not _a then delete;

    /*-----------------------------------------------------------------------
      Convert AESTDTC from character YYYY-MM-DD to a SAS date when needed.
    -----------------------------------------------------------------------*/

    if missing(AESTD) and not missing(AESTDTC) then do;

        if prxmatch(
            '/^\d{4}-\d{2}-\d{2}$/',
            strip(AESTDTC)
        ) then
            AESTD = input(AESTDTC, yymmdd10.);
        else
            AESTD = .;
    end;


    /*-----------------------------------------------------------------------
      Define the AE analysis stop date.

      Analysis stop date = treatment end date + 28 days.

      If TRTEDT is missing, the stop date remains missing and the event
      is retained subject to the remaining analysis criteria.
    -----------------------------------------------------------------------*/

    if not missing(TRTEDT) then
        stop_dt = TRTEDT + 28;
    else
        stop_dt = .;
        
    /*-----------------------------------------------------------------------
      Determine whether the AE occurred within the analysis window.

      Event is considered in-window when:

        AESTD >= TRTSDT

      AND

        AESTD <= TRTEDT + 28 days

      If TRTEDT is unavailable, the missing stop date is allowed through
      for subsequent QC/review.
    -----------------------------------------------------------------------*/

    if not missing(AESTD) and not missing(TRTSDT) then do;

        if AESTD >= TRTSDT and
           (missing(stop_dt) or AESTD <= stop_dt)

        then in_window = 1;

        else in_window = 0;

    end;
    else
        in_window = 1;
    /*-----------------------------------------------------------------------
      Standardize CTCAE grade.

    -----------------------------------------------------------------------*/

    if not missing(AETOXGR) then
        AETOXGRN =
            input(
                compress(strip(AETOXGR), , 'kd'),
                best12.
            );
    else
        AETOXGRN = .;
    /* Create Grade >=3 indicator. */
    grade3 = (AETOXGRN >= 3);

    /* Retain only variables required for downstream analysis. */
    keep
        USUBJID
        AESEQ
        AESOC
        AEDECOD
        AETOXGR
        AETOXGRN
        AESTD
        TRT01A
        TRT01AN
        SAFFL
        TRTSDT
        TRTEDT
        stop_dt
        in_window
        grade3;

run;


/*=============================================================================
  5. SELECT GRADE >=3 EVENTS IN THE SAFETY ANALYSIS SET
=============================================================================
  Selection criteria:

    SAFFL = "Y"
    Grade >=3
    Event occurred within the defined analysis window
=============================================================================*/

data adae_g3;
    set adae_m;
    where
        upcase(SAFFL) = 'Y'
        and grade3 = 1
        and in_window = 1;
run;

/*=============================================================================
  6. DEDUPLICATE SUBJECTS BY SOC / PT
=============================================================================
  Clinical reporting convention:

  A subject is counted only once for a particular Preferred Term (PT),
  even if that subject experienced the same event multiple times.

  DISTINCT is used to prevent multiple AE records for the same subject/PT
  from inflating the patient count.
=============================================================================*/

proc sql;
    create table subj_pt as
    select distinct
        USUBJID,
        AESOC   as SOC,
        AEDECOD as PT,
        TRT01A  as TRT,
        TRT01AN as TRT_CODE
    from adae_g3
    order by
        TRT_CODE,
        SOC,
        PT;
quit;
/*=============================================================================
  7. COUNT UNIQUE SUBJECTS BY SOC / PT / TREATMENT
=============================================================================*/

proc sql;

    create table pt_counts as

    select
        SOC,
        PT,
        TRT,
        TRT_CODE,
        count(distinct USUBJID) as n_subj from subj_pt

    group by
        SOC,
        PT,
        TRT,
        TRT_CODE

    order by
        SOC,
        PT,
        TRT_CODE;

quit;


/*=============================================================================
  8. DERIVE SAFETY DENOMINATORS
=============================================================================
  The denominator is the number of subjects in the Safety Analysis Set
  for each treatment group.

  This denominator is used for the percentage calculation:

=============================================================================*/

proc sql;

    create table trt_n as
    select
        TRT01AN as TRT_CODE,
        TRT01A  as TRT,
        count(distinct USUBJID) as N

    from adsl
    where upcase(SAFFL) = 'Y'

    group by
        TRT01AN,
        TRT01A
    order by
        TRT01AN;
quit;


/*=============================================================================
  9. DERIVE OVERALL GRADE >=3 AE COUNTS
=============================================================================
  This dataset provides the number of subjects with at least one Grade >=3
  AE in each treatment group.

  Because ADAE_G3 contains event-level records, COUNT(DISTINCT USUBJID)
  ensures each subject contributes only once to the overall count.
=============================================================================*/

proc sql;

    create table overall as

    select
        TRT01AN as TRT_CODE,
        TRT01A  as TRT,
        count(distinct USUBJID) as n_any

    from adae_g3

    group by TRT01AN

    order by TRT01AN;

quit;

/*=============================================================================
  10. BUILD SOC / PT REPORTING FRAME
=============================================================================
  Create the complete list of SOC/PT combinations observed in the analysis.

  CROSS JOIN with treatment denominators ensures that every SOC/PT has
  a reporting position for each treatment group, including zero counts.
=============================================================================*/

proc sql;
    create table soc_pt as
    select distinct
        SOC,
        PT
    from subj_pt
    order by
        SOC,
        PT;


    create table rpt_long as

    select
        s.SOC,
        s.PT,
        t.TRT,
        t.N,

        coalesce(
            p.n_subj,
            0
        ) as n_subj,

        case

            when t.N > 0 then
                100 * coalesce(p.n_subj, 0) / t.N

            else
                .

        end as pct

    from soc_pt as s

    cross join trt_n as t

    left join pt_counts as p

        on p.SOC      = s.SOC
        and p.PT      = s.PT
        and p.TRT     = t.TRT
        and p.TRT_CODE = t.TRT_CODE

    order by
        s.SOC,
        s.PT,
        t.TRT_CODE;

quit;

/*=============================================================================
  11. FORMAT COUNTS AND PERCENTAGES
=============================================================================
  Convert numeric results into the standard clinical table display:

      n (%)

  Example:

      12 (15.8%)
=============================================================================*/

data rpt_long_1;

    set rpt_long;

    length count_pct $20;

    count_pct =
        catx(
            ' ',
            put(n_subj, 8.),
            '(' || put(coalesce(pct, 0), 5.1) || '%)'
        );

run;
/*=============================================================================
  12. APPEND OVERALL GRADE >=3 AE ROWS
=============================================================================
  A special SOC value is assigned so that the overall row can be positioned
  separately from the actual System Organ Class categories.
=============================================================================*/

proc sql;
    create table overall_long as
    select
        'ZZZ' as SOC,
        'Any Grade >=3 AE' as PT,
        t.TRT,
        t.N,
        coalesce(
            o.n_any,
            0
        ) as n_subj,case
            when t.N > 0 then
                100 * coalesce(o.n_any, 0) / t.N
            else  .
end as pct
    from trt_n as t
    left join overall as o
        on t.TRT_CODE = o.TRT_CODE
    order by
        t.TRT_CODE;

quit;


/*=============================================================================
  13. FORMAT OVERALL COUNTS AND PERCENTAGES
=============================================================================*/

data overall_long_1;
    set overall_long;
    length count_pct $20;
    count_pct =
        catx(' ',put(n_subj, 8.),'(' || put(coalesce(pct, 0), 5.1) || '%)');

run;


/*=============================================================================
  14. COMBINE EVENT-LEVEL AND OVERALL REPORTING DATA
=============================================================================
  IMPORTANT:
  Proprietary treatment names are intentionally not retained in the
  public portfolio version.

  Treatment groups are represented as:

      Treatment A
      Treatment B
=============================================================================*/

data final_report_long;
    set
        rpt_long_1
        overall_long_1;

    /* Standardize treatment labels for public GitHub use. */

    if TRT = 'Treatment A' then
        TRT = 'Treatment A';
 else if TRT = 'Treatment B' then
        TRT = 'Treatment B';

    /* Standardize SOC values for consistent reporting. */

    SOC = upcase(SOC);

run;


/*=============================================================================
  15. ENSURE ONE REPORTING VALUE PER SOC / PT / TREATMENT
=============================================================================*/

proc sql;
    create table final_report_long2 as
    select
        SOC,
        PT,
        TRT,
        max(count_pct) as count_pct
    from final_report_long
    group by
        SOC,
        PT,
        TRT
    order by
        SOC,
        PT,
        TRT;
quit;


/*=============================================================================
  16. TRANSPOSE LONG DATA TO WIDE REPORTING STRUCTURE
=============================================================================
  PROC TRANSPOSE changes the dataset from:

      SOC | PT | Treatment | Count (%)

  to:

      SOC | PT | Treatment A | Treatment B

  This structure is suitable for PROC REPORT.
=============================================================================*/

proc transpose
    data=final_report_long2
    out=final_report_wide(drop=_NAME_);
    by
        SOC
        PT;
    id TRT;
    var count_pct;
run;
/*=============================================================================
  17. GENERATE FINAL CLINICAL TABLE
=============================================================================*/
options
    nodate
    nonumber
    linesize=132
    pagesize=60;


/* Public-safe output location and filename. */

ods pdf file="&outpath/ae_ctcae_grade3.pdf";

/* Table title */
title1
    "Adverse Events of CTCAE Grade 3 or Higher by SOC and PT";
title2
    "Safety Analysis Set";
/* Analysis population footnote */
footnote1
    "Analysis window: AE onset from treatment start through 28 days after treatment end.";
/*=============================================================================
  18. PROC REPORT
=============================================================================
  PROC REPORT creates the presentation-ready clinical table.

  Treatment columns are intentionally labeled using generic names to prevent
  disclosure of confidential compound or study information.
=============================================================================*/

proc report
    data=final_report_wide
    nowd
    headline
    spacing=1
    split='|'

    style(report)=[
        rules=group
        frame=box
    ]

    style(header)=[
        background=cxD9EAF7
        just=center
    ];


    /*-----------------------------------------------------------------------
      Define report columns.
    -----------------------------------------------------------------------*/

    column
        SOC
        PT
        (
            'Number (%) of Patients'
            'Treatment A'
            'Treatment B'
        );


    /*-----------------------------------------------------------------------
      System Organ Class
    -----------------------------------------------------------------------*/

    define
        SOC
        / order
          order=internal
          "System Organ Class"
          style(column)=[font_weight=bold];


    /*-----------------------------------------------------------------------
      Preferred Term
    -----------------------------------------------------------------------*/

    define
        PT
        / display
          "Preferred Term"
          left;


    /*-----------------------------------------------------------------------
      Treatment A
    -----------------------------------------------------------------------*/

    define
        'Treatment A'n
        / display
          width=15
          center
          "Treatment A";


    /*-----------------------------------------------------------------------
      Treatment B
    -----------------------------------------------------------------------*/

    define
        'Treatment B'n
        / display
          width=15
          center
          "Treatment B";


    /*-----------------------------------------------------------------------
      Add visual spacing between SOC groups.
    -----------------------------------------------------------------------*/

    compute before SOC;

        line ' ';

    endcomp;


    /* Add a page/report break after each SOC. */

    break after SOC / skip;

run;


/*=============================================================================
  19. CLOSE OUTPUT DESTINATION
=============================================================================*/

ods pdf close;


/*=============================================================================
  20. PROGRAM COMPLETION MESSAGE
=============================================================================*/

%put NOTE: Grade >=3 adverse event table generation complete.;
%put NOTE: Output file: &outpath/ae_ctcae_grade3.pdf;
