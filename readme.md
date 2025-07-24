Angioedema incidence after first exposure to lisinopril
================
William J. O’Brien

# Introduction

The purpose of this research notebook is to demonstrate a reproducible
program to assess the incidence rate of angioedema within 1 year of
first drug exposure to lisinopril.

# R libraries and setup

The tangled R code of this notebook is in ./lisinoprilScript.R

This study uses renv for reproducibility. Re-create the R library by
running:

``` r
renv::restore()
```

``` r
rm(list=ls())

library(DatabaseConnector)
library(SqlRender)
library(CohortGenerator)
library(Capr)
library(CohortIncidence)
library(Eunomia)
library(dplyr) 
library(DT)

cdmSchema <- "main"
cohortSchema <- "main" 
cohortTableName <- "lisinopril"

niceTable <- function(x) kableExtra::kable_styling(knitr::kable(x))
```

# Database connection

``` r
connectionDetails <- getEunomiaConnectionDetails()

connection <- connect(connectionDetails)
```

# Eunomia “customization”

I am using the synthetic Eunomia CDM for testing. But there is a
problem… Eunomia was designed for running the model GI bleed study and
the very small concept table does not even have an entry for our target
and outcome. I will hack Eunomia and insert the concept name
“lisinopril” in place of diclofenac, and “angioedema” in place of
gastrointestinal hemorrhage. Of course we would never mutate the data in
a real CDM.

``` r
renderTranslateQuerySql(connection, 
                        "select * from @cdmSchema.concept 
                         where concept_name like 'diclofenac'",
                        cdmSchema = cdmSchema) |>
  niceTable()
```

<table class="table" style="color: black; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
CONCEPT_ID
</th>
<th style="text-align:left;">
CONCEPT_NAME
</th>
<th style="text-align:left;">
DOMAIN_ID
</th>
<th style="text-align:left;">
VOCABULARY_ID
</th>
<th style="text-align:left;">
CONCEPT_CLASS_ID
</th>
<th style="text-align:left;">
STANDARD_CONCEPT
</th>
<th style="text-align:left;">
CONCEPT_CODE
</th>
<th style="text-align:left;">
VALID_START_DATE
</th>
<th style="text-align:left;">
VALID_END_DATE
</th>
<th style="text-align:left;">
INVALID_REASON
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
1124300
</td>
<td style="text-align:left;">
Diclofenac
</td>
<td style="text-align:left;">
Drug
</td>
<td style="text-align:left;">
RxNorm
</td>
<td style="text-align:left;">
Ingredient
</td>
<td style="text-align:left;">
S
</td>
<td style="text-align:left;">
3355
</td>
<td style="text-align:left;">
1970-01-01
</td>
<td style="text-align:left;">
2099-12-31
</td>
<td style="text-align:left;">
NA
</td>
</tr>
</tbody>
</table>

``` r
renderTranslateQuerySql(connection, 
                        "select * from @cdmSchema.concept 
                        where concept_name like '%gastro%'",
                        cdmSchema = cdmSchema) |>
  niceTable()
```

<table class="table" style="color: black; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
CONCEPT_ID
</th>
<th style="text-align:left;">
CONCEPT_NAME
</th>
<th style="text-align:left;">
DOMAIN_ID
</th>
<th style="text-align:left;">
VOCABULARY_ID
</th>
<th style="text-align:left;">
CONCEPT_CLASS_ID
</th>
<th style="text-align:left;">
STANDARD_CONCEPT
</th>
<th style="text-align:left;">
CONCEPT_CODE
</th>
<th style="text-align:left;">
VALID_START_DATE
</th>
<th style="text-align:left;">
VALID_END_DATE
</th>
<th style="text-align:left;">
INVALID_REASON
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
35208414
</td>
<td style="text-align:left;">
Gastrointestinal hemorrhage, unspecified
</td>
<td style="text-align:left;">
Condition
</td>
<td style="text-align:left;">
ICD10CM
</td>
<td style="text-align:left;">
4-char billing code
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
K92.2
</td>
<td style="text-align:left;">
2007-01-01
</td>
<td style="text-align:left;">
2099-12-31
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
192671
</td>
<td style="text-align:left;">
Gastrointestinal hemorrhage
</td>
<td style="text-align:left;">
Condition
</td>
<td style="text-align:left;">
SNOMED
</td>
<td style="text-align:left;">
Clinical Finding
</td>
<td style="text-align:left;">
S
</td>
<td style="text-align:left;">
74474003
</td>
<td style="text-align:left;">
1970-01-01
</td>
<td style="text-align:left;">
2099-12-31
</td>
<td style="text-align:left;">
NA
</td>
</tr>
</tbody>
</table>

``` r
renderTranslateExecuteSql(
  connection,
  "update @cdmSchema.concept
   set concept_name = 'Lisinopril'
   where concept_id = 1124300;",
  cdmSchema = cdmSchema)
```


``` r
renderTranslateExecuteSql(
  connection,
  "update @cdmSchema.concept
   set concept_name = 'Angioedema'
   where concept_id = 192671;",
  cdmSchema = cdmSchema)
```


# Find standard concept ids

## Lisinopril

My use of the SqlRender functions in these queries is so that other
organizations can run it as-is without refactoring for different sql
dialects

``` r
renderTranslateQuerySql(
  connection, 
  "select * 
   from @cdmSchema.concept 
   where concept_name = 'Lisinopril'
     and domain_id = 'Drug'
     and concept_class_id = 'Ingredient'
     and standard_concept = 'S'",
  cdmSchema = cdmSchema) |>
  niceTable()
```

<table class="table" style="color: black; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
CONCEPT_ID
</th>
<th style="text-align:left;">
CONCEPT_NAME
</th>
<th style="text-align:left;">
DOMAIN_ID
</th>
<th style="text-align:left;">
VOCABULARY_ID
</th>
<th style="text-align:left;">
CONCEPT_CLASS_ID
</th>
<th style="text-align:left;">
STANDARD_CONCEPT
</th>
<th style="text-align:left;">
CONCEPT_CODE
</th>
<th style="text-align:left;">
VALID_START_DATE
</th>
<th style="text-align:left;">
VALID_END_DATE
</th>
<th style="text-align:left;">
INVALID_REASON
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
1124300
</td>
<td style="text-align:left;">
Lisinopril
</td>
<td style="text-align:left;">
Drug
</td>
<td style="text-align:left;">
RxNorm
</td>
<td style="text-align:left;">
Ingredient
</td>
<td style="text-align:left;">
S
</td>
<td style="text-align:left;">
3355
</td>
<td style="text-align:left;">
1970-01-01
</td>
<td style="text-align:left;">
2099-12-31
</td>
<td style="text-align:left;">
NA
</td>
</tr>
</tbody>
</table>

``` r
lisinoprilConcept <- 1124300
```

What are the descendants?

I always review these to get a sense of:

- sheer number of formulations/brands we are dealing with

- did the mapping team make any mistakes?

- single vs. combo drugs

- doses or IV/intramuscula route that might indicate drug exposure in
  ED, as opposed to typical Rx exposure

``` r
renderTranslateQuerySql(
  connection,
  "select descendant_concept_id, 
     concept_name descendant_concept_name
   from @cdmSchema.concept_ancestor a
   left join @cdmSchema.concept b
     on a.descendant_concept_id = b.concept_id
   where ancestor_concept_id = @lisinoprilConcept",
  cdmSchema = cdmSchema,
  lisinoprilConcept = lisinoprilConcept) |>
  niceTable() 
```

<table class="table" style="color: black; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
DESCENDANT_CONCEPT_ID
</th>
<th style="text-align:left;">
DESCENDANT_CONCEPT_NAME
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
1124300
</td>
<td style="text-align:left;">
Lisinopril
</td>
</tr>
</tbody>
</table>

Results: there are no descendants of the target concept in Eunomia

## Angioedema

``` r
renderTranslateQuerySql(
  connection, 
  "select * 
   from @cdmSchema.concept 
   where concept_name = 'Angioedema'
     and domain_id = 'Condition'
     and standard_concept = 'S'",
  cdmSchema = cdmSchema) |>
  niceTable()
```

<table class="table" style="color: black; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
CONCEPT_ID
</th>
<th style="text-align:left;">
CONCEPT_NAME
</th>
<th style="text-align:left;">
DOMAIN_ID
</th>
<th style="text-align:left;">
VOCABULARY_ID
</th>
<th style="text-align:left;">
CONCEPT_CLASS_ID
</th>
<th style="text-align:left;">
STANDARD_CONCEPT
</th>
<th style="text-align:left;">
CONCEPT_CODE
</th>
<th style="text-align:left;">
VALID_START_DATE
</th>
<th style="text-align:left;">
VALID_END_DATE
</th>
<th style="text-align:left;">
INVALID_REASON
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
192671
</td>
<td style="text-align:left;">
Angioedema
</td>
<td style="text-align:left;">
Condition
</td>
<td style="text-align:left;">
SNOMED
</td>
<td style="text-align:left;">
Clinical Finding
</td>
<td style="text-align:left;">
S
</td>
<td style="text-align:left;">
74474003
</td>
<td style="text-align:left;">
1970-01-01
</td>
<td style="text-align:left;">
2099-12-31
</td>
<td style="text-align:left;">
NA
</td>
</tr>
</tbody>
</table>

``` r
angioedemaConcept <- 192671
```

Looking at Athena, I noticed there are some angioedema descendants we
may want to exclude. For example, is hereditary angioedema an adverse
event attributable to ACE-I? That is a decision for a clinician to make,
but I will do so here for demonstration.

``` r
renderTranslateQuerySql(
  connection,
  "select descendant_concept_id, 
     concept_name descendant_concept_name
   from @cdmSchema.concept_ancestor a
   left join @cdmSchema.concept b
     on a.descendant_concept_id = b.concept_id
   where ancestor_concept_id = @angioedemaConcept",
  cdmSchema = cdmSchema,
  angioedemaConcept = angioedemaConcept) |>
  niceTable() 
```

<table class="table" style="color: black; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
DESCENDANT_CONCEPT_ID
</th>
<th style="text-align:left;">
DESCENDANT_CONCEPT_NAME
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
192671
</td>
<td style="text-align:left;">
Angioedema
</td>
</tr>
</tbody>
</table>

``` r
hereditaryAngioedemaConcept <- 4307793
```

# Concept sets

## Lisinopril

I like to pull the concept details after creating the concept set so
they are clearly labelled

``` r
lisinoprilCs <- cs(descendants(lisinoprilConcept), 
                   name = 'linosoprilCs') |>
  getConceptSetDetails(connection, cdmSchema) 

lisinoprilCs 
```

    ## ── <Capr Concept Set> linosoprilCs ─────────────────────────────────────────────
    ## # A tibble: 1 × 9
    ##   conceptId conceptCode conceptName domainId vocabularyId standardConcept
    ##       <int> <chr>       <chr>       <chr>    <chr>        <chr>          
    ## 1   1124300 3355        Lisinopril  Drug     RxNorm       S              
    ## # ℹ 3 more variables: includeDescendants <lgl>, isExcluded <lgl>,
    ## #   includeMapped <lgl>

## Angioedema

``` r
angioedemaCs <- cs(descendants(angioedemaConcept),
                   exclude(hereditaryAngioedemaConcept),
                   name = 'angioedemaCs') |>
  getConceptSetDetails(connection, cdmSchema)

angioedemaCs 
```

    ## ── <Capr Concept Set> angioedemaCs ─────────────────────────────────────────────
    ## # A tibble: 2 × 9
    ##   conceptId conceptCode conceptName  domainId    vocabularyId standardConcept
    ##       <int> <chr>       <chr>        <chr>       <chr>        <chr>          
    ## 1    192671 "74474003"  "Angioedema" "Condition" "SNOMED"     "S"            
    ## 2   4307793 ""          ""           ""          ""           ""             
    ## # ℹ 3 more variables: includeDescendants <lgl>, isExcluded <lgl>,
    ## #   includeMapped <lgl>

# Cohort definitions

## Target: lisinopril

The target cohort is people with their first drug exposure to the
ingredient lisinopril. Exit is 1 year after first exposure, so they
remain in the cohort whether or not they continue the medication. To
have reasonable assurance it is their first exposure, I will use an
attribute (in Capr-speak) to the cohort entry query that requires one
year of prior continous observation. I also include an age attribute
because I presume this is not a pediatric study. I would ask here if we
want to exclude from the cohort anyone with prior angioedema, but the
exercise does not specify so I will leave attrition as NULL.

``` r
targetCohort <- cohort(
  entry = entry(drugExposure(lisinoprilCs, 
                             firstOccurrence(),
                             age(gte(18))),
                observationWindow = continuousObservation(priorDays = 365)),
  attrition = NULL,
  exit = exit(endStrategy = fixedExit(offsetDays = 365)),
  era = NULL) 
```

## Event: angioedema

This includes all condition occurrences for the event, and one person
can appear in many rows. Cohort duration is arbitrarily one day, as only
the entry date matters here.

``` r
eventCohort <- cohort(
  entry = entry(conditionOccurrence(angioedemaCs)),
  attrition = NULL,
  exit = exit(endStrategy = fixedExit(offsetDays = 1)),
  era = NULL) 
```

## Cohort set

``` r
cohortSet <- makeCohortSet(targetCohort, eventCohort)
```

# Cohort generation

``` r
cohortTableNames <- getCohortTableNames(cohortTable = cohortTableName)

createCohortTables(connectionDetails = connectionDetails,
                   cohortDatabaseSchema = cohortSchema,
                   cohortTableNames = cohortTableNames)

generateCohortSet(connectionDetails = connectionDetails,
                  cdmDatabaseSchema = cdmSchema,
                  cohortDatabaseSchema = cohortSchema,
                  cohortTableNames = cohortTableNames,
                  cohortDefinitionSet = cohortSet) 
```

How many entries are in each cohort?

``` r
renderTranslateQuerySql(
  connection,
  "select cohort_definition_id, 
     count(*)
   from @cohortSchema.@cohortTableName
   group by cohort_definition_id",
  cohortSchema = cohortSchema,
  cohortTableName = cohortTableName) |>
  niceTable()
```

<table class="table" style="color: black; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
COHORT_DEFINITION_ID
</th>
<th style="text-align:right;">
COUNT(\*)
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
830
</td>
</tr>
<tr>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
479
</td>
</tr>
</tbody>
</table>

# Incidence rate calculation

``` r
t1 <- createCohortRef(id = 1, name = "Lisinopril cohort")

o1 <- createOutcomeDef(id = 1, name = "Angioedema outcome", cohortId = 2)

tar1 <- createTimeAtRiskDef(id = 1, 
                            startWith = "start",
                            endWith = "end")

analysis1 <- createIncidenceAnalysis(
  targets = c(t1$id), 
  outcomes = c(o1$id),
  tars = c(tar1$id)
)

irDesign <- createIncidenceDesign(
  targetDefs = list(t1),
  outcomeDefs = list(o1),
  tars = list(tar1),
  analysisList = list(analysis1)
)

buildOptions <- buildOptions(
  cohortTable = paste0(cohortSchema, ".", cohortTableName),
  cdmDatabaseSchema = cdmSchema,
  sourceName = "myCdm",
  refId = 1
)

incidenceResults <- executeAnalysis(connectionDetails = connectionDetails,
                                    incidenceDesign = irDesign,
                                    buildOptions = buildOptions)
```

# Results

``` r
sprintf("The incidence rate of angioedema within 1 year after initial lisinopril exposure was %.01f%% (%s/%s)",
        incidenceResults$incidence_summary$INCIDENCE_PROPORTION_P100P,
        incidenceResults$incidence_summary$OUTCOMES,
        incidenceResults$incidence_summary$PERSONS_AT_RISK
)
```

    ## [1] "The incidence rate of angioedema within 1 year after initial lisinopril exposure was 14.9% (124/830)"
