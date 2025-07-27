## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 
# renv::restore()
# 


## ----output=FALSE, message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------

rm(list=ls())

library(DatabaseConnector)
library(SqlRender)
library(CohortGenerator)
library(Capr)
library(CohortIncidence)
library(Eunomia)
library(knitr)

cdmSchema <- "main"
cohortSchema <- "main" 
cohortTableName <- "lisinopril"



## ----warning=FALSE, message = FALSE, results='asis'-------------------------------------------------------------------------------------------------------------------------------------

connectionDetails <- getEunomiaConnectionDetails()

connection <- connect(connectionDetails)



## ----warning=FALSE, message = FALSE, results='asis'-------------------------------------------------------------------------------------------------------------------------------------

renderTranslateQuerySql(connection, 
                        "select concept_id,
                           concept_name,
                           standard_concept
                         from @cdmSchema.concept 
                         where concept_name like 'diclofenac'",
                        cdmSchema = cdmSchema) |>
  kable() 

renderTranslateQuerySql(connection, 
                        "select concept_id,
                           concept_name,
                           standard_concept
                         from @cdmSchema.concept 
                        where concept_name like '%gastro%'",
                        cdmSchema = cdmSchema) |>
  kable()



## ----warning=FALSE, message = FALSE, results='asis', output=FALSE-----------------------------------------------------------------------------------------------------------------------

renderTranslateExecuteSql(
  connection,
  "update @cdmSchema.concept
   set concept_name = 'Lisinopril'
   where concept_id = 1124300;",
  cdmSchema = cdmSchema)

renderTranslateExecuteSql(
  connection,
  "update @cdmSchema.concept
   set concept_name = 'Angioedema'
   where concept_id = 192671;",
  cdmSchema = cdmSchema)



## ----warning=FALSE, message = FALSE, results='asis'-------------------------------------------------------------------------------------------------------------------------------------

renderTranslateQuerySql(
  connection, 
  "select concept_id,
     concept_name,
     standard_concept 
   from @cdmSchema.concept 
   where concept_name = 'Lisinopril'
     and domain_id = 'Drug'
     and concept_class_id = 'Ingredient'
     and standard_concept = 'S'",
  cdmSchema = cdmSchema) |>
  kable()

lisinoprilConcept <- 1124300



## ----warning=FALSE, message = FALSE, results='asis'-------------------------------------------------------------------------------------------------------------------------------------

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
  kable() 



## ----warning=FALSE, message = FALSE, results='asis'-------------------------------------------------------------------------------------------------------------------------------------

renderTranslateQuerySql(
  connection, 
  "select concept_id,
     concept_name,
     standard_concept
   from @cdmSchema.concept 
   where concept_name = 'Angioedema'
     and domain_id = 'Condition'
     and standard_concept = 'S'",
  cdmSchema = cdmSchema) |>
  kable()

angioedemaConcept <- 192671



## ----warning=FALSE, output=FALSE, results='asis'----------------------------------------------------------------------------------------------------------------------------------------

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
  kable() 

hereditaryAngioedemaConcept <- 4307793



## ----message = FALSE, warning = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------

lisinoprilCs <- cs(descendants(lisinoprilConcept), 
                   name = 'linosoprilCs') |>
  getConceptSetDetails(connection, cdmSchema) 

str(lisinoprilCs@Expression)



## ----message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------

angioedemaCs <- cs(descendants(angioedemaConcept),
                   exclude(hereditaryAngioedemaConcept),
                   name = 'angioedemaCs') |>
  getConceptSetDetails(connection, cdmSchema)

str(angioedemaCs@Expression)
 


## ----output=FALSE, message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------

targetCohort <- cohort(
  entry = entry(drugExposure(lisinoprilCs, 
                             firstOccurrence(),
                             age(gte(18))),
                observationWindow = continuousObservation(priorDays = 365)),
  attrition = NULL,
  exit = exit(endStrategy = fixedExit(offsetDays = 365)),
  era = NULL) 



## ----output=FALSE, message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------

eventCohort <- cohort(
  entry = entry(conditionOccurrence(angioedemaCs)),
  attrition = NULL,
  exit = exit(endStrategy = fixedExit(offsetDays = 1)),
  era = NULL) 



## ----output=FALSE, message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------

cohortSet <- makeCohortSet(targetCohort, eventCohort)



## ----output = FALSE, message = FALSE, warning = FALSE, results='hide'-------------------------------------------------------------------------------------------------------------------

cohortTableNames <- getCohortTableNames(cohortTable = cohortTableName)

createCohortTables(connectionDetails = connectionDetails,
                   cohortDatabaseSchema = cohortSchema,
                   cohortTableNames = cohortTableNames)

generateCohortSet(connectionDetails = connectionDetails,
                  cdmDatabaseSchema = cdmSchema,
                  cohortDatabaseSchema = cohortSchema,
                  cohortTableNames = cohortTableNames,
                  cohortDefinitionSet = cohortSet) 



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

renderTranslateQuerySql(
  connection,
  "select cohort_definition_id, 
     count(*)
   from @cohortSchema.@cohortTableName
   group by cohort_definition_id",
  cohortSchema = cohortSchema,
  cohortTableName = cohortTableName) |>
  kable()



## ----output=FALSE, message=FALSE, warning=FALSE, results='hide'-------------------------------------------------------------------------------------------------------------------------

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



## ----message=FALSE, warning=FALSE, results='asis'---------------------------------------------------------------------------------------------------------------------------------------

sprintf("The 1-year incidence rate of angioedema after first exposure  
        to lisinopril was %.01f per 100 person-years",
        incidenceResults$incidence_summary$INCIDENCE_RATE_P100PY
)


