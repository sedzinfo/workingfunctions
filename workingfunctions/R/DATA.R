#' Admission Data
#'
#' This data set contains information about graduate admission, 
#' including GRE scores, GPA, and the ranking of the undergraduate institution.
#'
#' @format A data frame with 8 rows and 4 variables:
#' \describe{
#'   \item{admit}{Binary variable indicating admission (0 = No, 1 = Yes)}
#'   \item{gre}{GRE (Graduate Record Examination) score}
#'   \item{gpa}{Grade Point Average}
#'   \item{rank}{Ranking of the undergraduate institution (1 = highest, 4 = lowest)}
#' }
#' @source researchpy repo
"df_admission"

#' Automotive Data
#'
#' This data set contains various automotive information including 
#' engine location, dimensions, weight, engine type, number of cylinders, 
#' and other specifications.
#'
#' @format A data frame with 38 rows and 26 variables:
#' \describe{
#'   \item{engine.location}{Location of the engine (e.g., front, rear)}
#'   \item{wheel.base}{Wheelbase of the vehicle in inches}
#'   \item{length}{Length of the vehicle in inches}
#'   \item{width}{Width of the vehicle in inches}
#'   \item{height}{Height of the vehicle in inches}
#'   \item{curb.weight}{Curb weight of the vehicle in pounds}
#'   \item{engine.type}{Type of engine (e.g., dohc, ohcv, ohc, l)}
#'   \item{num.of.cylinders}{Number of cylinders in the engine}
#'   \item{engine.size}{Size of the engine in cubic inches}
#'   \item{fuel.system}{Fuel system used (e.g., mpfi, 2bbl, mfi, 1bbl)}
#'   \item{bore}{Diameter of the cylinders in the engine}
#'   \item{stroke}{Stroke length of the engine}
#'   \item{compression.ratio}{Compression ratio of the engine}
#'   \item{horsepower}{Horsepower generated by the engine}
#'   \item{peak.rpm}{Peak RPM of the engine}
#'   \item{city.mpg}{Miles per gallon in the city}
#'   \item{highway.mpg}{Miles per gallon on the highway}
#'   \item{price}{Price of the vehicle}
#' }
#' @source Downloaded from Kaggle.com by the user Ramakrishnan Srinivasan. 
#' see \url{https://www.kaggle.com/toramky/automobile-dataset}
"df_automotive_data"

#' Blood Pressure Data
#'
#' This data set contains blood pressure readings for patients before and after a certain treatment or intervention.
#'
#' @format A data frame with 30 rows and 5 variables:
#' \describe{
#'   \item{patient}{Unique identifier for each patient}
#'   \item{sex}{Sex of the patient (e.g., Male, Female)}
#'   \item{agegrp}{Age group of the patient (e.g., 30-45, 46-59)}
#'   \item{bp_before}{Blood pressure reading before the intervention}
#'   \item{bp_after}{Blood pressure reading after the intervention}
#' }
#' @source researchpy repo
"df_blood_pressure"

#' Crop Yield Data
#'
#' This data set contains information about crop yields based on different fertilizer types and water conditions.
#'
#' @format A data frame with 20 rows and 3 variables:
#' \describe{
#'   \item{Fert}{Type of fertilizer used (A or B)}
#'   \item{Water}{Watering condition (High or Low)}
#'   \item{Yield}{Crop yield (in unspecified units)}
#' }
#' @source researchpy repo (simulated data, not real)
"df_crop_yield"

#' Difficile Data
#'
#' This data set contains information about the impact of different doses on libido.
#'
#' @format A data frame with 15 rows and 3 variables:
#' \describe{
#'   \item{person}{Unique identifier for each person}
#'   \item{dose}{Dose received (e.g., 1, 2, 3)}
#'   \item{libido}{Libido level of the person}
#' }
#' @source researchpy repo
"df_difficile"

#' Insurance Data
#'
#' This data set contains information about insurance charges based on various factors 
#' such as age, sex, BMI, number of children, smoking status, and region.
#'
#' @format A data frame with 19 rows and 7 variables:
#' \describe{
#'   \item{age}{Age of the individual}
#'   \item{sex}{Sex of the individual (e.g., male, female)}
#'   \item{bmi}{Body Mass Index of the individual}
#'   \item{children}{Number of children covered by the insurance}
#'   \item{smoker}{Smoking status (yes or no)}
#'   \item{region}{Region where the individual resides (e.g., southwest, southeast, northwest, northeast)}
#'   \item{charges}{Insurance charges}
#' }
#' @source researchpy repo
"df_insurance"

#' Responses State Data
#'
#' This data set contains simulated state information paired with participant numbers from the responses data set.
#'
#' @format A data frame with 28 rows and 2 variables:
#' \describe{
#'   \item{Participant.Number}{Unique identifier for each participant}
#'   \item{State}{State code where the participant resides (e.g., MI, OH, CO, CA, MA, WA)}
#' }
#' @source researchpy repo (simulated data, not real)
"df_responses_state"

#' Sexual Compatibility Data
#'
#' This data set contains responses to questions about sexual compatibility, 
#' including scores, gender, and age.
#'
#' @format A data frame with 22 rows and 13 variables:
#' \describe{
#'   \item{Q1}{Response to question 1}
#'   \item{Q2}{Response to question 2}
#'   \item{Q3}{Response to question 3}
#'   \item{Q4}{Response to question 4}
#'   \item{Q5}{Response to question 5}
#'   \item{Q6}{Response to question 6}
#'   \item{Q7}{Response to question 7}
#'   \item{Q8}{Response to question 8}
#'   \item{Q9}{Response to question 9}
#'   \item{Q10}{Response to question 10}
#'   \item{score}{Total score}
#'   \item{gender}{Gender of the respondent (1 = Male, 2 = Female)}
#'   \item{age}{Age of the respondent}
#' }
#' @source researchpy repo
"df_sexual_comp"
