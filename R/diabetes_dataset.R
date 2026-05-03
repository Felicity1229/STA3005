#' Diabetes Dataset
#'
#' This dataset is originally from the National Institute of Diabetes and Digestive
#' and Kidney Diseases. The objective of the dataset is to diagnostically predict
#' whether or not a patient has diabetes, based on certain diagnostic measurements.
#' All patients in this dataset are females at least 21 years old of Pima Indian heritage.
#'
#' @format A data frame with 768 rows and 9 columns:
#' \describe{
#'   \item{Pregnancies}{Number of pregnancies the patient has had.}
#'   \item{Glucose}{Plasma glucose concentration. A higher glucose level generally indicates a higher chance of diabetes.}
#'   \item{BloodPressure}{Diastolic blood pressure (mm Hg).}
#'   \item{SkinThickness}{Triceps skin fold thickness (mm).}
#'   \item{Insulin}{2-Hour serum insulin level (mu U/ml).}
#'   \item{BMI}{Body Mass Index (weight in kg/(height in m)^2), used to classify weight categories.}
#'   \item{DiabetesPedigreeFunction}{A function providing data on diabetes mellitus history in relatives and their genetic relationship to the patient, indicating hereditary risk.}
#'   \item{Age}{Age of the patient (years).}
#'   \item{Outcome}{Binary target variable: 1 = Patient has diabetes, 0 = Patient does not have diabetes.}
#' }
#' @source \url{https://www.kaggle.com/code/melikedilekci/diabetes-dataset-for-beginners}
"diabetes_data"
