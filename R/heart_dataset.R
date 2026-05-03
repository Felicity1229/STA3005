#' Heart Disease Dataset
#'
#' This dataset contains clinical and non-invasive test results of patients.
#' It is widely used to predict the presence of heart disease based on various
#' patient attributes, symptom descriptions, and diagnostic measurements.
#'
#' @format A data frame with 14 columns (13 predictors and 1 target variable):
#' \describe{
#'   \item{age}{Age of the patient in years}
#'   \item{sex}{Gender of the patient: 0 = male, 1 = female}
#'   \item{cp}{Chest pain type: 0 = Typical angina, 1 = Atypical angina, 2 = Non-anginal pain, 3 = Asymptomatic}
#'   \item{trestbps}{Resting blood pressure in mm Hg}
#'   \item{chol}{Serum cholesterol in mg/dl}
#'   \item{fbs}{Fasting blood sugar level, categorized as above 120 mg/dl: 1 = true, 0 = false}
#'   \item{restecg}{Resting electrocardiographic results: 0 = Normal, 1 = Having ST-T wave abnormality, 2 = Showing probable or definite left ventricular hypertrophy}
#'   \item{thalach}{Maximum heart rate achieved during a stress test}
#'   \item{exang}{Exercise-induced angina: 1 = yes, 0 = no}
#'   \item{oldpeak}{ST depression induced by exercise relative to rest}
#'   \item{slope}{Slope of the peak exercise ST segment: 0 = Upsloping, 1 = Flat, 2 = Downsloping}
#'   \item{ca}{Number of major vessels (0-4) colored by fluoroscopy}
#'   \item{thal}{Thalium stress test result: 0 = Normal, 1 = Fixed defect, 2 = Reversible defect, 3 = Not described}
#'   \item{target}{Target variable indicating heart disease status: 0 = no disease, 1 = presence of disease}
#' }
#' @source \url{https://www.kaggle.com/code/farzadnekouei/heart-disease-prediction/input}
"heart_dataset"
