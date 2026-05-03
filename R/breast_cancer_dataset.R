#' Breast Cancer Dataset
#'
#' This dataset contains clinical and pathological records of breast cancer patients.
#' The data was collected for predicting patient survival status (Alive/Dead) based
#' on various tumor characteristics and patient demographics.
#'
#' @format A data frame with 4024 rows and 16 columns:
#' \describe{
#'   \item{Age}{Patient age at diagnosis (scaled)}
#'   \item{Race}{One-hot encoded: Black, white and others}
#'   \item{Marital.Status}{One-hot: Divorced marital status (Single, Married, Divorced, Widowed)}
#'   \item{T.Stage}{Tumor stage: T1, T2, T3, T4 (ordered, increasing severity)}
#'   \item{N.Stage}{Node stage: N1, N2, N3 (ordered, increasing metastasis)}
#'   \item{X6th.Stage}{Overall cancer stage: IIA, IIB, IIIA, IIIB, IIIC}
#'   \item{Differentiate}{Degree of differentiation: Well, Moderate, Poor, Undifferentiated}
#'   \item{Grade}{Tumor grade: 1, 2, 3, 4}
#'   \item{A.Stage}{Adjuvant therapy stage: 1 = Regional, 0 = Distant}
#'   \item{Tumor.Size}{Size of the tumor in millimeters (scaled)}
#'   \item{Estrogen.Status}{Estrogen receptor status: 1 = Positive, 0 = Negative}
#'   \item{Progesterone.Status}{Progesterone receptor status: 1 = Positive, 0 = Negative}
#'   \item{Regional.Node.Examined}{Number of regional lymph nodes examined
#'         during biopsy/surgery. Typically ranges from 0-30+}
#'   \item{Regional.Node.Positive}{Number of lymph nodes that tested positive
#'         for cancer metastasis (out of the examined count). Higher values
#'         indicate more advanced disease}
#'   \item{Survival.Months}{Survival time in months (scaled)}
#'   \item{Status}{Binary target variable: 1 = Alive, 0 = Dead}
#' }
#' @source \url{https://www.kaggle.com/datasets/alaahussien/breast-canser}
"breast_cancer_data"
