#' Breast Cancer Dataset
#'
#' This dataset contains clinical and pathological records of breast cancer patients.
#' The data was collected for predicting patient survival status (Alive/Dead) based
#' on various tumor characteristics and patient demographics.
#'
#' @format A data frame with 4024 rows and 16 columns:
#' \describe{
#'   \item{Status}{Binary target variable: 1 = Alive, 0 = Dead}
#'   \item{Age}{Patient age at diagnosis (scaled)}
#'   \item{Tumor.Size}{Size of the tumor in millimeters (scaled)}
#'   \item{T.Stage}{Tumor stage: T1, T2, T3, T4 (ordered, increasing severity)}
#'   \item{N.Stage}{Node stage: N1, N2, N3 (ordered, increasing metastasis)}
#'   \item{X6th.Stage}{Overall cancer stage: IIA, IIB, IIIA, IIIB, IIIC}
#'   \item{Differentiate}{Degree of differentiation: Well, Moderate, Poor, Undifferentiated}
#'   \item{Grade}{Tumor grade: 1, 2, 3, 4}
#'   \item{Estrogen.Status}{Estrogen receptor status: 1 = Positive, 0 = Negative}
#'   \item{Progesterone.Status}{Progesterone receptor status: 1 = Positive, 0 = Negative}
#'   \item{A.Stage}{Adjuvant therapy stage: 1 = Regional, 0 = Distant}
#'   \item{Positive_Ratio}{Ratio of positive lymph nodes to examined nodes}
#'   \item{Survival.Months}{Survival time in months (scaled)}
#'   \item{RaceBlack}{One-hot encoded: Black race indicator}
#'   \item{RaceWhite}{One-hot encoded: White race indicator}
#'   \item{Marital.StatusDivorced}{One-hot: Divorced marital status}
#' }
#' @source \url{https://www.kaggle.com/datasets/breast-cancer}
"breast_cancer_data"
