% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict_with_model.R
\name{predict_with_model}
\alias{predict_with_model}
\title{Predict with Model and Print Results}
\usage{
predict_with_model(prediction_data, model, training_data)
}
\arguments{
\item{prediction_data}{A data frame containing the prediction data}

\item{model}{A trained model (either lm or ranger random forest)}

\item{training_data}{The original training data used to fit the model}
}
\value{
A data frame with the predicted results
}
\description{
Predict with Model and Print Results
}
