# Harvard-University-Data-Science-Movie-Lens-Project

### Movie Recommendation System

This repository contains the capstone project submission for the HarvardX PH125.9x Data Science: Capstone Project, part of the HarvardX Data Science Professional Certificate program.

### Project Overview

This project develops a movie recommendation system using the MovieLens 10M dataset, which includes user ratings for movies. The primary objective is to predict movie ratings with high accuracy, achieving a Root Mean Squared Error (RMSE) below 0.86490 on the validation set. The scope involves data ingestion, exploratory data analysis (EDA), model development (including linear models, regularization, and matrix factorization), and performance evaluation. This work demonstrates practical application of machine learning techniques for collaborative filtering, highlighting their significance in enhancing user experiences on platforms like Netflix or Amazon.


### Technologies Used

**Programming Language:** R (version 4.4 or compatible)

**Key Libraries:**

-tidyverse (for data manipulation and visualization)

-caret (for model training and evaluation)

-ggplot2 (for plotting)

-lubridate (for date-time handling)

-recosystem (for matrix factorization in recommendation systems)

**Tools:** RStudio (recommended for development), Git for version control



### Installation Instructions

**Clone the Repository:**
git clone https://github.com/your-username/movie-recommendation-system.git
cd movie-recommendation-system

**Install R: **Ensure R is installed on your system. Download from CRAN.

**Install Required Packages:** Open R or RStudio and run the following commands to install dependencies:

install.packages(c("tidyverse", "caret", "ggplot2", "lubridate", "recosystem"), repos = "http://cran.us.r-project.org")


**Verify Setup: **Load the libraries in an R session to confirm installation:

-library(tidyverse)

-library(caret)

-library(ggplot2)

-library(lubridate)

-library(recosystem)



## Note:  The process may take a few minutes, especially for downloading the MovieLens dataset during script execution.
Usage


**Run the Main Script: ** Execute the R script to perform data processing, model training, and evaluation:  movielens.R

This script downloads the MovieLens dataset, creates training/validation sets, performs EDA, trains models (e.g., regularized linear and matrix factorization), and outputs RMSE results.


**Key Outputs:**

*Console prints:* Summary statistics, model performance (RMSE, MSE, MAE), and visualizations (if run interactively in RStudio).

*Generated Files:* None by default; results are displayed in the console or can be exported manually.


*Interactive Mode:* For exploring visualizations or modifying parameters, open movielens.R in RStudio and run sections step-by-step.

## Note: Set a seed for reproducibility (e.g., set.seed(1)). The script may take several minutes to run due to dataset size and model training.

### Project Report
The comprehensive project report is available as Movielens_report.pdf in the repository root. It includes:

-Introduction and dataset summary

-Methods and analysis (data ingestion, EDA, model development)

-Results (model performance tables and visualizations)

-Conclusion and references

This PDF was generated from an R Markdown file and totals 29 pages.
