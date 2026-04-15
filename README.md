# Echoes of an Era: The Anatomy of Popular Music

## Project Description
This interactive dashboard explores over 160,000 Spotify tracks from 1921-2020 to analyze how the audio characteristics of popular music have evolved alongside historical events and what makes a hit song today.

**Team 10 Members:** [Marilyn Ross, Varun Korisapati, Simi Chakravarty]

## Prerequisites
To run this application locally, you will need R and RStudio installed on your machine. You will also need to install the following R packages. You can install them by running this command in your R console:

`install.packages(c("shiny", "plotly", "dplyr", "tidyr", "readr", "stringr", "RColorBrewer", "DT", "bslib"))`

## How to Run the App Locally
1. Clone this repository to your local machine (or download it as a ZIP file and extract it).
2. Open RStudio.
3. Open the `Dashboard.R` file.
4. Go to the top menu and select **Session** -> **Set Working Directory** -> **To Source File Location** to ensure R is looking in the correct folder.
5. Verify that the three data files (`data_by_year.csv`, `spotify_dataset.csv`, `data_w_genres.csv`) are located in the exact same directory as `Dashboard.R`.
6. Click the **"Run App"** button at the top right of the script editor.
