# FloodCamML (Google Cloud Run)

This repository houses a template for a shiny web application that uses a machine learning model to classify webcam images for flooding and collect training images and labels from users. 

This template includes code for deploying the application to [**Google Cloud Run**](https://cloud.google.com/run). This template is based off the [COPE COMET](https://copecomet.github.io/index.html) "sunny-day" flooding shiny application, the [NC12 Flood CamML](https://github.com/FloodCamML/NC12-FloodCamML). 

The published web application is: 
* built with R using {[shiny](https://github.com/rstudio/shiny)}
* containerized with [Docker](https://www.docker.com/)
* hosted with [Google Cloud Run](https://cloud.google.com/run)

## Example

An example (NC-12 Flood CamML) is available at [floodcamml.org](https://floodcamml.org/)

## Other deployment options

Code for deploying the application to [shinyapps.io](https://www.shinyapps.io/) is available in the [FloodCamML_shinyapps repo](https://github.com/FloodCamML/FloodCamML_shinyapps).

## About the CamML Project

CamML is an open source project for crowd labeling and ML prediction of real-time webcam imagery. See the full project description in the [NC-12 FloodCamML repo](https://github.com/FloodCamML/NC12-FloodCamML#nc-12-floodcamml).

