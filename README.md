- Data collected from DataSUS related to congenital anomalies from Parana 2013-2022
- Data cleaning into a initial base of variables
- Data transformation using DataSUS fields patterns
- Data visualization of the individual variables, aswell as the relation for each pair of anomaly x variable
- Non adjusted analysis for variable significancy usign chi-squared
- Data modelling, using GLM (Logistic Regression)

There are two options for the Shiny App: 

- On the fly data using app.r (aswell as r folder) where the plots are calculated using the database
- Agregated data with light_app.r for a lighter and faster version (deployed version) using light_data.
