# Heat-Risks-in-Swiss-Milk-Production

Citation for the use of any code: 

Bucheli et al. (2022). Heat Risks in Swiss Milk Production. Journal of the Agricultural and Applied Economics Association.

The code has the following modular structure: 

01_sample_geo: Prepares the data, identifies the location of a farm and creates figure 1

02_RH_municipality: Get's relative humidity for each municipality center by identifying the closest weather station

03_Daily_THI: Calculates hourly THI-values for each farm

04_Precipitation: Calculates cumulative precipitation for each farm

05_Daily_temperature: Calculates hourly temperature-values for each farm

06_1_model_revenue: Model calibration and estimation for milk revenues

06_2_figure_revenue: Figures for milk revenues

07_1_model_veterinary: Model calibration and estimation for veterinary expenses

07_2_figure_veterinary: Figures for veterinary expenses

08_1_model_feed: Model calibration and estimation for feed purchases

08_2_figure_feed: Figures for feed purchases

09_descriptive_figures: Figures for descriptive statistics

The appendix contains code to derive minimum detectable slopes of the models and run the robustness checks. Robustness checks only contain codes different from the main specification. Run these codes together with correpsonding modular codes 01-08. 
