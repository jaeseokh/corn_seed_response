## **Code Structure**


### **Functions Folder**
This folder contains reusable R scripts with modular functions:
- `functions_for_analysis.R`: Core functions for data analysis.
- `functions_for_process.R`: Functions used for data cleaning and processing.
- `functions_for_table_figure.R`: Functions to generate tables and visualizations.
- `unpack_trial_info.R`: Helper script to process trial-level information.

### **Main Folder**
This folder contains the main workflow scripts and RMarkdown files:
- **`0_Set_up_preparation.R`**: Prepares the workspace, loads libraries, and initializes the environment.
- **`1_Data_Process.Rmd`**: Processes raw data, including cleaning, transformation, and preparation.
- **`2_Analysis_Results(GAM).Rmd`**: Performs Generalized Additive Model (GAM)-based analysis and generates results.
- **`3_Make_figures_tables.Rmd`**: Creates summary tables and visualizations for reporting.

## **How to Use**
1. Start with `0_Set_up_preparation.R` to initialize the environment.
2. Run `1_Data_Process.Rmd` to process the data.
3. Use `2_Analysis_Results(GAM).Rmd` to perform GAM analysis.
4. Generate figures and tables with `3_Make_figures_tables.Rmd`.

## **Contact**
If you need access to the data, figures, or tables, please contact me at [jaeseok2@illinois.edu](mailto:jaeseok2@illinois.edu).
