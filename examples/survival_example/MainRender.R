rmarkdown::render("/hewenjun/rstudio_work/Sharingan/examples/survival_example/Exploratory_Analysis_to_Survival_data.Rmd",
                  output_file = "Example_survival_analysis.html",
                  params = list(excel_path="/hewenjun/rstudio_work/Sharingan/examples/survival_example/example_survival_data.xlsx",
                                outcome_var_Time="OS",
                                outcome_var_Status="Death"))

