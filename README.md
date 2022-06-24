In this repository we release all code to replicate all results, tables and figures presented in the paper:
HPO X ELA: Investigating Hyperparameter Optimization Landscapes by Means of Exploratory Landscape Analysis

The repository is structured as follows:
  * `data/` contains raw benchmark data and preprocessed data used for analyses
  * `plots/` contains plots as presented in the paper
  * `tasks/` contains data of all HPO tasks
  * `ela_splits.csv` contains the exact CV splits used for the HPO problems
  * `run_hpo.R` and `run_bbob.R` contain code to run optimizers on HPO and BBOB problems
  * `run_gensa_ablation` contains code to run the GENSA ablation study (see appendix)
    * Job scheduling on HPCs was performed using [batchtools](https://github.com/mllg/batchtools)
    * If you are interested in the full batchtools registries with all available information, please open an issue
  * `compute_features_hpo.R` and `compute_features_bbob.R` contain code for computing ELA features on HPO and BBOB
    problems
  * `preprocess_hpo.R` contains code to preprocess HPO data and visualize surface landscapes
  * `optimizer_performance.R` contains code for the analysis of optimizer performance
  * `optimizer_performance_gensa.R` contains code for the analysis of the GENSA ablation study (see appendix)
  * `ert.R` contains code for the ERT analyses of optimizers
  * `ela_analysis.R` contains code for the analysis of ELA features
  * `ela_cluster.R` contains code for the cluster analysis of ELA features
  * `predict_kmeans.R` contains helper code for predicting in k-means clustering
  * `tasks_cv_splits.R` contains code to generate the CV splits used for the HPO problems
  * `renv.lock` lists the exact R packages that were used on the cluster and can be used for setting up an
    [renv](https://github.com/rstudio/renv/) environment
  * `appendix.pdf` is our online appendix

# Online Appendix

You can find our appendix [here](appendix.pdf).
