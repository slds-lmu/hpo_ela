In this repository we release all code to replicate all results, tables and figures presented in the paper:
HPO X ELA: Investigating Hyperparameter Optimization Landscapes by Means of Exploratory Landscape Analysis

The repository is structured as follows:
  * `data/` contains raw benchmark data and preprocessed data used for analyses
  * `plots/` contains plots as presented in the paper
  * `run_hpo.R` and `run_bbob.R` contain code to run optimizers on HPO and BBOB problems
    * Job scheduling on HPCs was performed using [batchtools](https://github.com/mllg/batchtools)
    * If you are interested in the full batchtools registries with all available information, please open an issue
  * `compute_features_hpo.R` and `compute_features_bbob.R` contain code for computing ELA features on HPO and BBOB
    problems
  * `preprocess_hpo.R` contains code to preprocess HPO data and visualize surface landscapes
  * `optimizer_performance.R` contains code for the analysis of optimizer performance
  * `ert.R` contains code for the ERT analyses of optimizers
  * `ela_analysis.R` contains code for the analysis of ELA features
  * `ela_cluster.R` contains code for the cluster analysis of ELA features
  * `predict_kmeans.R` contains helper code for predicting in k-means clustering
  * `renv.lock` lists the exact R packages that were used on the cluster and can be used for setting up an
    [renv](https://github.com/rstudio/renv/) environment

