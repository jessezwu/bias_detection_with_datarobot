# Bias Detection and Mitigation Demo

Given a list of protected features defined in `protected_features.txt`, run tests to detect and mitigate against model bias.

## Requirements

All scripts are developed in R.

To use, you will need to have set up the R datarobot package, see also the [documentation](https://cran.r-project.org/web/packages/datarobot/vignettes/IntroductionToDataRobot.html).

Currently assumes a binary classification problem.

### Installation

`Rscript install.R`

## Use

Project definitions should be defined in a yaml config file, e.g. `config/project_config.yaml`.

TODO

Once satisfied, run `./generate_report.sh`
