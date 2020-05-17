
# Population aging and COVID-19 death rate relationship analysis

In this project, I attempted to use multiple linear regression and
general linear models to study the relationship between the portion of
the senior (age 65 and up) in the population and the COVID-19 death
rate. To approach this problem, I tested 11 potential confounding
variables and several statistical models. My final statistical model is
a GLM model including only two confounding variables, `med_bed` (the
number of hospital beds per 1,000 people) and `death_100_ind` (the ratio
of days with fewer than 100 accumulated deaths from 1/21/2020 to
5/14/2020). I assessed the model with bootstrapping and provided some
evidence that the COVID-19 death rate is positively associated with the
portion of the senior in the population.

The two COVID-19 datasets, [the global confirmed
dataset](https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv)
and [the global death
dataset](https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv),
are from the [data
repository](https://github.com/CSSEGISandData/COVID-19) for the 2019
Novel Coronavirus Visual Dashboard operated by the Johns Hopkins
University Center for Systems Science and Engineering (JHU CSSE). The
dataset is updated every day starting from January 22nd, 2020. Both
datasets contain columns of location information (`Province/State`,
`Country/Region`, `Lat`, and `Long`) or daily accumulated numbers of
confirmed or deaths in the corresponding location, starting from
`1/22/20` to present. Other country statistical data are from the [World
Bank Open Data](https://data.worldbank.org/) and the [Oxford Covid-19
Government Response Tracker
repository](https://github.com/OxCGRT/covid-policy-tracker/).

## Data Analysis and Report

The exploratory data analysis and complete data analysis is in the [eda
notebook](https://github.com/flizhou/age_effects_on_COVID-19/blob/master/scripts/eda.ipynb)
and the final report can be found
[here](https://github.com/flizhou/age_effects_on_COVID-19/blob/master/doc/report.md)

## Dependencies

  - Python 3.7.4 and Python packages:
      - pandas==0.25.2
      - requests==2.23.0
  - R 3.6.1 and R packages:
      - knitr==1.27.2
      - tidyverse==1.3.0
      - mice==3.7.0  
      - MASS==7.3.51.4
      - rsample==0.0.5
      - rjson==0.2.20
      - testthat==2.3.1
      - repr==1.0.1
