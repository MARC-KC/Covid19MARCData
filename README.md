
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Mid-America Regional Council’s COVID-19 Data

This repository holds all the code [MARC](https://www.marc.org/) uses to
keep their [COVID-19 Data Hub](https://marc2.org/covidhub/) up to date
for the Kansas City Region. It exposes the entire process for
downloading the most up-to-date datasets from our MARC Data API and
transforming it to the forms needed to feed the [COVID-19 Data
Hub](https://marc2.org/covidhub/).

## How to use this repository?

### If you just want to download the base datasets directly as a zipped set of csv files:

<img src="man/figures/downloadCSVs.png" height="50px" style='border: 5px solid #555;'>

These files are updated as part of our process for pushing updated data
to our publication server so they should match the data in the MARC Data
API.

### If you want access to the API directly, the GET URL’s are as follows:

  - Case, Death, and Test Data:
    </br><https://gis2.marc2.org/MARCDataAPI/api/covidcasedeathtest>
      - This is the time series of the back-updated Case, Death, Test
        data that MARC uses
  - Newly Reported Case, Death, and Test Data:
    </br><https://gis2.marc2.org/MARCDataAPI/api/covidcasedeathtestnewlyreported>
      - This is the time series of Newly Reported Case, Death, Test data
        that MARC uses to provide an estimate of how many Cases, Deaths,
        and Tests were reported in the last 24 hours similar to how the
        media reports these values. This data is not back-updated.
  - Hospital Data:
    </br><https://gis2.marc2.org/MARCDataAPI/api/covidhospital>
      - This is the time series of the back-updated Hospital data that
        MARC uses
  - **Note** The LastUpdated columns are in UTC time if downloading
    directly from the API. The time conversion to
    ‘America/Chicago’/‘Central’ is implemented when downloading
    from the R package with the function `downloadBaseData()`

### If you want to implement the API or the R package into your pipeline using R

#### Make sure dependencies are installed:

Programs to install:

  - R (From CRAN; latest version at time of writing was 4.0.3)
  - RStudio (Helpful R IDE)

Once these programs are installed. Open up RStudio and install the
{remotes} package by running:

``` r
install.packages('remotes')
```

Then install the Covid19MARCData package with:

``` r
remotes::install_github('MARC-KC/Covid19MARCData')
```

This will launch a process that may install a bunch of packages that the
Covid19MARCData package is dependent upon. Once completed you should be
able to load the new package with:

``` r
library(Covid19MARCData)
```

This will attach the package to your environment and allow you to call
the package functions. See the section [Using the R package
Covid19MARCData](#using-the-r-package-covid19marcdata) for more
information.

## Using the R package Covid19MARCData

#### Download the data

This is similar to calling the API directly, except that it loads the
resulting table into the R session and does the conversion of the
LastUpdated columns from UTC to Central time.  
You can download all three datasets using the same function with a
different `type` argument:

``` r
#Case, Death, and Test Data
cdtData <- downloadBaseData(type = "CDT")

#Newly Reported Case, Death, and Test Data
cdtNRData <-  downloadBaseData(type = "CDT_NewlyReported")

#Hospital Data
hospData <-  downloadBaseData(type = "Hospital")
```

## Where does the data come from?

See COVID Data Hub FAQ