# fishprior

### Background

The fishprior R package provides a streamlined workflow for constructing Bayesian prior distributions for fish life-history parameters, using FishBase (and `rfishbase`). The package is structured around three steps: data retrieval, trait summarization, and prior construction.

Raw life-history data are queried from FishBase using the `get_fishbase_traits()` function. For a given vector of species names, the function retrieves five separate data tables from FishBase: von Bertalanffy growth parameters (popgrowth), population characteristics including maximum length and age (popchar), length-weight relationships (poplw), maturity schedules (maturity), and fecundity estimates (fecundity) and combines them into a single tidy long-format tibble. Each row in the resulting table corresponds to a single observation. Standard errors and sample sizes are provided for individual studies, and the reporting of this information varies by trait type. 

For prior construction, raw data from FishBase needs to be aggregated into a form that is useful for prior construction. The `summarize_fishbase_traits()` function aggregates raw trait data at the species level for eight life-history parameters commonly encountered in fish stock assessments: asymptotic length (L∞), the von Bertalanffy growth coefficient (k), natural mortality (M), maximum length (Lmax), maximum age (tmax), length at maturity (Lm), age at maturity (tm), and mean fecundity. For each species–trait combination, the function returns both arithmetic and log-scale summary statistics (mean and standard deviation), with the log-scale summaries computed after excluding non-positive values. 

The fishprior package constructs priors using a custom S4 class, which stores the distributional family, parameter values, trait name, prior type (informative or diffuse), grouping variable (typically species), and the underlying data. Two families of prior are currently supported: normal and lognormal, though future development will add a wider range of distributions.

### Installation instructions

The `fishprior` package can be installed via `remotes`, `devtools`, `pak`, or similar packages. The general syntax is

```r
pak::pak("NOAA-FIMS/fishprior")
```

### Documentation and Examples

The `pkgdown` generated documentation for the package is [https://noaa-fims.github.io/fishprior/](https://noaa-fims.github.io/fishprior/). This includes 
- [a basic demonstation of the package](https://noaa-fims.github.io/fishprior/articles/demo.html)
- [example workflow of a meta-analysis](https://noaa-fims.github.io/fishprior/articles/meta.html)

## NOAA Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

Software code created by U.S. Government employees is not subject to copyright in the United States (17 U.S.C. section 105). The United States/Department of Commerce reserve all rights to seek and obtain copyright protection in countries other than the United States for Software authored in its entirety by the Department of Commerce. To this end, the Department of Commerce hereby grants to Recipient a royalty-free, nonexclusive license to use, copy, and create derivative works of the Software outside of the United States.
