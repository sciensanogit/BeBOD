## BeBOD

[![Travis-CI Build Status](https://travis-ci.org/brechtdv/BeBOD.svg?branch=master)](https://travis-ci.org/brechtdv/BeBOD)

_Belgian National Burden of Disease Study_

The `BeBOD` package provides helper functions for the Belgian National Burden of Disease Study.  

The BeBOD project is initiated and managed by Sciensano, the Belgian institute for health. More information on the BeBOD project can be found on https://www.sciensano.be/en/projects/belgian-national-burden-disease-study. 

#### Available functions

Calculation of Years of Life Lost
<table>
<tr><td><code>rsle</code></td><td>Calculate residual life expectancy</td></tr>
<tr><td><code>explode_icd</code></td><td>Expand ICD group into individual ICD codes</td></tr>
<tr><td><code>sim_target_icd</code></td><td>Simulate target specific causes based on ICD codes</td></tr>
<tr><td><code>sim_target_gbd</code></td><td>Simulate target specific causes based on GBD causes</td></tr>
<tr><td><code>map_gbd</code></td><td>Map level 4 GBD cause to other levels</td></tr>
<tr><td><code>redistribute_gbd</code></td><td>Redistribute based on GBD causes</td></tr>
<tr><td><code>redistribute_gbd_all</code></td><td>Redistribute to all GBD causes</td></tr>
<tr><td><code>plot_idd</code></td><td>Plot original vs redistributed deaths</td></tr>
<tr><td><code>plot_yll</code></td><td>Plot original vs redistributed YLLs</td></tr>
<tr><td><code>tab</code></td><td>Tabulate results</td></tr>
</table>

#### Install

To download and install the latest development version from GitHub:
```r
devtools::install_github("brechtdv/BeBOD")
```
