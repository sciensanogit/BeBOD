## BeBOD

_Belgian National Burden of Disease Study_

The `BeBOD` package provides helper functions for the calculation of Years of Life Lost as part of the Belgian National Burden of Disease Study.  

The BeBOD project is initiated and managed by Sciensano, the Belgian institute for health. More information on the BeBOD project can be found on https://www.sciensano.be/en/projects/belgian-national-burden-disease-study. 

#### Available functions

<table>
<tr><td><code>rsle</code></td><td>Calculate residual life expectancy</td></tr>
<tr><td><code>explode_icd</code></td><td>Expand ICD group into individual ICD codes</td></tr>
<tr><td><code>map_gbd</code></td><td>Map level 4 GBD cause to other levels</td></tr>
</table>

#### Available datasets

<table>
<tr><td><code>gbd</code></td><td>ICD10 to GBD cause list map</td></tr>
<tr><td><code>causelist</code></td><td>Master cause list used in the BeBOD study</td></tr>
</table>

#### Install

To download and install the latest development version from GitHub:
```r
devtools::install_github("sciensanogit/BeBOD")
```
