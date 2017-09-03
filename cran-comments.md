## This is a resubmission. In this version,
* I've removed all the NOTEs I could.
* I've given some examples of the statistical tools that are provided even if a full list of the features of this package has already been provided in the README file.
* I've mentionned that there is a scientific paper in preparation associated with this package (not yet submitted, not even to bioRxiv).
* I've removed the double space in the description. 
* I've removed the Remotes field from the DESCRIPTION file.
* I've reduced some tolerances of tests.
* I've incremented the package version.


## Test environments
* local Linux (CentOS 7), R 3.3.3 and 3.4.0
* local Windows (10), R 3.4.0
* Linux on Travis-CI (devel and release)
* Max on Travis-CI (release)
* Windows on Appveyor (release)
* win-builder (devel)

## R CMD check results
There were no ERRORs or WARNINGs. 

Overall, there were 2 NOTEs:

* checking installed package size ... NOTE

  - 12.6 Mb on Linux
  -  5.6 Mb on Windows
  -  < 5 Mb on Mac

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Florian Privé <florian.prive.21@gmail.com>'
  New submission
  
  I don't understand this NOTE because I have another package on CRAN (https://cran.r-project.org/package=primefactr).
