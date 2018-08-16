__I am aware of the WARNING reported by win-builder. Please see below.__

## Test environments

* local Linux (CentOS 7 & Linux Mint), R 3.5.0
* local Windows (10), R 3.4.4
* Linux on Travis-CI (devel and release)
* Mac on Travis-CI (release)
* Windows on Appveyor (release)
* win-builder (devel)

## R CMD check results

Overall, there were no ERROR, 1 WARNING and 2 NOTEs:

* Found the following significant warnings:
  d:/RCompile/CRANpkg/lib/3.6/BH/include/boost/interprocess/detail/win32_api.hpp:145:9: warning: ISO C++ prohibits anonymous structs [-Wpedantic]
  d:/RCompile/CRANpkg/lib/3.6/BH/include/boost/interprocess/detail/win32_api.hpp:153:9: warning: ISO C++ prohibits anonymous structs [-Wpedantic]
  d:/RCompile/CRANpkg/lib/3.6/BH/include/boost/interprocess/detail/win32_api.hpp:178:13: warning: ISO C++ prohibits anonymous structs [-Wpedantic]
  d:/RCompile/CRANpkg/lib/3.6/BH/include/boost/interprocess/detail/win32_api.hpp:180:7: warning: ISO C++ prohibits anonymous structs [-Wpedantic]
  
-> Package BH comes with these issues on 'r-devel-windows-ix86+x86_64' only. 
   This is also the case for many other packages (e.g. package bigmemory) and should be fixed in the next version of package BH.

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Florian Privé <florian.prive.21@gmail.com>'

New submission

Package was archived on CRAN

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2018-02-02 as usage restrictions in
    README were incompatible with a FOSS licence.
    
  -> I removed the Code of Conduct.    

  It tried to write in the installed package's location.
  
  -> I've searched everywhere. To the best of my ability, I think this is fixed, but I've no way to be sure.


* checking installed package size ... NOTE

  - 11.7 Mb on Linux
  -  5.3 Mb on Windows
  -  < 5 Mb on Mac
