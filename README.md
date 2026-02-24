# Quick guide

This GitHub repository provides the PRODEMOS open-source health-economic simulation model aimed to estimate the cost-effectiveness of the PRODEMOS coach-supported mobile health intervention for the primary prevention of dementia. 

A detailed description of the model and its input estimates can be found here: <https://doi.org/10.1016/j.tjpad.2026.100526>, which is linked to released version 1.0.0. 

The branch "main" is for development and fune tuning before release and is intended to be stable. The branch "develop" (if present) is for developing and testing and is not necessarily intended to be stable. Releases are stable versions often associated to specific applications of the model. 

Contact details of the main developer Ron Handels can be found here: <https://www.maastrichtuniversity.nl/r-handels>. 

# Requirements

The model script is written in R (version 4.3.1) and relies on the package 'heemod' (version 1.0.1). 

# Installation tutorial

- Make sure to have R installed (<https://cran.r-project.org/mirrors.html>) and possibly also RStudio installed (https://www.rstudio.com).
- Download latest release (e.g., via <https://github.com/ronhandels/prodemos/releases/>). 
- Extract ZIP to your desired folder. 
- Open R file named "PRODEMOS open-source model.R" (stored in folder "R") in R or RStudio.
- Follow the instruction at the beginning of the script to set the working directory and install packages.
- Run the remainder of the script or source it with echo. The script loosely follows the DARTH coding framework <https://doi.org/10.1007/s40273-019-00837-x> in terms of headings and prefixes, contains in-line comments and loosely follows the TidyVerse styleguide <https://style.tidyverse.org/>. Results are printed in the console. 

# Cite this work

Name: PRODEMOS open-source model <https://github.com/ronhandels/prodemos>. 

Citation: <https://doi.org/10.1016/j.tjpad.2026.100526>

# Version details

## v1.0.0 (2026; released)

This version is linked to the publication entitled "Prevention of dementia using mobile phone applications (PRODEMOS) – a health-economic cost-utility analysis in people aged 55–75 years with low socio-economic status" <https://doi.org/10.1016/j.tjpad.2026.100526>. Please note this R version represents only an adapted version of the base case of the model. It does not reflect decreased utility and costs in the first year after a myocardial infarction or stroke event. Therefore, the outcomes of this R version differ from the outcomes reported in the publication. See publication supplementary material for details. 

Features: Adapted base case. 

Developers: 

- Original model: Ron Handels, Anders Wimo, Linus Jonsson. 
- R replication: Yunfei Li, Linus Jonsson, Ron Handels. 

# Acknowledgment

- We acknowledge the PRODEMOS study collaborators Ron Handels, Marieke Hoevenaar-Blom, Manshu Song, Carol Brayne, Eric Moll van Charante, Fiona E Matthews, Junfang Xu, Linus Jönsson, Nicola Coley, Rachael Brooks, Xuening Jian, Tingting Qin, Youxin Wang, Wei Wang, Edo Richard, Anders Wimo, PRODEMOS study group.
- We acknowledge all developers and contributors of the model. 
- We acknowledge the developers of the `heemod` package. 
