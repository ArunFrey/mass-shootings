# The Impact of Mass Shootings on Attitudes toward Gun Restrictions

This repository contains code and data for our paper on _The Impact of Mass Shootings on Attitudes Towards Gun Restrictions_. 

## Abstract
Is the American public more likely to favor stricter gun legislation in the aftermath of deadly mass shootings? This paper leverages the occurrence of several mass shootings during multiple survey waves of the General Social Survey between 1987 and 2018 to examine whether exposure to a mass shooting sways public opinion on gun legislation. Results reveal that mass shootings increase support for stricter gun permits among Democrats but not for individuals of other political orientations. An exception to this finding occurs with school shootings, which mobilize broad support for firearm legislation among both Democrats and Republicans.


## Structure

The structure of the repository is as follows:
* `\DATA`: this folder contains the data on which the analysis is based. 
* `\SCRIPTS`: this folder includes the `R` code used to modify and analyse the data. 
* `\OUTPUT`: this folder includes all output generated by the `R` code. This folder in turn includes the following subfolders: `\MODELS`, `\TABLES`, and `\PLOTS`, which are each further subdivided to differentiate between descriptive (`\desc`), main (`\main`), and supplementary (`\supp`) output. 

## Code 
* To generate all tables and plots that feature in the paper and the appendix, run `00_run_all.R`. 
* `01_load_files.R`: Loads the raw data. 
* `02_cleaning.R`: generates the variables and sample
* `03_desc.R`: generates descriptive statistics. 
* `04_main.R`: generates the main results reported in the paper. 
* `04_r_*`: each script re-estimates the main results but using a different specification. 


## Authors

__Arun Frey__
Arun Frey is a Postdoctoral Researcher with the Leverhulme Centre for Demographic Science, and a member of the Department of Sociology at the University of Oxford. His research interests include immigration and group conflict, inequality, and causal inference and computational social science. His work has appeared in the _Proceedings of the National Academy of Sciences_ and the _European Sociological Review_. 

__David S. Kirk__
David Kirk is Professor in the Department of Sociology and Nuffield College at the University of Oxford as well as faculty associate of the Leverhulme Centre for Demographic Science. His recent book, _Home Free_, traces the effect of residential displacement among the formerly incarcerated in the wake of Hurricane Katrina. In other work, with collaborator Rob Sampson, he has launched a 5th survey wave of the Project on _Human Development in Chicago Neighborhoods_, to examine the correlates and consequences of gun violence over the life course over the last quarter century.

