# HeartBD2K Significant Mapping
This project is the culmination of two quarters' worth of effort at my internship at HeartBD2K, which resulted in an R Shiny App. The aim of this project was to access big data quickly and to visualize it. This app saved a lot of time in creating visualizations and exploring data. The app has multiple functionalities, as will be listed below.

## Purpose
The app streamlined a lot of data analyzing by being able to achieve the following:
1. Using our 3000 annotated case reports (ACCRs), can map raw count of regions as well as significant regions (as determined by 2 proportion z-test in comparing the ACCRs to the 1.2 million total case report set) within each disease system using both US states and countries (on PUBMED, only the country name is available, so this project used text-mining extraction techniques from a previous project, named as Geographic Data in my GitHub repository, to mine the US states as well)
2. Using ACCRs, can map by decade
3. Can map by PMID, by searching the online PUBMED database
4. Can map by search phrase, by searching the online PUBMED database

These efforts save immense time since now, we no longer need to manually, for instance, download all case reports that are related to "vascular septal defects," clean the data, find the regions of each case report, analyze which regions are significant, and then map them. At the click of a few buttons, we can explore an entire database worth of data!

## Content
I have included the ui.R and server.R files that correlate to both the front- and back-end of the project as well as necessary data files needed to run the script and sample screenshots of the website.

## Limitations
This project shows only where case reports are most widely published, not necessarily incidence rate. To help normalize for countries that, in general, publish many case reports, I used the standardization technique, by comparing # of case reports/region as determined by the user search to the overall # of case reports/region of the entire 1.2 million case report database. However, the 1.2 million case repoort database is static, and will need to be constantly updated (which takes time to mine and extract regions from).

## Future Work
A few more functionalities will make this tool even more fresh:
1. Implement decade feature for PMID and searching options
2. Implement feature to visualize at the same time both decade feature and standardized proportion feature
3. Optimize entire algorithm for speed
4. Look into incidence/prevalance rates of major diseases to establish correlation between the mapping results and actual disease rates
5. Improve user interface/appearance of app
