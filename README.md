# HeartBD2K-Significant-Mapping
This project is the culmination of two quarters' worth of effort at my internship at HeartBD2K, which resulted in an R Shiny App. The aim of this project was to access big data quickly and to visualize it. This app saved a lot of time in creating visualizations and exploring data. The app has multiple functionalities, as will be listed below.

## Purpose
The app streamlined a lot of data analyzing by being able to achieve the following:
1. Using our 3000 annotated case reports (ACCRs), can map raw count of regions as well as significant regions (as determined by 2 proportion z-test in comparing the ACCRs to the 1.2 million total case report set) within each disease system
2. Using ACCRs, can map by decade
3. Can map by PMID, by searching the online PUBMED database
4. Can map by search phrase, by searching the online PUBMED database

These efforts save immense time since now, we no longer need to manually, for instance, download all case reports that are related to "vascular septal defects," clean the data, find the regions of each case report, analyze which regions are significant, and then map them. At the click of a few buttons, we can explore an entire database worth of data!

## Content
I have included the ui.R and server.R files that correlate to both the front- and back-end of the project as well as necessary data files needed to run the script and sample screenshots of the website.

## Future Work
A few more functionalities will make this tool even more fresh:
1. Implement decade feature for PMID and searching options
2. Implement feature to visualize at the same time both decade feature and standardized proportion feature
3. Optimize entire algorithm for speed
4. Instead of using country database, use country names that the ACCRs and user search yield
5. Improve user interface/appearance of app
