# Significant Mapping for Clinical Case Reports

This project is an R Shiny App intended to provide a map visualization of the worldwide geographic distribution of clinical case reports.

The initial version of this project was assembled and thoroughly tested by Kitu Komya. Additional contributions were provided by Clement Feyt, Joel Perez, and Amanda Tsai.

## Purpose
This map performs the following functions, given our set of annotated clinical case reports (ACCRs):
* Maps the raw count of regions as well as significant regions (as determined by 2 proportion z-test in comparing the ACCRs to a set of 1.2 million total clinical case reports) within each disease system using both US states and countries. Documents obtained through PubMed in MEDLINE format include only country name, so this project also incorporates text-mining extraction techniques to determine US states and resolve discrepancies (see *geographic-data.R*).
* Maps by decade.

The app can also:
* Map by specific PMID
* Map by search phrase

both by executing a PubMed query.

These efforts save time, as the usual protocol is as follows: 
1. Manually download all case reports that are related to a given search term or MeSH descriptor, e.g., "vascular septal defects" 
2. Clean the data
3. Find the regions of each case report
4. Analyze which regions are significant
5. Create a map

## Content
The ui.R and server.R files correlate to both the front- and back-end of the project. 

The data processing file, *geographic-data.R*, contains methods for parsing case report location based on institutional affiliation. The file also contains a simplified version of the map visualization method.

Necessary data files are provided in the folder *data*. Sample screenshots are provided in the folder *screenshots*.

## Limitations
This project shows only where case reports are most widely published, not necessarily incidence rate. To help normalize for countries that, in general, publish many case reports, the app use the standardization technique, by comparing the count of case reports per region as determined by the user search to the overall count of case reports per region of the full set of  clinical case reports. Note that the full case report set is static and does not reflect the most recent set of case reports currently available within MEDLINE.

## Future Work
A few more functionalities will make this tool even more fresh:
1. Implement decade feature for PMID and searching options
2. Implement feature to visualize at the same time both decade feature and standardized proportion feature
3. Optimize entire algorithm for speed
4. Look into incidence/prevalance rates of major diseases to establish correlation between the mapping results and actual disease rates
5. Improve user interface/appearance of app
