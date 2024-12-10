# rs-wealth-changes
Repository for replication code and data for analysis in "Estimating the Impacts of Changes in Wealth on the Physical Characteristics of Neighborhood Environments."

Filepaths in R files will need to be repaired.

R Code in two files, one to pull and write files of census data, one to perform the analysis. Additional data files from the US census through IPUMS are required - 1990 tract level data using the variables: EYU001, EST001, E4U001. IPUMS access here: https://www.nhgis.org

Extraction of remote sensing data was performed with Google Earth Engine. All code can be found here:https://code.earthengine.google.com/2a0c55b5f61d25603e7edb9463e93839
The necessary shapefiles to run the GEE codes can be extracted using the finalproject_demodatapull_11.19.2024.R file.

