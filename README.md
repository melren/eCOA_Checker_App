# eCOA_Checker_App
Sample application for vendor eCOA data QC

# About
This app was created as part of my internship projects at Genentech. It is designed for the QC of received Electronic Clinical Outcome Assessments (eCOA) data provided by an external vendor for our phase II study in the Ulcerative Colitis indication. This is a modified version of the app and is intended to be used as a sample for my project portfolio. The purpose of this application is to cross-check the vendor-derived data values (bundled in the eCOA file with raw data) with our own derivation of the same variables based off the same raw data. It also provides some generic data checks to test for the presence of illogical data.

# Directions
1. Obtain portal dump file(s) in .csv format
    * Normally the file(s) would be obtained directly from the vendor but for this showcase purpose I have provided a downloadable sample file to use in the sidebar.
2. Select the type of data available
    * Choose Comprehensive Data Dump if the vendor single comprehensive data dump is available
      * This is the file with all testcodes stacked together in rows (Please choose this option and upload the sample I provided.)
    * Choose Raw Portal Data if the separate data files from the portal are available
      * Three separate files downloaded from the ERT portal are required for this option
      * Daily Diary Data is the file with only raw diary entries
      * Daily Diary Scores Data is the file with the vendor-computed average diary scores
      * Bowel Prep Date Data is the file with bowel prep and endoscopy date information
3. Upload the correct file(s) first before attempting to navigate to other pages of the app
4. Use this the Portal Data Checker tab/page to check device algorithm (average daily diary score calculations)
    * Click the Download Comprehensive Report button to download a PDF with a summary of the data QC
5. Use the Data Viewer to view a summary of the computed scores and raw diary entries for uploaded data

Once the Vendor Portal data file is uploaded, the app will automatically refresh to load the data check and display a summary of whether the data checks have passed. Aqua-colored tables indicate checks passed while red-colored tables indicate errors were found. If checks failed, data tables with the inconsistent records will appear below.
