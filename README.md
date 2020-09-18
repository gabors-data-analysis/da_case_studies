# da_case_studies
**Code for Case Studies in**

**Data Analysis for Business, Economics, and Policy** by Gábor Békés (CEU) and Gábor Kézdi (U. Michigan) 

Graduate textbook forthcoming (January 2021) by Cambridge University Press

## Code language versions
1. **R** -- We used v4.0.2. Install packages `ch00-tech-prep/ch00_install_libraries`. See more [info incluidng package list](https://gabors-data-analysis.com/howto-r/){:target="_blank"}  
2. **Stata** -- We used version 15, allmost all code should work in version 13 up.
3. **Python** (in preparation) -- We used Python 3.8.3.


## Organization
1. Each case study has a separate folder.
2. Within case study folders, codes in different languages are simply stored together. 
3. Some intermediary files (csv, dta, rds) may be saved there, too. 
4. Currently output is not stored here 

## Get data
[Get data by datasets](https://drive.google.com/drive/folders/1g5j6v_WtB2lQDrSjpfhuw-P4s3Wm7Ucc?usp=sharing)  
NOTE: Draft version, please do not share. 


## How to run case studies in R

1. Step 1: Set the working directory for your project.

	- Option 1: [Recommended] In case you use `RStudio` create a new `Rstudio` project for the case studies and load it every time you are working on the project. See the [official documentation](https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects) on how to create and use `Rstudio` projects.
	- Option 2: Make sure some other way that your working directory is the root folder of the case study repository.

2. Step 2: Change set-data-directory file to include your actual folder containing the data
make sure some other way that this `.Renviron` file is processed.

## How to run case studies in Stata

Install user written packages `ch00-tech-prep/ch00_install_libraries`. 

## How to run case studies in Python


#### Without Docker:
Install requirements.txt from ch00-tech-prep folder, and use jupyter-notebook to run notebooks!

#### Recommended steps with Docker:

 1.  Please install [Docker](https://docs.docker.com/get-docker/) on your computer. 
 2. Create a project folder with any name and any location!
 
	 - Clone this repository into the project folder or download and extract the da_case_studies folder.
	 - Download the data folder into the project folder.
	 - Make sure you have da_case_studies and da_data_repo in your project folder.
		 - If you use different names please update the Dockerfile!
 3.  Open terminal (Linux & MacOS) or PowerShell/Docker Terminal (Windows) and navigate to you project folder.
 4. Use the following code to build docker image (after that use run command only):
	```
	docker build -t "da_jupyter" -f da_case_studies/Dockerfile .
	```
 5. Start this image with the following:
 ```
docker run --rm -p 443:443 da_jupyter
```
 6. Use Ctrl+click (Cmd+click) on jupyter link in your terminal or just copy and paste into your browser
 7. ENJOY!
 

