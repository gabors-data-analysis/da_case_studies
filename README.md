# da_case_studies

**R, Python and Stata code for**  
**Data Analysis for Business, Economics, and Policy**   
by Gábor Békés (CEU) and Gábor Kézdi (U. Michigan)   
Forthcoming in January 2021 by Cambridge University Press  
[**gabors-data-analysis.com**](https://gabors-data-analysis.com/)


## How to use
[Overview of data and code](https://gabors-data-analysis.com/data-and-code/)

## Get data
[Get data by datasets](https://drive.google.com/drive/folders/1g5j6v_WtB2lQDrSjpfhuw-P4s3Wm7Ucc?usp=sharing)  

## Status (18 September, 2020)
1. **R** -- All codes ready. Used for graphs in textbook. 
2. **Stata** -- All codes ready. In the lack of machine learning capabilities, no code for chapters 15,16,17, some limitations for chapter 18.
3. **Python**  -- Under preparation. Chapters 01-12 ready, chapters 13-24 are under development. Should be ready by early 2021. 

## Organization
1. Each case study has a separate folder.
2. Within case study folders, codes in different languages are simply stored together. 
3. Data should be downloaded and stored in a separate folder. 

## Code language versions
1. **R** -- We used R 4.0.2. 
2. **Stata** -- We used version 15, allmost all code should work in version 13 up.
3. **Python** -- We used Python 3.6.0.

## How to run case studies in R

Install packages `ch00-tech-prep/ch00_install_libraries`. See more [info incluidng package list](https://gabors-data-analysis.com/howto-r/){:target="_blank"}  

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
 

