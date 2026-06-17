@echo off
setlocal

REM The default checkout on Windows is D:\a\da_case_studies\da_case_studies.
REM Some scripts expect chapter directories under D:\a\da_case_studies.
cd /d D:\a\da_case_studies

for /d %%i in (da_case_studies\ch*) do if not exist "%%~nxi" mklink /J "%%~nxi" "%%i"
for /d %%i in (da_case_studies\da*) do if not exist "%%~nxi" mklink /J "%%~nxi" "%%i"
for /d %%i in (da_case_studies\pre*) do if not exist "%%~nxi" mklink /J "%%~nxi" "%%i"

dir
