# python ch00-tech-prep/test_env.py

import unittest
import nbconvert
import os
import subprocess

NOTEBOOKS = [
    "ch01-billion-prices-collect",
    "ch01-hotels-data-collect",
    "ch01-management-data-collect",
    "ch02-football-manager-success",
    "ch02-hotels-data-prep",
    "ch02-immunization-crosscountry",
    "ch03-city-size-japan",
    "ch03-distributions-height-income",
    "ch03-football-home-advantage",
    "ch03-hotels-europe-compare",
    "ch03-hotels-vienna-explore",
    "ch03-simulations",
    "ch04-management-firm-size",
    "ch05-stock-market-loss-generalize",
    "ch06-online-offline-price-test",
    "ch06-stock-market-loss-test",
    "ch07-hotels-simple-reg",
    "ch07-ols-simulation",
    "ch08-hotels-measurement-error",
    "ch08-hotels-nonlinear",
    "ch08-life-expectancy-income",
    "ch09-gender-age-earnings",
    "ch09-hotels-europe-stability",
    "ch10-gender-earnings-understand",
    "ch10-hotels-multiple-reg",
    "ch11-australia-rainfall-predict",
    "ch11-smoking-health-risk",
    "ch12-electricity-temperature",
    "ch12-stock-returns-risk",
    "ch12-time-series-simulations",
    "ch13-used-cars-reg",
    "ch14-airbnb-reg",
    "ch14-used-cars-log",
    "ch15-used-cars-cart",
    #"ch16-airbnb-random-forest",
    #"ch17-predicting-firm-exit",
    "ch18-case-shiller-la",
    "ch18-swimmingpool",
    "ch19-food-health",
    "ch20-ab-test-social-media",
    "ch20-working-from-home",
    "ch21-ownership-management-quality",
    "ch22-airline-merger-prices",
    "ch23-immunization-life",
    "ch23-import-demand-and-production",
    "ch24-football-manager-replace",
    #"ch24-haiti-earthquake-gdp",
]

notebook_files = []
for folder in NOTEBOOKS:
    files = os.listdir(folder)
    files = [f"{folder}/{f}" for f in files if f.endswith(".ipynb")]
    files.sort()
    notebook_files.extend(files)

class TestNotebooks(unittest.TestCase):
    notebooks_to_test = notebook_files
    
    @classmethod
    def setUpClass(cls):
        # Create a PythonExporter to convert notebook into Python code
        cls.converter = nbconvert.PythonExporter()

    def test_notebooks(self):
        # Iterate through the list of notebooks and test each
        for notebook in self.notebooks_to_test:
            with self.subTest(notebook=notebook):
                # Convert the notebook to Python script
                output, _ = self.converter.from_filename(notebook)

                # Save the Python script to a file
                py_file = notebook.replace('.ipynb', '.py')
                with open(py_file, 'w') as f:
                    f.write(output)
                
                # Execute the Python script
                print(f"Executing {notebook}")
                try:
                    subprocess.run(['python', py_file], check=True)
                    os.remove(py_file)
                except Exception as e:
                    self.fail(f"Execution failed for {notebook}: {e}")

if __name__ == "__main__":
    # Run the tests
    unittest.main()
