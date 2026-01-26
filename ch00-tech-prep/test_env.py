# python ch00-tech-prep/test_env.py

import unittest
import nbconvert
import os
import subprocess

NOTEBOOKS = [
    "ch01-hotels-data-collect/ch01-hotels-data-collect.ipynb",
    "ch02-football-manager-success/ch02-football-manager-success.ipynb",
    "ch02-hotels-data-prep/ch02-hotels-data-prep.ipynb",
    "ch02-immunization-crosscountry/ch02-immunization-crosscountry.ipynb",
    "ch03-city-size-japan/ch03-city-size-Japan.ipynb",
    "ch03-distributions-height-income/ch03-height-income.ipynb",
    "ch03-football-home-advantage/ch03-football-home-advantage-describe.ipynb",
    "ch03-hotels-europe-compare/ch03-hotels-europe-compare.ipynb",
    "ch03-hotels-vienna-explore/ch03-hotels-vienna-explore.ipynb",
    "ch03-simulations/ch03-distributions.ipynb",
    "ch04-management-firm-size/ch04-wms-management-size.ipynb",
    "ch05-stock-market-loss-generalize/ch05-stock-market-loss-generalize.ipynb",
    "ch06-online-offline-price-test/ch06-online-offline-price-test.ipynb",
    "ch06-stock-market-loss-test/ch06-stock-market-loss-test.ipynb",
    "ch07-hotels-simple-reg/ch07-hotels-simple-reg.ipynb",
    "ch07-ols-simulation/ch07-ols-simulation.ipynb",
    "ch08-hotels-measurement-error/ch08-hotels-measeerror.ipynb",
    "ch08-hotels-nonlinear/ch08-hotels-nonlinear-reg.ipynb",
    "ch08-life-expectancy-income/ch08-life-expectancy-income.ipynb",
    "ch09-gender-age-earnings/ch09-earnings-inference.ipynb",
    "ch09-hotels-europe-stability/ch09-hotels-externalvalid.ipynb",
    "ch10-gender-earnings-understand/ch10-gender-earnings-multireg.ipynb",
    "ch10-hotels-multiple-reg/ch10-hotels-multiple-reg.ipynb",
    "ch11-australia-rainfall-predict/ch11-australia-rainfall-predict.ipynb",
    "ch11-smoking-health-risk/ch11-smoking-health-risk.ipynb",
    "ch12-electricity-temperature/ch12-arizona-electricity.ipynb",
    "ch12-stock-returns-risk/ch12-stock-returns-risk.ipynb",
    "ch12-time-series-simulations/ch12-randomwalk-serialcorr-simul.ipynb",
    "ch13-used-cars-reg/ch13-used-cars.ipynb",
    "ch14-airbnb-reg/ch14-airbnb-prepare.ipynb",
    "ch14-airbnb-reg/ch14-airbnb-prediction.ipynb",
    "ch14-used-cars-log/ch14-used-cars-log.ipynb",
    "ch15-used-cars-cart/ch15-used-cars-cart.ipynb",
    "ch16-airbnb-random-forest/ch16-airbnb-prepare-london.ipynb",
    # "ch16-airbnb-random-forest/ch16-airbnb-random-forest.ipynb",
    "ch17-predicting-firm-exit/ch17-firm-exit-data-prep.ipynb",
    # "ch17-predicting-firm-exit/ch17-predicting-firm-exit.ipynb",
    "ch18-case-shiller-la/ch18-ts-pred-homeprices.ipynb",
    "ch18-swimmingpool/ch18-swimmingpool-predict.ipynb",
    "ch19-food-health/ch19-food-health.ipynb",
    "ch19-food-health/ch19_food-health-maker.ipynb",
    "ch20-ab-test-social-media/ch20-ab-test-powercalc-pvalues.ipynb",
    "ch20-working-from-home/ch20-wfh.ipynb",
    "ch21-ownership-management-quality/ch21-wms-01-dataprep.ipynb",
    "ch21-ownership-management-quality/ch21-wms-02-analysis.ipynb",
    "ch22-airline-merger-prices/ch22-airlines-01-dataprep.ipynb",
    "ch22-airline-merger-prices/ch22-airlines-02-analysis.ipynb",
    "ch23-immunization-life/ch23-immunization-life.ipynb",
    "ch23-import-demand-and-production/ch23-asia-ip-imports.ipynb",
    "ch24-football-manager-replace/ch24-football-manager-replace.ipynb",
    "ch24-haiti-earthquake-gdp/ch24-haiti-earthquake-gdp.ipynb",
    "da_illustrations/da_illustration_plots.ipynb",
]


class TestNotebooks(unittest.TestCase):
    notebooks_to_test = NOTEBOOKS

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
                py_file = notebook.replace(".ipynb", ".py")
                with open(py_file, "w") as f:
                    f.write(output)

                # Execute the Python script
                print(f"Executing {notebook}")
                env = os.environ.copy()
                env["MPLBACKEND"] = "Agg"
                
                # Add ch00-tech-prep to PYTHONPATH so notebooks can find py_helper_functions
                repo_root = os.getcwd()
                tech_prep_path = os.path.join(repo_root, "ch00-tech-prep")
                if "PYTHONPATH" in env:
                    env["PYTHONPATH"] = f"{tech_prep_path}{os.pathsep}{env['PYTHONPATH']}"
                else:
                    env["PYTHONPATH"] = tech_prep_path
                try:
                    result = subprocess.run(
                        ["python", py_file],
                        check=True,
                        env=env,
                        capture_output=True,
                        text=True,
                    )
                    os.remove(py_file)
                except subprocess.CalledProcessError as e:
                    print(f"\n{'='*60}")
                    print(f"FAILED: {notebook}")
                    print(f"{'='*60}")
                    if e.stdout:
                        print(f"STDOUT:\n{e.stdout}")
                    if e.stderr:
                        print(f"STDERR:\n{e.stderr}")
                    print(f"{'='*60}\n")
                    
                    # Save error log to file for artifact upload
                    log_file = py_file.replace(".py", ".log")
                    with open(log_file, "w") as f:
                        f.write(f"FAILED: {notebook}\n")
                        f.write(f"{'='*60}\n")
                        f.write("STDOUT:\n")
                        f.write(e.stdout or "")
                        f.write(f"\n{'='*60}\n")
                        f.write("STDERR:\n")
                        f.write(e.stderr or "")
                    
                    self.fail(f"Execution failed for {notebook}: {e.stderr[:500] if e.stderr else str(e)}")
                except Exception as e:
                    self.fail(f"Execution failed for {notebook}: {e}")

    @classmethod
    def tearDownClass(cls):
        files = [
            "case-shiller-workfile-2000-2017.pkl",
            "case-shiller-workfile-2000-2018.pkl",
            "usedcars_work.csv",
            "ch11_share.csv",
            "airbnb_hackney_work.csv",
        ]
        for file in files:
            if os.path.exists(file):
                os.remove(file)


if __name__ == "__main__":
    # Run the tests
    unittest.main()
