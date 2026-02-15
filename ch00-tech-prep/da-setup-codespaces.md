# How to set up your environment in GitHub Codespaces to do Data Analysis

Setting up your environment locally for the first time may be daunting for less tech-savvy people.
While we do believe that ultimately, you should set-up your local environment as well, we understand that you want to 
embark on your data analysis journey right away. For this reason, this repository comes with pre-configured GitHub Codespaces 
which ensure that you can quickstart your analytics journey in no time. Currently, GitHub Codespaces configuration is available for Python and R.

## Why should you use Codespaces as a student?

As a student, you have access to a [GitHub Education account](https://education.github.com/pack) (registration required), which - among other perks - provides generous access to GitHub Codespaces. Codespaces are essentially virtual machines running on a remote server maintained by GitHub, where you can work in a web-based Visual Studio Code environment. Thus, the code is not running on your local machine, but rather on a remote server, which can be especially useful later in the coursework when you will have to train such models which are very demanding for lower-tier hardware.

Using Codespaces also spares you the inevitable headache that comes with trying to set-up your local environment for the first time, be it Python or R. Using our pre-configured Codespaces, you can set-up your environment with just a click of a button (and some waiting) (details later). You do not have to worry about which versions and dependencies you should install or writing lengthy terminal commands to set things up: the Codespace automatically installs the recommended environment for running the codebase.

In addition, Codespaces integrate closely into the broader GitHub ecosystem: you can share your current workspace with others and work interactively using the [Live Share](https://docs.github.com/en/codespaces/developing-in-a-codespace/working-collaboratively-in-a-codespace) functionality, and if you have forked the repository, you can commit your changes or any new code you have written in Codespaces.

## Why should you tell students to use Codespaces as an instructor?

Any course including coding must start with a session to set-up the correct environments for each student. This can take valuable course time, and even then, it may happen later that something works on one laptop, while not on an other, which leads to lengthy debugging sessions to figure out the issues in the environments.

Using the pre-configured GitHub Codespaces lets you bypass these issues to concentrate on actual analysis. Students can set-up the correct environments in Codespaces with just a click of a button (details later) to guarantee that everyone uses the exact same environment.

In addition, students can [Live Share](https://docs.github.com/en/codespaces/developing-in-a-codespace/working-collaboratively-in-a-codespace) their work with the instructors as well, helping debugging on the fly or highlighting good solutions to everyone.

## Setting up your Codespace

The easiest way to start a new Codespace with a pre-made configuration is to go to the root of the repository and click on the `Open in GitHub Codespaces` button for the language of your choice in the README. In the upcoming window, you can either press `Create new codespace` to go with the default settings, or you can press `Change options` to select a different machine type (or change other settings - generally, you should not bother with them unless you know what you are doing). The types of machines that are available to you depend on the GitHub plan you have.

> [!NOTE]
> This wil create a Codespace from the repository owned by `gabors-data-analysis`, even if you are in a forked repository. To create a Codespace directly from your fork (where you will be able to commit any changes), you should use the set-up detailed below.

Altenatively, you can also go to the root of the repository, click `Code` in the top right corner, then go `Codespaces > Codespaces > ... > New with options > Dev container configuration`. There, you can select whether you want to create a Codespace in Python or R. You can also configure the machine type you want here.

> [!NOTE]
> If you have already set up a Codespace, it can be found under `Code > Codespaces`, where you can resume (or delete) your session. Clicking the buttons in the README will also prompt you to resume your corresponding Codespace if you have already started one.

Once you have started the set-up, you will see a mostly blank web-based Visual Studio Code editor in your browser. This means that low-level dependencies as well as the coding environment are currently being installed in the Codespace, which usually takes around 5-10 minutes. After this is done, the repository structure will appear in your editor, and a `markdown` file will open up detailing the next steps.

At this point, the environment is fully set-up and you could start coding. However, if you plan to use your Codespace in the long run, we recommend that you download the data files as well. To do so, paste the following command in the terminal than hit Enter: `bash .devcontainer/scripts/download-data.sh`.

To actually be able to run the codes the set-up differs between the languages:

### Python

- Open any Jupyter notebook you want to run. Click the *Select kernel* button in the top right corner.
- Select the pre-configured `daenv` Python environment.
- You can now run codecells in the notebook by clicking in it and pressing `Ctrl/Cmd + Enter`.

### R

- Start an interactive R terminal. To do so, press `Ctrl/Cmd+Shift+P` to open up the Visual Studio Code command palette. Search for *R: Create R Terminal* and click on the command. A new R terminal will be started for you.
- Open any R code you want to run.
- You can run the line where your cursor is at or the currently selected codechunk by pressing `Ctrl/Cmd+Enter`

## If things don't work out

While the set-up should work *in theory* every time, errors can happen. These are usually related to one of the services the configuration tries to use being down. These includes the Codespace servers, the remote container repositories and the language specific package repositories. Such errors are rather rare, and deleting the running Codespace and trying again usually solves the issues.

## How do Codespaces work

GitHub Codespaces allow you to open, edit and execute any code in a repository. You can start a basic Codespace from any public repository (but then you need to configure the environment yourself). Therefore, in this repository, we included two presets to start Codespaces: one for Python, the other for R. These configuration files are located in the `.devcontainer` directory (you do not have to do anything in this directory, but if you are interested, you are welcome to check it out).

These configuration files essentially tell GitHub Codespaces what kind of software we need on the virtual machine behind our Codespace instance. These include lower-level dependencies that are needed for executing any code in the selected language (e.g. the programming language and its interpreter), as well as higher-level dependencies (like the specific packages and libraries we need for our analytics work).

The configuration in our case consists of two components: a `Dockerfile` and a `devcontainer.json`. The former serves as our base, as it contains the exact containers to pull from a remote container repository. These essentially include the core for our set-up: for R, we use `ghcr.io/rocker-org/devcontainer/r-ver:4.5`, while for Python, we opt for `mcr.microsoft.com/devcontainers/miniconda:3`. On a high-level, these containers include the related programming languages and their dependencies. The `Dockerfile` also includes a set of additional commands we want the Codespace to run when it builds our environment. These commands depend on the language we want to set-up the environment for, and they are reliable for creating the correct environment for you. Executing all the commands in our `Dockerfile`s take a long time - thus, even if it would be possible to execute them (that is, to build our Docker image) when a Codespace is started, it would be painfully slow. Therefore, we have opted for another approach, which is to build the images only once when one of our environment definitions changes, and store the resulting image on GitHub Container Repository. We can then load the pre-configured images from here when a Codespace is started. 

> [!NOTE]
> While understanding how a `Dockerfile` works is not a must for every data professional, it is certainly a nice-to-have in most cases. You can read more about it [here](https://docs.docker.com/reference/dockerfile/).

The `devcontainer.json` acts as the main orchestrator for configuring the Codespace. It starts with pulling the environment specified in the `Dockerfile` from GitHub Container Repository. In addition, it also prompts the Codespace to install certain useful additional features (like CLI Git and GitHub interfaces) and configure Visual Studio Code with some extensions needed for the specific languages. You can read more about `devcontainer.json` files [here](https://docs.github.com/en/codespaces/setting-up-your-project-for-codespaces/adding-a-dev-container-configuration/introduction-to-dev-containers).