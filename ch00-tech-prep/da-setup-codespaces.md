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

The easiest way to start a new Codespace with a pre-made configuration is to go to the root of the repository and click on the `Open in GitHub Codespaces` button for the language of your choice in the README. In the upcoming window, you can either press `Create new codespace` to go with the default settings, or you can press `Change options` to select a different machine type (or change other settings, but you should not bother with them unless you know what you are doing). The types of machines that are available to you depend on the GitHub plan you have.

> [!NOTE]
> This wil create a Codespace from the repository owned by `gabors-data-analysis`, even if you are in a forked repository. To create a Codespace directly from your fork (where you will be able to commit any changes), you should use the set-up detailed below.

Altenatively, you can also go to the root of the repository, click `Code` in the top right corner, then go `Codespaces > Codespaces > ... > New with options > Dev container configuration`. There, you can select whether you want to create a Codespace in Python or R. You can also configure the machine type you want here.

> [!NOTE]
> If you have already set up a Codespace, it can be found under `Code > Codespaces`, where you can resume your session.

Once you have started the set-up, you will see a mostly blank web-based Visual Studio Code editor in your browser. This means that low-level dependencies are currently being installed in the Codespace, which usually takes around 5 minutes. After this is done, the repository structure will appear in your editor, and a `markdown` file will open up detailing the next steps.

> [!IMPORTANT]
> The Codespace is not yet ready at this moment. You must wait until the `postCreateCommand` is finished before trying to run any code.

Shortly, a terminal window will pop-up at the bottom of the screen which tells you that the Codespace is currently running the `postCreateCommand`. This means that the necessary packages are being installed for your selected language. For Python, this usually takes around 1-2 minutes, while for R, it's 8-10 minutes. You can only start coding once this terminal window shows that the `postCreateCommand` has finished!

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

## How do Codespaces work

GitHub Codespaces allow you to open, edit and execute any code in a repository. You can start a basic Codespace from any public repository (but then you need to configure the environment yourself). Therefore, in this repository, we included two presets to start Codespaces: one for Python, the other for R. These configuration files are located in the `.devcontainer` directory (you do not have to do anything in this directory, but if you are interested, you are welcome to check it out).

These configuration files essentially tell GitHub Codespaces what kind of software we need on the virtual machine behind our Codespace instance. These include lower-level dependencies that are needed for executing any code in the selected language (e.g. the programming language and its interpreter), as well as higher-level dependencies (like the specific packages and libraries we need for our analytics work).

When a Codespace instance is started, multiple things happen in the background. First, a so-called `docker` image is pulled from remote container repository. This essentially includes the base for our set-up: for R, we use `ghcr.io/rocker-org/devcontainer/r-ver:4.5`, while for Python, we opt for `mcr.microsoft.com/devcontainers/miniconda:3`. On a high-level, these containers contain the related programming languages and their dependencies. Installing these usually takes around 5 minutes, but this may vary based on GitHub's server availability.

Once these are set-up, the Codespace instance is created - but it does not yet have the coding environments set-up. To set up the enviroments, the configuration files prompt the server to execute a set-up script which creates the environments by downloading and installing the necessary packages to the remote machine. This is the so-called `postCreateCommand`. This usually takes 1-2 minutes for Python and 8-10 minutes for R. After this is done, the environment is ready and you can start coding.