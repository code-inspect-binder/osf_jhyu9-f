# Executable Environment for OSF Project [jhyu9](https://osf.io/jhyu9/)

This repository was automatically generated as part of a project to test the reproducibility of open science projects hosted on the Open Science Framework (OSF).

**Project Title:** Multilevel Response Surface Analysis

**Project Description:**
> Here you can find R codes and Mplus codes for the simulated illustrative example (Example 1). If there are any bugs, please write us an e-mail. Thanks!

Edits 25-08-2019: 
1. A small change was made to the AnalyzeData-File. Specifically, we have added a control-parameter to the lmer-function call. Please use AnalyzeData_25082019.R from now on.
2. We also changed the Makeplots-file. There was an error in the commands for Figure 2. Thanks to Dr. Xinwen Bai from the Chinese Academy of Sciences for catching it. Please use MakePlots_25082019.R from now on.
3. Finally, we also made a small change to the MLRSA_Utils_RandomEffectMatrix-function in the MultilevelRSA-Source file so that a level 2 variance estimate is estimated even if only one random effect is estimated (in addition to the intercept). Please use the MultilevelRSA_25082019-file from now on.

Addition 30-04-2021:
We are very often asked how one can use the codes to compute a Multilevel-RSA in which one of the congruence variables is a Level 1 predictor and the other is a Level 2 predictor (see Bleidorn et al., 2016). We added a folder to the project that contains R codes for this.  

Edits 23-05-2021:
There was a bug in the MultilevelRSA-function due to a change in the lme4-function. We corrected that. Please use MultilevelRSA_23052021.R from now on.

Edits 24-05-2021:
We improved customizability of the functions that generate the three-dimensional plots. You can now specify various arguments that will be forwarded to the plotRSA function (see help(plotRSA) for a list of possibilities). For example, you can customize the axis ranges with xlim, ylim, and zlim. Please use MultilevelRSA_24052021 from now on. 

**Original OSF Page:** [https://osf.io/jhyu9/](https://osf.io/jhyu9/)

---

**Important Note:** The contents of the `jhyu9_src` folder were cloned from the OSF project on **12-03-2025**. Any changes made to the original OSF project after this date will not be reflected in this repository.

The `DESCRIPTION` file was automatically added to make this project Binder-ready. For more information on how R-based OSF projects are containerized, please refer to the `osf-to-binder` GitHub repository: [https://github.com/Code-Inspect/osf-to-binder](https://github.com/Code-Inspect/osf-to-binder)

## flowR Integration

This version of the repository has the **[flowR Addin](https://github.com/flowr-analysis/rstudio-addin-flowr)** preinstalled. flowR allows visual design and execution of data analysis workflows within RStudio, supporting better reproducibility and modular analysis pipelines.

To use flowR, open the project in RStudio and go to `Addins` > `flowR`.

## How to Launch:

**Launch in your Browser:**

🚀 **MyBinder:** [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/code-inspect-binder/osf_jhyu9-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment in your web browser.
   * Please note that Binder may take a few minutes to build the environment.

🚀 **NFDI JupyterHub:** [![NFDI](https://nfdi-jupyter.de/images/nfdi_badge.svg)](https://hub.nfdi-jupyter.de/r2d/gh/code-inspect-binder/osf_jhyu9-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment on the NFDI JupyterHub platform.

**Access Downloaded Data:**
The downloaded data from the OSF project is located in the `jhyu9_src` folder.

## Run via Docker for Long-Term Reproducibility

In addition to launching this project using Binder or NFDI JupyterHub, you can reproduce the environment locally using Docker. This is especially useful for long-term access, offline use, or high-performance computing environments.

### Pull the Docker Image

```bash
docker pull meet261/repo2docker-jhyu9-f:latest
```

### Launch RStudio Server

Run the container (with a name, e.g. `rstudio-dev`):
```bash
docker run -it --name rstudio-dev --platform linux/amd64 -p 8888:8787 --user root meet261/repo2docker-jhyu9-f bash
```

Inside the container, start RStudio Server with no authentication:
```bash
/usr/lib/rstudio-server/bin/rserver --www-port 8787 --auth-none=1
```

Then, open your browser and go to: [http://localhost:8888](http://localhost:8888)

> **Note:** If you're running the container on a remote server (e.g., via SSH), replace `localhost` with your server's IP address.
> For example: `http://<your-server-ip>:8888`

## Looking for the Base Version?

For the original Binder-ready repository **without flowR**, visit:
[osf_jhyu9](https://github.com/code-inspect-binder/osf_jhyu9)

