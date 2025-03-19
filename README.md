# VisionEval

VisionEval is a model system and supporting software framework for building collaborative disaggregate strategic
planning models. These models can support scenario planning and a variety of "what-if" analyses.

**NOTE ON WEBSITE**:
The VisionEval website can be found at
[https://VisionEval.github.io](https://VisionEval.github.io)

There are instructions on the website for installing VisionEval.

The `development` branch of the VisionEval repository is the basis for development of new features and fixes to
VisionEval. All new development should be based on the `development` branch.

Thanks for your cooperation. If you have questions, please contact info at visioneval.org

## Documentation

Documentation for VisionEval is online at
[https://visioneval.github.io/docs](https://visionveal.github.io/docs)

## Release

You can retrieve the current binary release of VisionEval by running the installation script.

Start your selected version of R (using the built-in RGui or RStudio), then copy the following line into your
R console:

```R
source("https://visioneval.github.io/assets/install/VE4-install.R")
```

You can download the script for security review by visiting [the download
link](https://visioneval.github.io/assets/install/VE4-install.R) in your browser.

## VisionEval Repositories

There are five repositories in the VisionEval organization to serve different purposes:

 - **[VisionEval](https://github.com/VisionEval/VisionEval)**: Public release version of VisionEval.
   There is one master branch only. If you have a bug report or other issue, create an issue instead
   in the VisionEval-Dev repository ([here](https://github.com/VisionEval/VisionEval-Dev/issues)).
   **DEPRECATED** in VisionEval 4.0: this repository will be changed to contain a smaller set of scripts
   and related information for installing VisionEval. The system code will only reside in VisionEval-dev.
 
 - **[VisionEval-Dev](https://github.com/VisionEval/VisionEval-Dev)**: Main repository for
   developers and power-users who want to contribute code improvements. There are multiple branches,
   including `master` (which is can be considered the beta release) and `development` where active code
   development happens. Additional branches can be used to evaluate new features or pull requests.
   Developers / power-users should [create issues](https://github.com/VisionEval/VisionEval-Dev/issues)
   and [pull requests](https://github.com/VisionEval/VisionEval-Dev/pulls) to this repository.
   **RESTRUCTURED** in VisionEval 4.0: the VE-4.0 branch will initially be released, followed by a
   shift where "development" becomes "VE-3.0" and "VE-4.0" becomes "development". Future work should
   be based on VE-4.0.
 
 - **[VisionEval.github.io](https://github.com/VisionEval/VisionEval.github.io)**: Website repository. You can
   [create issues](https://github.com/VisionEval/VisionEval.org/issues) here for website-related change
   requests.
 
 - **[VisionEval-Docs](https://github.com/VisionEval/VisionEval-Docs)**: Documentation respository.
   Changes checked into the main branch here will be auto-posted to the website. 

 - **[VisionEval-Extras](https://github.com/VisionEval/VisionEval-Extras)**: Additional tools and
   module packages that may be added to core VisionEval.

   **IMPROVED** in VisionEval 4.0: These packages can be built into a standard VisionEval
   installation with a simple configuration change. You can add them to an end-user installation
   without having to rebuild the rest of VisionEval.

## Issues

Please submit issues, bugs, or feature requests about VisionEval on the
[VisionEval-Dev issues page](https://github.com/VisionEval/VisionEval-Dev/issues). 

Please submit issues or content change requests about the VisionEval.org website on the
[VisionEval.org issues page](https://github.com/VisionEval/VisionEval.org/issues).

## For Developers: Building 

To modify and rebuild the released VisionEval system, you can clone a suitable branch
(either "main" or "development") from the "development" repository:
[VisionEval-Dev repository](https://github.com/VisionEval/VisionEval-dev). 

Here are the build steps:

1. Clone the Github
2. Start `VisionEval-dev.Rproj` in the root directory, or you can use `launch.bat` to start the
   standard R GUI. You do *NOT* need RStudio to build or run VisionEVal, just a compatible
   version of R. If you use `launch.bat`, you will need to set the R_HOME environment variable
   or edit the script itself to point at your version of R. Supported R versions are listed in
   `build/R-versions.yml`.
3. Run ve.build() to construct the packages
4. Run ve.run() to launch the runtime (note that the built "runtime" is only used indirectly)
    1. VisionEval runs in the new "runtime.test" directory
    2. You can set a directory of your choice selected either by passing it as a parameter
       (`ve.run('myRuntimeDirectory")`) or by setting the VE_RUNTIME environment variable either
       as a system or user environment variable, or by defining it in the `.Renviron` file that
       is created in the repository root when you run `ve.build()`.
       A complete working runtime will be created in VE_RUNTIME if it does not already exist
5. Once running, do `walkthrough()` or run `ve.test()` (with no parameters) to get a list of
   sample scripts illustrating basic functions (all to run in an additional temporary runtime to
   avoid confusing them with real work).
    1. `walkthrough()` is also available for ordinary users in the distributed runtime
    2. Run `ve.test("VEModel")` to load more detailed API test functions (a comprehensive exercise of what works and how).
    3. The walkthrough function creates a temporary runtime directory (to avoid trampling any real models you
       may have). Run `exit.walkthrough()` (or quit and restart the R session) to return to regular VE_RUNTIME

## For Developers: Submitting changes or bug requests

If you intend to submit changes back to the VisionEval project, please clone the [VisionEval-Dev
repository, `development` branch](https://github.com/VisionEval/VisionEval-Dev/tree/development).
Pull requests against this branch are welcome (but make sure you have rebased the pull request on
the current HEAD of `development`).

Pre-built binary installers of recently released versions (the "main" branch) are available at
[https://visioneval.org](https://visioneval.org) and as "releases" in the `development` branch of
`VisionEval-dev`.

You can install the directly from a copy (.zip) or clone of this VisionEval repository branch, using
the instructions in the `build/Building.md` file in the repository, or in the detailed installation
instructions at [https://visioneval.github.io/docs](https://visioneval.github.io/docs)

You do *NOT* need to fork the repository unless you are planning to submit changes (pull requests)
back to the VisionEval project.
