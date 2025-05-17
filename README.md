# VisionEval

VisionEval is a model system and supporting software framework for building collaborative disaggregate strategic
planning models. These models can support scenario planning and a variety of "what-if" analyses.

**NOTE ON WEBSITE**:
The VisionEval website can be found at
[https://VisionEval.github.io](https://VisionEval.github.io)

There are instructions on the website for installing VisionEval from recent releases. We are moving to a new
installation process which greatly simplifies the installation.

The new installation and current version of VisionEval is presently fvound in the [VisionEval-4 repository on
Github](https://github.com/VisionEval/VisionEval-4). See the repository details below, especially if you planning
to do development work on VisionEval.

The `development` branch of the VisionEval repository is the basis for development of new features
and fixes to VisionEval. All new development should be based on the `development` branch. For the
time being (2025 May 17) VisionEval 4 is not open for development as there is not yet a
corresponding branch in [VisionEval-dev](https://github.com/VisionEval/VisionEval-dev). Check back
periodically looking for a "VE-40" branch in VisionEval-dev.

Thanks for your cooperation. If you have questions, please contact info at visioneval.org

## Documentation

Documentation for the VisionEval models is online at
[https://visioneval.github.io/docs](https://visionveal.github.io/docs)

## Release Installation

You can retrieve the current binary release of VisionEval by running the installation script.

Start your selected version of R (using the built-in RGui or RStudio), then copy the following line into your
R console:

```R
source(ve.url<-"https://visioneval.github.io/assets/install/VE4-install.R")
```

You can download the script for security review by visiting [the download
link](https://visioneval.github.io/assets/install/VE4-install.R) in your browser.

The mysterious `ve.url<-` incantation enables some tcltk dialogs that give you flexibility in using
releases other than the most current, or in downloading a snapshot of the full VisionEval code so you
can build the entire system locally. Note that you still want to do a Git clone from VisionEval-dev
if you are planning to contribute your developments back to the main project.

If you want the absolute shortest path to the current VisionEval release, you can just do this:

```R
source("https://visioneval.github.io/assets/install/VE4-install.R")
```

You'll get very few dialogs or confirmations in the latter case: VisionEval will be installed directly
in the R working directory, and you may (if your R supports any interactive dialogs at all)
get a dialog asking where you want to put your VisionEval startup files and models. You can,
if you like, make that the same folder into which you are installing VisionEval, but for backup and
versioning purposes in a production environment, we recommend setting up a separate runtime location.

When the installation is complete, VisionEval will automatically start with your selected runtime
directory as the R working directory. For the time being, the VisionEval functionality that is installed
will be the same as the most recent VisionEval 3.x release.

## Detailed Installation Instructions

The `ve.url<-` method for calling the installation script gives you a variety of choices for
installation.

The first question in the full installation is where you want to put VE_HOME (location for the VisionEval
code library).

If you have an existing VE installation, it is probably safe (but not recommended if
you have existing production models because there may bugs we haven't unearthed yet) to point the
installer's VE_HOME location to your existing VisionEval directory. You can now have multiple R
(and VisionEval) versions hosted in the same home folder, though you are limited to one VisionEval
version per R version - if you want different versions of VisionEval for the same R version, you
should install in separate home directories.

The installation script will then ask what kind of installation you want to do. Unless you're planning
to make changes to the VisionEval core code, you should pick the default installation type, which will
get you one of the pre-built releases. If you have cloned or unzipped a snapshot of VisionEval 4,
you can have two options:

1. Build from Release Code: This method will let you download the full code snapshot of the release
you select and launch the VE-Bootstrap.R script that loads the build environment. You can then
build the entire VisionEval core system from scratch.

2. Build from Local Clone: This method expects you to already have made a "clone" of the
VisionEval-4 repository and it will ask you for the directory where you made that clone. It will
then launch VE-Bootstrap.R to load the build environment.

If you pick the (recommended) Release installation type, the installation script will visit the
VisionEval-4 repository and load a list of available releases and installers for your version of
R. Pre-built installers are available for R 4.4.x and R 4.5.x (the two latest major release lines).
Generally, you should accept the default, then follow the prompts to confirm download and
installation. Currently (2025-May-17) there is only one release. You can try the WindowsLibrary
installer if you like: it downloads a single zip file with all the dependencies - usuually it's
faster and easier to use the standard Windows installer.

The Source installer will build VisionEval from source packages, an intermediate step that
requires less setup than doing a full core build using VE-Bootstrap.R. The catch is that the
dependency packages will also be built from source, and many of them have "SystemRequirements" that
can be hard to satisfy (and require some tedious preparatory work, with many false starts).
We are working on automating the SystemRequirements, which will eventually make installing on
Macintosh or Linux as easy as a Windows installation (though it will require system administrator
rights, which the Windows installation does not).

If you're doing any kind of building from source (or from clones or snapshots of VisionEval-4), you
will need [RTools for your version of R](https://cran.r-project.org/bin/windows/Rtools/).

On Linux or Macintosh, you'll need to ensure you have a development version of R installed, along
with the operation system "devtools" package (including C++ and Fortan compilers and related tools).
Be aware that building and running VisionEval requires a lot of RAM (a minimum of 8 Gigabytes is
recommended). If you're using a Linux cloud server, you'll need something quite a bit more capacious
(and expensive) than what is usually delivered at the "low end". Having multiple CPUs will not speed
up the build process, but when you run models, you can use additional processes to run multiple
model scenarios in parallel. But remember: 8 Gigabytes of RAM for each scenario you're running
simultaneously.

The biggest annoyance (for now) is that as VisionEval and its dependencies build on Linux or
Macintosh, you'll keep getting errors for missing operating system packages that are required by the
various R package dependencies. You'll need to keep using the operating system package manager
(e.g. Ubuntu apt-get) to install them, then start again, crash again, install again, and so on. It
will eventually work, but you might spend quite a while at it.

## VisionEval Repositories

There are five repositories in the VisionEval organization to serve different purposes:

 - **[VisionEval](https://github.com/VisionEval/VisionEval)**: Public release version of VisionEval.
   There is one master branch only. If you have a bug report or other issue, create an issue instead
   in the VisionEval-Dev repository ([here](https://github.com/VisionEval/VisionEval-Dev/issues)).
   **DEPRECATED** Eventually, this existing repository will be renamed and the VisionEval-4 repository
   will be renamed to replace this repository.
 
 - **[VisionEval-Dev](https://github.com/VisionEval/VisionEval-Dev)**: Main repository for
   developers and power-users who want to contribute code improvements. There are multiple branches,
   including `master` (which is can be considered the beta release) and `development` where active code
   development happens. Additional branches can be used to evaluate new features or pull requests.
   Developers / power-users should [create issues](https://github.com/VisionEval/VisionEval-Dev/issues)
   and [pull requests](https://github.com/VisionEval/VisionEval-Dev/pulls) to this repository.
   **RESTRUCTURED** in VisionEval 4.0: the VE-40 branch will initially be released (it's not there
   yet, as of 2025-May-17), followed by a change in branch names where "development" becomes "VE-3" and "VE-40"
   becomes "development". Future work should be based on VE-40/development.

 - **[VisionEval-4](https://github.com/VisionEval/VisionEval-4)**: This repository holds releases of
   the VisionEval software to support the new VisionEval 4 installation process (and eventually,
   updated code with many cool new features). Though you can fork or clone it, it is unwise to do so
   at this time (2025-May-17) because the repository will be force-updated without history as bugs
   in the initial release are fixed. You should only access it through the installation script
   described above. Development history for VE-4 will soon be added to VisionEval-dev and that will
   continue to be the repository that should be used for new development.
 
 - **[VisionEval.github.io](https://github.com/VisionEval/VisionEval.github.io)**: Website repository. You can
   [create issues](https://github.com/VisionEval/VisionEval.org/issues) here for website-related change
   requests.
 
 - **[VisionEval-Docs](https://github.com/VisionEval/VisionEval-Docs)**: Documentation respository.
   Changes checked into the main branch here will be auto-posted to the website (if you are user
   with permission to push to those repositories).

 - **[VisionEval-Extras](https://github.com/VisionEval/VisionEval-Extras)**: Additional tools and
   module packages that may be added to core VisionEval.
   **IMPROVED** in VisionEval 4.0: The "Extras" packages can be built into a standard VisionEval
   installation with a simple configuration change. You can add them to an end-user installation
   without having to rebuild the rest of VisionEval. See the detailed instructions above.

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
   You will also need [RTools](https://cran.r-project.org/bin/windows/Rtools/) if you are building on
   Windows. Currently, it is possible to build from source on Mac or Linux, but you will need to load
   all the SystemRequirements into your operating system. That can be annoying and tedious. We will fix
   that soon so the SytemRequirements can be auto-installed (if you have administrator/sudo permissions)
   or at least give you an installation instruction (so your authorized administrator can install them).
   Keep an eye out for future releases in June 2025.
2. Start `VisionEval-dev.Rproj` in the root directory, or you can use launch.bat. To use `launch.bat`,
   you will need to set the R_HOME system or user environment variable. Open the R you want to use
   and run `R.home()` to get the directory you need. The build will work with any recent R version
   (ideally in the 4.3 or 4.4 series of R releases).
3. Run ve.build() to construct the packages
4. Run ve.run() to launch the runtime (note that the built "runtime" is only used indirectly)
    1. VisionEval runs in the new "runtime.test" directory
    2. You can set a directory of your choice selected either by passing it as a parameter
       (`ve.run('myRuntimeDirectory")`) or by setting the VE_RUNTIME environment variable either
       as a system or user environment variable, or by defining it in the `.Renviron` file that
       is created in the repository root when you run `ve.build()`.
       A complete working runtime will be created in VE_RUNTIME if it does not already exist
5. Once running, do `walkthrough()` (with no parameters) to get a list of
   sample scripts illustrating basic functions (all to run in an additional temporary runtime to
   avoid confusing them with real work).
    1. `walkthrough()` is also available for ordinary users in the distributed runtime
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
