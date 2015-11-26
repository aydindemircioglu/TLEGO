
# TLEGO

TLEGO is a simple method to apply the idea of time limited training to
 model selection via EGO. This repository contains the experiments
  that show that this simple
 combination can yield a very fast model selection.


## General Prerequisites

This package is written in R. While it should be possible to execute the whole experiment
on any platform on which R and the SVM solvers are available, it is tested and written only
for Linux. Other platforms will need (heavy?) adaptations.

To get the experiments running, you need to to do the following things:
- install all R packages (including the SVMBridge),
- load and split the data sets,
- compile the software
- execute the experiment.



## Prerequisites

#### R packages

Apart from standard packages you can get from CRAN, you will need the latest
github packages for the following packes: ```mlr, mlrMBO, parallelMap, ParamHelpers```.
You can install these via devtools and then do

```R
# make sure we have the latest devtools
library(devtools)
devtools::install_github("hadley/devtools")

# install latest packages from github
devtools::install_github("berndbischl/ParamHelpers")
devtools::install_github("mlr-org/mlr")
devtools::install_github("berndbischl/mlrMBO")
devtools::install_github("berndbischl/parallelMap")
```

To benefit from the parallel implementation of EGO, you must configure mlr/mlrMBO.
Please refer to the documentation of the packages to find out how.
One way is to create the file ```~/.BatchJobs.R``` and put into it a line like
```cluster.functions <- makeClusterFunctionsMulticore(ncpus = 16)```.

Another prerequisite is the ```SVMBridge``` package. Unluckily this is currently
 in development, so it might be risky to use it.  You can find it at
 https://github.com/aydindemircioglu/SVMBridge.


#### SVM packages

We included all the sources of the modified SVM solvers. Each of them has
additional parameters to  control for the wall time and also for saving
intermediate models. To see how it works, please look into the
corresponding wrapper in the solvers directory. In general, there is one extra
parameter (usually "-L") that allows for setting the wall time. Then there are
two extra parameter that allow for saving intermediate models ('snapshots').

#### SVM solvers

Before you can start the experiment, you must first compile all the solvers.
Currently we only support Linux, though for other platforms this should
work roughly the same (but will for sure need adaptations). To build the
software, do the following:

```bash
cd ./software
mkdir build
cd build
cmake ..
make -j4 all
```

This will build all software in a separate directory and after building the
script will
copy all binaries into the respective binary directory, e.g. the binary for LIBSVM,
```svm-train``` will be copied to ```./software/LIBSVM/bin```. You must make
sure that you have permissions to write to the software directory.


#### Data sets

Nearly all data sets are free (except for arthrosis and spektren) and can be found on https://bitbucket.org/aydin_demircioglu/datasets.
To split these into train/validation/test, you need to use the ```helpers/splitDatasets.R``` script (Adapt it, if necessary, it usually will expect the data sets at "./datasets"
and will create "./splits").
Notice that this script already needs BatchExperiments to be installed, as it will
generate 10 different splits. **Be aware that the data sets themselves take roughly 1,5 GB,
and all splits take around 12 GB!**



## Starting the experiment

The main script is 'startExperiment.R'. There are several options at the beginning of
the file you might want to change, e.g. the path where the data set splits can be found.

After adapting, you can just execute the experiment via

```
$ Rscript ./startExperiment.R
```
**Make sure that you have enough free space on your harddrive!** If you execute
the whole experiment, this will take nearly 20 gigabytes on your hard drive, as
all intermediate EGO models will be saved. The whole experiment will take also
some time, roughly 4-5 days on our machine (roughly 1 day for the model selection
  and 3-4 days for retraining a model with the best parameters found).


## Output and Results

All results will be put into the results directory (defaults to "./results").
The intermediate output (all the EGO models and retrained models, caches etc)
will be put into "./output".

For convenience we already put our results into the results directory.
Notice that your results might differ as the stopping heuristic might be
different. Our results were computed on a workstation with two Xeon CPUs
and 64 GB of RAM.


#### Outputs

**EGO-Search**:
For each combination of data set, replication and solver, the script creates an own
BatchExperiment registry, e.g. "LASVM_poker_1".
The computed models will be put under ```./output/models``` and follow a similar
naming scheme, e.g. the models for LASVM on the first replication of poker
will be put into "models/LASVM/poker_1".
After all computations, a cache file is generated to avoid recomputation, called
"EGOcache.RData".

**Retraining**:
After the best point is found, for each data  set, replication and solver combination
a full model is trained. For this a registry called "./output/retrainingRegistry" will
be created. The models will be put under the registry directory named with the job
number, e.g. "./output/retrainingRegistry/models/21/". Caching will be done via
the file "retrainingcache.RData".

**Final Validation**:
The full model needs to be evaluated on the test set. Again this is done in parallel.
The registry "finalTestRegistry" is created for this purpose. Caching will be done
via the file "finalTestcache.RData".



#### Results

Several result files will be produced. If you need direct access to the data,
you can simply load the "finalData.RData" file in the output directory.
A plot with the accuracy results will be produced as well as a plot with
the timings relative to the ParEGO experiment.


### Feedback

If you have any kind of feedback, just get in touch with us.
