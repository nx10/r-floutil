# floutil

Collection of commonly used helper functions for project management: Initialize, distribute and some helper function.

## Installation

```R
install.packages("devtools") # if not already installed
devtools::install_github("nx10/r-floutil")
```


## Usage

This package is not intended to be included in a project using `library(...)`. It is best called directly with `floutil::some_function()`. 

### Initialize project

```R
floutil::init()
```

Initialize project directory with default folders, scripts and .gitignore file.

### Build + distribute project

```R
floutil::build()
```

Pack relevant files in a timestamped zip archive for distribution. Ignores user specific files like `.Rproj` and `.Rhistory`.

### Compare ID columns in different datasets

```R
floutil::matchinfo(idcol1, idcol2)
```

Finds and displays duplicate IDs as well as IDs missing in either column.

### List used packages

Finds all used dependencies, will work recursively on `source("other_script.R")`.

```R
floutil::report_dependencies("script.R")
```