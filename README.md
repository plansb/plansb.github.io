[![R build status](https://github.com/plansb/plansb.github.io/workflows/R-CMD-check/badge.svg)](https://github.com/plansb/plansb.github.io/actions)

# sb
Revitalize Santa Barbara - 2020 Charrette for State St

## Workflow

1. Submit a Proposal
    - Use a Google Form to submit content into a Google Sheet
1. Update Website
    - Using a Github Action, we spin up a server with programming languge (R) to read in the Google Sheet and render the website with updated information.
1. View Website

## Next Steps

- Map interface
    - two locations: 
        - Focus Areas (polygons): click to get popup list of proposals with tiny thumbnail. Click again on proposal to go to individual proposal.
        - Addresses (points): color code by Type of Vision.
- Proposal page

## Automatically run

Setup Github Action:

```R
# ghactions - not working
remotes::install_github("maxheld83/ghactions")
library(ghactions)
use_ghactions(workflow = website())


# usethis dependency failing
remotes::install_github("r-lib/gert")
```

Configuration failed to find libgit2 library. Try installing:
 * brew: libgit2 (MacOS)
 * deb: libgit2-dev (Debian, Ubuntu, etc)
 * rpm: libgit2-devel (Fedora, CentOS, RHEL)
 
 
If libgit2 is already installed, check that 'pkg-config' is in your
PATH and PKG_CONFIG_PATH contains a libgit2.pc file. If pkg-config
is unavailable you can set INCLUDE_DIR and LIB_DIR manually via:
R CMD INSTALL --configure-vars='INCLUDE_DIR=... LIB_DIR=...'
-------------------------- [ERROR MESSAGE] ---------------------------
<stdin>:1:10: fatal error: 'git2.h' file not found

```bash
brew install libgit2
```

```
remotes::install_github("r-lib/gert")

remotes::install_github("r-lib/usethis")

usethis:::use_github_action('blogdown.yaml')
usethis:::use_github_action('check-release.yaml')


usethis::use_github_action_check_release()
```

✓ Adding '^\\.github$' to '.Rbuildignore'
✓ Adding '*.html' to '.github/.gitignore'
✓ Writing '.github/workflows/R-CMD-check.yaml'
● Copy and paste the following lines into '/Users/bbest/github/plansb.github.io/README.md':
  <!-- badges: start -->
  [![R build status](https://github.com/plansb/plansb.github.io/workflows/R-CMD-check/badge.svg)](https://github.com/plansb/plansb.github.io/actions)
  <!-- badges: end -->
  [Copied to clipboard]

```r
usethis::use_github_action_check_full()
More details are in chapter 2.

There are a range of other R actions available in the r-lib library. You can add these example yaml files using the following function (demonstrated here with the check-release action):



```    

```
✓ Setting active project to '/Users/bbest/github/plansb.github.io'
✓ Creating '.github/workflows/'
✓ Adding '^\\.github/workflows$' to '.Rbuildignore'
✓ GitHub actions is set up and ready to go.
● Commit and push the changes.
● Visit the actions tab of your repository on github.com to check the results.
```