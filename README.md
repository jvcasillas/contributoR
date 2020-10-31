
## contributoR

An R package for documenting scholarly contributions.

Last update: 2020-10-31

### Overview

The goal of contributoR is to facilitate the documentation of scholarly
contributions. This package uses [CRediT](https://casrai.org/credit/)
(Contributor Roles Taxonomy) to visualize contributor roles.

Specifically, CRediT is a “high-level taxonomy, including 14 roles, that
can be used to represent the roles typically played by contributors to
scientific scholarly output. The roles describe each contributor’s
specific contribution to the scholarly output” (see website for more
details: <https://casrai.org/credit/>).

There are 14 roles, which are defined as follows:

1.  **Conceptualization**: Ideas; formulation or evolution of
    overarching research goals and aims.
2.  **Data curation**: Management activities to annotate (produce
    metadata), scrub data and maintain research data (including software
    code, where it is necessary for interpreting the data itself) for
    initial use and later re-use.
3.  **Formal analysis**: Application of statistical, mathematical,
    computational, or other formal techniques to analyze or synthesize
    study data.
4.  **Funding acquisition**: Acquisition of the financial support for
    the project leading to this publication.
5.  **Investigation**: Conducting a research and investigation process,
    specifically performing the experiments, or data/evidence
    collection.
6.  **Methodology**: Development or design of methodology; creation of
    models.
7.  **Project administration** – Management and coordination
    responsibility for the research activity planning and execution.
8.  **Resources**: Provision of study materials, reagents, materials,
    patients, laboratory samples, animals, instrumentation, computing
    resources, or other analysis tools.
9.  **Software**: Programming, software development; designing computer
    programs; implementation of the computer code and supporting
    algorithms; testing of existing code components.
10. **Supervision**: Oversight and leadership responsibility for the
    research activity planning and execution, including mentorship
    external to the core team.
11. **Validation**: Verification, whether as a part of the activity or
    separate, of the overall replication/reproducibility of
    results/experiments and other research outputs.
12. **Visualization**: Preparation, creation and/or presentation of the
    published work, specifically visualization/data presentation.
13. **Writing – original draft**: Preparation, creation and/or
    presentation of the published work, specifically writing the initial
    draft (including substantive translation).
14. **Writing – review & editing**: Preparation, creation and/or
    presentation of the published work by those from the original
    research group, specifically critical review, commentary or revision
    – including pre- or post-publication stages.

### Installation

You can install the development version from GitHub with:

    install.packages("devtools")
    devtools::install_github("jvcasillas/contributoR")

### Usage

To create a `CRediT` visualization you need to use the `contributor`
function. This function takes a single argument, a list, which assigns
any of the 14 contributor roles to an individual. In other words, the
name of the list element is the name of the contributor and any numbers
included for that individual correspond with 1-14 in the list of roles.
For example…

``` r
library("contributoR")

# Create example list
ex_1 <- list(
 JVC = 1:13,
 JGP = 1,
 NR  = c(2, 4))

# Plot contributions
contributor(contributions = ex_1)
```

<img src='https://raw.githubusercontent.com/jvcasillas/contributoR/master/README_files/figure-gfm/example1-1.png' align='center' width='800px'/>

You can also build the list directly inside the `contributor` function.

``` r
# Build list inside function
contributor(
 contributions = list(
   "Author 1" = seq(1, 14, by = 3),
   "Author 2" = c(1, 3, 5, 7, 9, 13),
   "Lazy person" = NA,
   "Author 3" = c(2, 4, 8, 14), 
   "Advisor" = 12)
   )
```

<img src='https://raw.githubusercontent.com/jvcasillas/contributoR/master/README_files/figure-gfm/example1-2.png' align='center' width='800px'/>
