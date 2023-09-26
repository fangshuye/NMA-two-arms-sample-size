# OssaNMA: An R package for using information from network meta-analyses to optimize the power and sample allocation of a new two-arm trial

- R Shiny app:
  - Website: https://fangshu.shinyapps.io/CalSampleSize/
  - Sample Dataset: sampledat.csv (https://github.com/fangshuye/NMA-two-arms-sample-size/blob/main/sampledat.csv)
    - a sample dataset for investigators to refer to when using the shiny app to calculate the sample size, please upload your dataset using the same format as sampledat.csv
    - This dataset contains 5 columns with each column meaning:
        - studlab: study id
        - treat1: name of treatment 1
        - treat2: name of treatment 2
        - TE: treatment effect size (log odds ratio) between treat1 and treat2
        - seTE: standard error of the estimated TE

- R package
  - https://CRAN.R-project.org/package=OssaNMA
  - https://github.com/fangshuye/OssaNMA
 

## replication code

### Folder R Shiny

File `app.R` runs the [R Shiny app]<https://fangshu.shinyapps.io/CalSampleSize/>

### Folder Package Example

File `code.R` replicates all the code in Section 6: The package and Section 7: Empirical illustrations

### Folder Figure

File `run.R` generates Figure 3. After running it, the output is saved in `Figure/plot`. 

### Folder Simulation

#### Folder Example Replication

See README.md in Folder Example Replication to generate similar result as Table 1 and Table 2 within reasonable time.

#### Folder Full Replication

See README.md in Folder Full Replication to replicate Table 1 and Table 2. It takes around one and a half hours to run on [Pronto Cluster]<https://researchit.las.iastate.edu/guides/pronto/>.

