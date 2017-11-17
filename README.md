# latently

An R package to visualize latent factors

## Install

1. Get yourself a personal auth token for github. Go to https://github.com/settings/tokens, click on "Generate new token", enter "atheylab.utils" as the token description, check only the box in front of "repo" and then scroll down and click on "Generate token". On the next screen you will be shown the token. Github will only show it to you once.
2. Create a file named `.Renviron` in your home directory (you can find your home directory by running `Sys.getenv("HOME")` in the R console) that should have the following contents:

```
# github personal access token
GITHUB_PAT = YOUR TOKEN GOES HERE
```

and replace `YOUR TOKEN GOES HERE` with the access token you obtained in (1)

3. Restart your R session

4. Run the following commands in the R console:

``` r
install.packages("devtools")
devtools::install_github("gsbDBI/latently")
