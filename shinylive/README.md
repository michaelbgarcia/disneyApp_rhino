# Shinylive version

This folder contains a browser-native version of the Disney wait time dashboard that can be exported with [Shinylive](https://shiny.posit.co/shinylive/).

## What's included

- `app.R`: self-contained Shiny app that stays within Shinylive-compatible dependencies (`shiny`, `bslib`, `dplyr`, `ggplot2`).
- `data/rides.csv`: static snapshot generated from `app/static/parks_data.rds`, used to emulate live wait time data in the browser.
- `www/styles.css`: light theming tweaks applied via the standard `www/` path.

## Exporting with Shinylive

```r
install.packages("shinylive")
shinylive::export_app(
  app_dir = "shinylive",
  dest_dir = "shinylive/dist",
  include_r = TRUE,
  packages = c("shiny", "bslib", "dplyr", "ggplot2")
)
```

The `dist/` folder can be served from any static host (e.g., GitHub Pages, Netlify). To preview locally:

```r
shinylive::run_app("shinylive")
```

## GitHub Pages deployment

1. Commit the `shinylive/` folder and the workflow in `.github/workflows/shinylive-pages.yml`, then push to `main`.
2. In the GitHub UI go to **Settings → Pages** and choose **GitHub Actions** as the source (this only has to be done once).
3. Trigger the **Deploy Shinylive to GitHub Pages** workflow (either via push or the **Run workflow** button). The job will build the Shinylive bundle on CI, upload it as the Pages artifact, and publish it automatically.
4. Subsequent pushes that touch the Shinylive app or snapshot script will redeploy the site.

## Updating the snapshot dataset

If ride metadata changes, regenerate the CSV from the original Rhino app without disturbing the Shinylive folder:

```r
source("scripts/build_shinylive_snapshot.R")
```

(You can recreate the helper script from the README example in case it doesn't exist yet.)
