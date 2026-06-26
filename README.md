# Study Timer (R Shiny + Google Sheets)

A tiny Shiny app: press **Start studying** / **Start break** / **Stop**, and each
finished segment is written as a row to a Google Sheet. The dashboard shows your
study, break and total time for today and a live cumulative-study line chart.

## Files
- `app.R` — entry point: UI + buttons + server wiring (the file Posit runs)
- `01-main.R` — data layer: Google Sheets auth + read/write
- `02-logic.R` — pure logic: adding up study/break/total, date breakdown, chart data
- `manifest.json` — you generate this (see step 6); tells Posit which packages to install

The Google Sheet ends up with one row per segment:

| date | task_name | type | start_time | end_time | duration_min |
|------|-----------|------|------------|----------|--------------|

---

## One-time setup

### 1. Make the Google Sheet
Create a blank Google Sheet. Copy its **ID** from the URL
`https://docs.google.com/spreadsheets/d/THIS_PART/edit`. The app auto-creates a
tab named `log` with headers on first run, so you don't have to.

### 2. Create a Google service account (so the deployed app can log in without a browser)
1. Go to <https://console.cloud.google.com/> → create a project.
2. APIs & Services → Library → enable **Google Sheets API** (and **Google Drive API**).
3. APIs & Services → Credentials → Create credentials → **Service account**.
4. Open the service account → **Keys** → Add key → **JSON**. A `.json` file downloads.
5. Copy the service account's email (looks like `name@project.iam.gserviceaccount.com`).

### 3. Share the sheet with the service account
In the Google Sheet, click **Share** and give that service-account email **Editor** access.
(The app writes as the service account, so it must have access.)

### 4. Run locally
- Put the downloaded JSON file in this folder, renamed `service-account.json`
  (it's gitignored — it will not be committed).
- Create a `.Renviron` file in this folder with your sheet ID:
  ```
  STUDY_SHEET_ID=PASTE_YOUR_SHEET_ID_HERE
  ```
- Install packages, then run:
  ```r
  install.packages(c("shiny","bslib","dplyr","ggplot2","googlesheets4","tibble"))
  shiny::runApp()
  ```

### 5. Push to GitHub
```bash
git init
git add .
git commit -m "Study timer v1"
git branch -M main
git remote add origin https://github.com/YOUR_NAME/study-timer.git
git push -u origin main
```
`service-account.json` and `.Renviron` are gitignored, so your secrets stay out of GitHub.

### 6. Generate the dependency file Posit needs
In R, from this folder:
```r
install.packages("rsconnect")
rsconnect::writeManifest()
```
Commit and push the resulting `manifest.json`.

### 7. Deploy on Posit Connect Cloud
1. Sign in at <https://connect.posit.cloud/>.
2. Click **Publish** → **Shiny** → pick your GitHub repo and branch → choose `app.R`.
3. Before/after first publish, open the content's **Variables / Secrets** and add:
   - `STUDY_SHEET_ID` = your sheet ID
   - `GOOGLE_SERVICE_ACCOUNT_JSON` = the **entire contents** of your service-account.json
     (open the file, copy everything, paste it as the value)
4. Publish. To update later, just push to GitHub and click **Republish**.

> Alternative host: **shinyapps.io** works too, but you deploy with
> `rsconnect::deployApp()` from RStudio rather than from GitHub. Same env vars apply.

## Notes / ideas to extend
- `daily_summary()` in `02-logic.R` already produces per-day study/break/total —
  add a second chart or table to show history across days.
- Timer refresh is every second (`reactiveTimer(1000)` in `app.R`); set it to
  `60000` for once-a-minute updates.