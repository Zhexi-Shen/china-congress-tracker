# China–US Congress Tracker

A small static site that tracks recent U.S. congressional activity related to
China (economy & trade emphasis), refreshed hourly:

1. **Bills** — recently-updated bills filtered to China relevance.
2. **Hearings & meetings** — committee hearings/meetings filtered by title and committee.
3. **Members** — activity (sponsored/cosponsored China bills) for a curated
   watchlist of high-influence members, plus members auto-added because they
   sponsor recent China bills.

Everything comes from the official [Congress.gov API](https://api.congress.gov),
and every entry links to its official congress.gov page.

## One-time setup

1. **Get a Congress.gov API key** at <https://api.congress.gov/sign-up/>.
2. **Push this folder to a GitHub repository.** For free GitHub Pages the repo
   must be **public** (GitHub Pages on private repos requires a paid plan).
3. **Add the key as a secret:**
   *Repository → Settings → Secrets and variables → Actions → New repository secret*
   - Name: `CONGRESS_API_KEY`
   - Value: your key
4. **Enable GitHub Pages:**
   *Repository → Settings → Pages → Source → "GitHub Actions"*.
5. **Run once** to verify:
   *Repository → Actions → "refresh-and-deploy" → Run workflow*.
   The site will appear at `https://<user>.github.io/<repo>/`.

From then on the workflow runs hourly (see
[`.github/workflows/refresh.yml`](.github/workflows/refresh.yml)).

## Run locally

```bash
# Windows PowerShell
$env:CONGRESS_API_KEY = "your-key"
Rscript R/fetch.R
quarto preview
```

Requires the R packages `httr2`, `jsonlite`, `reactable`, `htmltools`, and a
[Quarto](https://quarto.org) installation.

## Configuration

- **Watchlist** — edit [`config/members.csv`](config/members.csv). Names are in
  "Last, First" form and are matched against the current member list at fetch
  time; unmatched/ambiguous names are skipped with a warning in the workflow log.
- **Relevance keywords** — edit the `china_terms`, `econ_trade_terms`, and
  committee term lists at the top of [`R/fetch.R`](R/fetch.R).
- **Lookback window** — set the `LOOKBACK_HOURS` repository variable (default 72),
  or the `LOOKBACK_HOURS` environment variable locally.

## Known limitations

- The Congress.gov API has no committee-roster endpoint, so the "high-influence"
  list is a **curated watchlist + auto-derived sponsors**, not a full committee
  roster. Review the watchlist periodically (especially after leadership changes).
- Member activity uses title-keyword matching (cross-referenced with
  subject-verified bills), so a bill whose China relevance appears only in its
  subject tags can occasionally be missed there.
- The API rate limit is 5,000 requests/hour; a full run uses roughly a few
  hundred requests.
