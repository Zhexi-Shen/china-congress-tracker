# china-congress-tracker: fetch China-related congressional activity
#
# Pulls from the Congress.gov API (https://api.congress.gov):
#   1. recently-updated bills (filtered to China relevance),
#   2. committee meetings / hearings (filtered by title + committee),
#   3. activity for a curated watchlist of members plus members who are
#      actively sponsoring China-related bills.
#
# Requires the CONGRESS_API_KEY environment variable. Writes JSON into data/.

library(httr2)
library(jsonlite)

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (length(a) == 0L) return(b)
  if (length(a) == 1L && is.atomic(a) && is.na(a)) return(b)
  a
}

ch <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA_character_)
  as.character(x[1])
}

detect_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  f <- grep("^--file=", args, value = TRUE)
  if (length(f)) {
    return(normalizePath(file.path(dirname(sub("^--file=", "", f[1])), "..")))
  }
  normalizePath(getwd())
}

project_root <- Sys.getenv("PROJECT_ROOT", unset = detect_root())
data_dir <- file.path(project_root, "data")
config_dir <- file.path(project_root, "config")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

base_url <- "https://api.congress.gov/v3"
lookback_hours <- as.numeric(Sys.getenv("LOOKBACK_HOURS", "72"))
max_detail_calls <- as.numeric(Sys.getenv("MAX_DETAIL_CALLS", "500"))
throttle_sec <- as.numeric(Sys.getenv("THROTTLE_SEC", "0.1"))

now_utc <- function() as.POSIXct(Sys.time(), tz = "UTC")
fmt_dt <- function(x) format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# ---------------------------------------------------------------------------
# Relevance keyword lists (edit to tune what counts as "China" / "economy")
# ---------------------------------------------------------------------------

china_terms <- c(
  "china", "chinese", "people's republic of china", "peoples republic of china",
  "prc", "taiwan", "taiwanese", "hong kong", "tibet", "xinjiang", "uyghur",
  "uighur", "macau", "macao", "south china sea", "indo-pacific", "indopacific",
  "beijing", "communist party", "ccp"
)

econ_trade_terms <- c(
  "trade", "tariff", "export", "import", "semiconductor", "chip", "chips",
  "technology transfer", "supply chain", "rare earth", "critical mineral",
  "sanction", "currency", "exchange rate", "investment", "manufacturing",
  "intellectual property", "tiktok", "telecommunication", "5g", "decoupling",
  "export control", "most favored nation", "mfn", "world trade organization",
  "wto", "economic", "economy", "fentanyl", "lithium", "cobalt", "nickel",
  "graphite"
)

econ_policy_areas <- c(
  "Foreign Trade and International Finance", "International Affairs",
  "Economics and Public Finance", "Taxation", "Commerce",
  "Finance and Financial Sector", "Energy", "Science, Technology, Communications",
  "Armed Forces and National Security", "Transportation and Public Works",
  "Immigration"
)

china_committee_terms <- c("china", "strategic competition", "indo-pacific", "taiwan")

relevant_committee_terms <- c(
  "foreign relations", "foreign affairs", "ways and means", "finance",
  "armed services", "intelligence", "homeland security", "banking",
  "commerce", "energy and commerce", "appropriations", "science, space",
  "small business", "judiciary"
)

match_any <- function(text, terms) {
  if (length(text) == 0L || any(is.na(text))) return(FALSE)
  txt <- tolower(paste(text, collapse = " "))
  if (!nzchar(trimws(txt))) return(FALSE)
  any(vapply(terms, function(t) grepl(t, txt, fixed = TRUE), logical(1)))
}

# ---------------------------------------------------------------------------
# API helpers
# ---------------------------------------------------------------------------

api_get <- function(path, query = list(), retries = 4) {
  key <- Sys.getenv("CONGRESS_API_KEY", "")
  if (!nzchar(key)) stop("CONGRESS_API_KEY environment variable is not set.")
  combined <- c(query, list(format = "json", api_key = key))
  req <- request(paste0(base_url, path))
  req <- do.call(req_url_query, c(list(.req = req), combined))
  # Handle HTTP status codes ourselves rather than letting httr2 throw on 4xx/5xx
  req <- req_error(req, is_error = function(resp) FALSE)
  for (i in seq_len(retries)) {
    resp <- tryCatch(req_perform(req), error = function(e) e)
    if (inherits(resp, "error")) {
      message("  request error (attempt ", i, "): ", conditionMessage(resp))
      Sys.sleep(min(2^i, 30))
      next
    }
    status <- resp_status(resp)
    if (status == 403L) {
      warning("  HTTP 403 for ", path, " (check CONGRESS_API_KEY)")
      return(NULL)
    }
    if (status %in% c(429L, 500L, 502L, 503L, 504L)) {
      message("  HTTP ", status, " on ", path, " (attempt ", i, ")")
      Sys.sleep(min(2^i * 2, 60))
      next
    }
    if (status >= 400L) {
      warning("  HTTP ", status, " for ", path)
      return(NULL)
    }
    Sys.sleep(throttle_sec)
    return(resp_body_json(resp, simplifyVector = FALSE))
  }
  warning("Giving up on ", path)
  NULL
}

fetch_list <- function(path, query = list(), item_key, max_pages = 10L, per_page = 250L) {
  out <- list()
  offset <- 0L
  repeat {
    page <- api_get(path, query = c(query, list(limit = per_page, offset = offset)))
    if (is.null(page)) break
    items <- page[[item_key]]
    if (is.null(items) || length(items) == 0L) break
    out <- c(out, items)
    if (length(items) < per_page) break
    offset <- offset + per_page
    if (offset >= per_page * max_pages) break
  }
  out
}

get_current_congress <- function() {
  r <- api_get("/congress/current")
  nm <- ch(r$congress$name)
  num <- suppressWarnings(as.integer(regmatches(nm, regexpr("[0-9]+", nm))))
  if (length(num) == 0L || is.na(num)) num <- 119L
  num
}

bill_id <- function(congress, type, number) {
  sprintf("%s-%s-%s", congress, tolower(type), number)
}

bill_official_url <- function(congress, type, number) {
  t <- tolower(type)
  doc <- switch(t,
    hr = "house-bill", s = "senate-bill",
    hjres = "house-joint-resolution", sjres = "senate-joint-resolution",
    hconres = "house-concurrent-resolution", sconres = "senate-concurrent-resolution",
    hres = "house-resolution", sres = "senate-resolution",
    "bill"
  )
  sprintf("https://www.congress.gov/bill/%sth-congress/%s/%s", congress, doc, number)
}

# ---------------------------------------------------------------------------
# 1. Bills
# ---------------------------------------------------------------------------

fetch_subjects <- function(congress, type, number) {
  r <- api_get(sprintf("/bill/%s/%s/%s/subjects", congress, tolower(type), number))
  subj <- r$subjects %||% list()
  leg <- subj$legislativeSubjects %||% list()
  list(
    subject_names = vapply(leg, function(x) ch(x$name), character(1)),
    policy_area = ch(subj$policyArea$name)
  )
}

fetch_china_bills <- function(from_dt, to_dt) {
  message("Fetching recently updated bills...")
  raw <- fetch_list(
    "/bill",
    query = list(
      fromDateTime = fmt_dt(from_dt), toDateTime = fmt_dt(to_dt),
      sort = "updateDate+desc"
    ),
    item_key = "bills", max_pages = 8L
  )
  message("  ", length(raw), " bills updated in window")
  # Offset pagination can drift on a live-updating feed; drop duplicate bills
  raw <- raw[!duplicated(vapply(raw, function(b) {
    bill_id(ch(b$congress), ch(b$type), ch(b$number))
  }, character(1)))]
  n <- min(length(raw), max_detail_calls)
  out <- list()
  for (i in seq_len(n)) {
    b <- raw[[i]]
    s <- fetch_subjects(ch(b$congress), ch(b$type), ch(b$number))
    policy <- ch(b$policyArea$name) %||% s$policy_area
    title <- ch(b$title)
    is_china <- match_any(title, china_terms) || match_any(s$subject_names, china_terms)
    if (!is_china) next
    is_econ <- policy %in% econ_policy_areas ||
      match_any(title, econ_trade_terms) ||
      match_any(s$subject_names, econ_trade_terms)
    sponsors <- lapply(b$sponsors %||% list(), function(sp) {
      list(bioguideId = ch(sp$bioguideId), fullName = ch(sp$fullName) %||% ch(sp$name))
    })
    out[[length(out) + 1L]] <- list(
      id = bill_id(ch(b$congress), ch(b$type), ch(b$number)),
      congress = ch(b$congress),
      bill = toupper(ch(b$type)),
      number = ch(b$number),
      title = title,
      policy_area = policy,
      is_econ_trade = is_econ,
      latest_action_date = ch(b$latestAction$actionDate),
      latest_action = ch(b$latestAction$text),
      update_date = ch(b$updateDate),
      sponsors = sponsors,
      url = bill_official_url(ch(b$congress), ch(b$type), ch(b$number))
    )
  }
  message("  ", length(out), " China-related bills")
  out
}

# ---------------------------------------------------------------------------
# 2. Committee meetings / hearings
# ---------------------------------------------------------------------------

fetch_meetings <- function(from_dt, to_dt) {
  message("Fetching committee meetings and hearings...")
  raw <- fetch_list(
    "/committee-meeting",
    query = list(fromDateTime = fmt_dt(from_dt), toDateTime = fmt_dt(to_dt)),
    item_key = "committeeMeetings", max_pages = 8L
  )
  message("  ", length(raw), " meetings in window")
  n <- min(length(raw), max_detail_calls)
  out <- list()
  for (i in seq_len(n)) {
    m <- raw[[i]]
    det <- api_get(sprintf(
      "/committee-meeting/%s/%s/%s",
      ch(m$congress), tolower(ch(m$chamber)), ch(m$eventId)
    ))
    if (is.null(det)) next
    det <- det$committeeMeeting %||% det
    title <- ch(det$title)
    committees <- vapply(det$committees %||% list(), function(c) ch(c$name), character(1))
    is_china <- match_any(title, china_terms) ||
      match_any(committees, china_committee_terms) ||
      (match_any(title, econ_trade_terms) && match_any(committees, relevant_committee_terms))
    if (!is_china) next
    event_url <- ch(det$videos[[1]]$url)
    if (is.na(event_url) || !nzchar(event_url)) {
      event_url <- sprintf(
        "https://www.congress.gov/event/%sth-congress/%s-event/%s",
        ch(det$congress), tolower(ch(det$chamber)), ch(det$eventId)
      )
    }
    out[[length(out) + 1L]] <- list(
      id = ch(det$eventId),
      congress = ch(det$congress),
      chamber = ch(det$chamber),
      date = ch(det$date),
      type = ch(det$type),
      status = ch(det$meetingStatus),
      title = title,
      committees = committees,
      url = event_url
    )
  }
  message("  ", length(out), " China-related meetings/hearings")
  out
}

# ---------------------------------------------------------------------------
# 3. Members
# ---------------------------------------------------------------------------

fetch_current_members <- function() {
  fetch_list("/member", query = list(currentMember = "true"),
             item_key = "members", max_pages = 8L)
}

member_chamber <- function(m) {
  if (is.null(m)) return(NA_character_)
  terms <- m$terms %||% list()
  items <- if (is.list(terms)) terms$item %||% terms else list()
  chs <- vapply(items, function(t) ch(t$chamber), character(1))
  chs <- unique(chs[!is.na(chs) & nzchar(chs)])
  if (length(chs) == 0L) return(NA_character_)
  if (any(grepl("Senate", chs, ignore.case = TRUE))) "Senate" else "House"
}

resolve_watchlist <- function(watchlist, members) {
  out <- list()
  for (i in seq_len(nrow(watchlist))) {
    nm <- tolower(trimws(watchlist$name[i]))
    hit <- Filter(function(x) identical(tolower(ch(x$name)), nm), members)
    m <- NULL
    if (length(hit) == 1L) {
      m <- hit[[1]]
    } else if (length(hit) == 0L) {
      last <- sub(",.*", "", nm)
      cand <- Filter(function(x) startsWith(tolower(ch(x$name)), paste0(last, ",")), members)
      if (length(cand) == 1L) {
        m <- cand[[1]]
      } else if (length(cand) > 1L) {
        warning("Ambiguous watchlist name: ", watchlist$name[i])
        next
      } else {
        warning("No current member matched: ", watchlist$name[i])
        next
      }
    } else {
      warning("Ambiguous watchlist name: ", watchlist$name[i])
      next
    }
    out[[length(out) + 1L]] <- list(
      bioguideId = ch(m$bioguideId),
      name = ch(m$name),
      state = ch(m$state),
      partyName = ch(m$partyName),
      chamber = ch(watchlist$chamber[i]),
      note = ch(watchlist$note[i]),
      watchlist = TRUE
    )
  }
  out
}

fetch_member_legislation <- function(member, current_congress, china_ids) {
  bio <- member$bioguideId
  out <- list()
  for (role in c("sponsored", "cosponsored")) {
    r <- api_get(sprintf("/member/%s/%s-legislation", bio, role), query = list(limit = 250L))
    if (is.null(r)) next
    key <- paste0(role, "Legislation")
    items <- r[[key]] %||% list()
    for (it in items) {
      if (!identical(as.character(it$congress), as.character(current_congress))) next
      title <- ch(it$title)
      id <- bill_id(ch(it$congress), ch(it$type), ch(it$number))
      is_china <- id %in% china_ids || match_any(title, china_terms)
      if (!is_china) next
      out[[length(out) + 1L]] <- list(
        role = if (role == "sponsored") "Sponsor" else "Cosponsor",
        id = id,
        title = title,
        introduced = ch(it$introducedDate),
        latest_action_date = ch(it$latestAction$actionDate),
        latest_action = ch(it$latestAction$text),
        url = bill_official_url(ch(it$congress), ch(it$type), ch(it$number))
      )
    }
  }
  out
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

main <- function() {
  if (!nzchar(Sys.getenv("CONGRESS_API_KEY", ""))) {
    stop("Set CONGRESS_API_KEY before running fetch.R.")
  }
  message("== China-US Congress tracker: fetch ==")

  current_congress <- get_current_congress()
  message("Current congress: ", current_congress)

  from_dt <- now_utc() - lookback_hours * 3600
  to_dt <- now_utc()
  message("Window: ", fmt_dt(from_dt), " .. ", fmt_dt(to_dt))

  bills <- fetch_china_bills(from_dt, to_dt)
  china_ids <- unique(vapply(bills, function(b) b$id, character(1)))

  meetings <- fetch_meetings(from_dt, to_dt)

  members_all <- fetch_current_members()
  message("  ", length(members_all), " current members")

  watchlist <- read.csv(
    file.path(config_dir, "members.csv"),
    stringsAsFactors = FALSE, strip.white = TRUE, comment.char = "#"
  )
  members <- resolve_watchlist(watchlist, members_all)

  # Auto-add sponsors of recent China-related bills (who is actively legislating)
  sponsor_ids <- character()
  sponsor_names <- character()
  for (b in bills) {
    for (sp in b$sponsors) {
      bio <- ch(sp$bioguideId)
      if (is.na(bio) || !nzchar(bio)) next
      sponsor_ids <- c(sponsor_ids, bio)
      sponsor_names[bio] <- ch(sp$fullName)
    }
  }
  watched_ids <- vapply(members, function(m) ch(m$bioguideId), character(1))
  if (length(sponsor_ids)) {
    tt <- sort(table(sponsor_ids), decreasing = TRUE)
    top <- names(tt)[seq_len(min(length(tt), 12L))]
    top <- setdiff(top, watched_ids)
    for (bio in top) {
      hit <- Filter(function(x) identical(ch(x$bioguideId), bio), members_all)
      info <- if (length(hit)) hit[[1]] else NULL
      members[[length(members) + 1L]] <- list(
        bioguideId = bio,
        name = ch(info$name) %||% ch(sponsor_names[bio]),
        state = ch(info$state),
        partyName = ch(info$partyName),
        chamber = member_chamber(info),
        note = sprintf("Sponsor of %d recent China-related bill(s)", unname(tt[bio])),
        watchlist = FALSE
      )
    }
  }

  message("Fetching activity for ", length(members), " members...")
  for (j in seq_along(members)) {
    m <- members[[j]]
    leg <- fetch_member_legislation(m, current_congress, china_ids)
    m$legislation <- leg
    m$n_legislation <- length(leg)
    members[[j]] <- m
  }

  members <- members[order(
    -vapply(members, function(m) isTRUE(m$watchlist), logical(1)),
    -vapply(members, function(m) length(m$legislation), integer(1))
  )]

  meta <- list(
    generated_at = fmt_dt(now_utc()),
    congress = as.integer(current_congress),
    lookback_hours = lookback_hours,
    counts = list(
      bills = length(bills),
      meetings = length(meetings),
      members = length(members)
    )
  )

  write_json(bills, file.path(data_dir, "bills.json"), auto_unbox = TRUE, pretty = TRUE, na = "null")
  write_json(meetings, file.path(data_dir, "meetings.json"), auto_unbox = TRUE, pretty = TRUE, na = "null")
  write_json(members, file.path(data_dir, "members.json"), auto_unbox = TRUE, pretty = TRUE, na = "null")
  write_json(meta, file.path(data_dir, "meta.json"), auto_unbox = TRUE, pretty = TRUE, na = "null")

  message("Done. Wrote data/ JSON files.")
  message(sprintf(
    "  bills=%d  meetings=%d  members=%d",
    length(bills), length(meetings), length(members)
  ))
}

if (!identical(Sys.getenv("FETCH_SKIP_RUN"), "1")) {
  main()
}
