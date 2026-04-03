# Data Science Job Listings Scraper — London

Automated scraper that finds data science roles at top-paying tech companies in London, scores them against your profile, and sends a weekly email digest with only new, high-match listings.

## Features

- **35+ target companies**: FAANG, top AI labs (Anthropic, OpenAI, DeepMind), quant firms (Citadel, Jane Street, Two Sigma), and top-paying tech (Stripe, Spotify, Databricks)
- **Reliable data sources**: Uses Adzuna API (UK-focused) and JSearch/RapidAPI (aggregates LinkedIn, Indeed, Glassdoor)
- **Smart matching**: Scores each listing against your resume (skills, seniority, domain) — only shows ≥85% matches
- **Work mode filter**: Prefers remote/hybrid, rejects 5-days-in-office
- **Salary intelligence**: Shows listed salary or market estimates for London (base + total comp)
- **Benefits breakdown**: Stock/RSU, bonus, gym, perks per company
- **Deduplication**: Never sends the same role twice (persisted across runs)
- **Weekly email digest**: Beautiful HTML email with ranked listings and "Apply Now" links

## Quick Start

### 1. Install dependencies

```bash
cd "Job Listings Scraper"
pip install -r requirements.txt
```

### 2. Get your FREE API keys (takes 2 minutes)

You need **at least one** of these (both recommended for best coverage):

**Adzuna (recommended — best for UK/London jobs):**
1. Go to https://developer.adzuna.com/
2. Sign up for free
3. You'll get an **App ID** and **API Key**

**JSearch via RapidAPI (aggregates LinkedIn, Indeed, Glassdoor):**
1. Go to https://rapidapi.com/letscrape-6bRBa3QguO5/api/jsearch
2. Sign up for free → subscribe to the **free plan** (500 requests/month)
3. Copy your **X-RapidAPI-Key** from the dashboard

### 3. Configure

Edit `config.py` and fill in your keys:

```python
# API Keys (at least one required)
ADZUNA_APP_ID = "your_app_id"
ADZUNA_API_KEY = "your_api_key"
RAPIDAPI_KEY = "your_rapidapi_key"

# Email (for weekly digest)
EMAIL_CONFIG = {
    "sender_email": "your_email@gmail.com",
    "sender_password": "your_gmail_app_password",
    "recipient_email": "your_email@gmail.com",
    ...
}
```

### 4. Run

```bash
# Dry run — prints results to console (no email sent)
python3 main.py --dry-run

# Single run — scrapes and emails results
python3 main.py --run-once

# Weekly scheduler — runs every Monday at 9am (configurable)
python3 main.py --schedule
```

## Project Structure

```
Job Listings Scraper/
├── main.py                 # Orchestrator: scrape → match → dedup → email
├── config.py               # All settings (API keys, companies, email, schedule)
├── candidate_profile.py    # Your profile (skills, experience, preferences)
├── matcher.py              # Scoring engine + salary/benefits data
├── requirements.txt
├── scrapers/
│   ├── base.py             # Base JobListing dataclass
│   ├── adzuna.py           # Adzuna API scraper (UK-focused)
│   └── jsearch.py          # JSearch/RapidAPI scraper (LinkedIn, Indeed, etc.)
└── utils/
    ├── dedup.py            # Deduplication tracker (JSON-backed)
    └── emailer.py          # HTML email builder + SMTP sender
```

## How Matching Works

Each job is scored (0–100) across five dimensions:

| Dimension | Max Points | What it checks |
|-----------|-----------|----------------|
| Title match | 25 | How closely the title matches target DS roles |
| Seniority | 20 | Staff/Senior/Lead alignment (rejects junior roles) |
| Technical skills | 30 | Overlap between your skills and job description |
| Domain relevance | 15 | Industry/domain alignment with your experience |
| Work mode | 10 | Remote (+10), Hybrid (+5), 5-day office (-15) |

Only listings scoring **≥ 85%** are included in the digest.

## Customisation

- **Add companies**: Edit `TARGET_COMPANIES` in `config.py`
- **Adjust match threshold**: Change `MIN_MATCH_SCORE` in `config.py`
- **Update your profile**: Edit `candidate_profile.py` with new skills/experience
- **Change schedule**: Edit `SCHEDULE_DAY` and `SCHEDULE_TIME` in `config.py`

## Running as a Background Service

```bash
# Using nohup
nohup python3 main.py --schedule > scraper.log 2>&1 &

# Using cron (alternative to --schedule)
# crontab -e
# 0 9 * * 1 cd /path/to/Job\ Listings\ Scraper && python3 main.py --run-once
```
