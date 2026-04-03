# Data Science Job Listings Scraper — London

Automated scraper that finds data science roles at top-paying tech companies in London, scores them against your profile, and sends a weekly email digest with only new, high-match listings.

## Features

- **25+ target companies**: FAANG, top AI labs (Anthropic, OpenAI, DeepMind), quant firms (Citadel, Jane Street, Two Sigma), and top-paying tech (Stripe, Spotify, Databricks)
- **Smart matching**: Scores each listing against your resume (skills, seniority, domain) — only shows ≥85% matches
- **Work mode filter**: Prefers remote/hybrid, rejects 5-days-in-office
- **Salary intelligence**: Shows listed salary or market estimates for London (base + total comp)
- **Benefits breakdown**: Stock/RSU, bonus, gym, perks per company
- **Deduplication**: Never sends the same role twice (persisted across runs)
- **Weekly email digest**: Beautiful HTML email with ranked listings and "Apply Now" links
- **Multi-platform scrapers**: Greenhouse API, Lever API, and Selenium-based generic scraper

## Quick Start

### 1. Install dependencies

```bash
cd "Job Listings Scraper"
pip install -r requirements.txt
```

For companies with JavaScript-rendered career pages, install Chrome/Chromium:
```bash
# Ubuntu/Debian
sudo apt install chromium-browser chromium-chromedriver

# macOS
brew install --cask chromium
```

### 2. Configure email

Edit `config.py` and update the `EMAIL_CONFIG` section:

```python
EMAIL_CONFIG = {
    "smtp_server": "smtp.gmail.com",
    "smtp_port": 587,
    "sender_email": "your_email@gmail.com",
    "sender_password": "your_app_password",   # Gmail App Password
    "recipient_email": "your_email@gmail.com",
}
```

**Gmail App Password**: Go to [Google Account → Security → 2-Step Verification → App passwords](https://myaccount.google.com/apppasswords) and generate one.

### 3. Run

```bash
# Dry run — prints results to console (no email sent)
python main.py --dry-run

# Single run — scrapes and emails results
python main.py --run-once

# Weekly scheduler — runs every Monday at 9am (configurable in config.py)
python main.py --schedule
```

## Project Structure

```
Job Listings Scraper/
├── main.py                 # Orchestrator: scrape → match → dedup → email
├── config.py               # All settings (companies, email, schedule)
├── candidate_profile.py    # Your profile (skills, experience, preferences)
├── matcher.py              # Scoring engine + salary/benefits data
├── requirements.txt
├── scrapers/
│   ├── base.py             # Base scraper class + JobListing dataclass
│   ├── greenhouse.py       # Greenhouse ATS API scraper
│   ├── lever.py            # Lever ATS API scraper
│   └── generic.py          # Selenium/BS4 scraper for custom career pages
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

To keep the scheduler running persistently:

```bash
# Using nohup
nohup python main.py --schedule > scraper.log 2>&1 &

# Using systemd (recommended for servers)
# Create /etc/systemd/system/job-scraper.service and enable it

# Using cron (alternative to --schedule)
# crontab -e
# 0 9 * * 1 cd /path/to/Job\ Listings\ Scraper && python main.py --run-once
```
