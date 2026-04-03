"""
Configuration for the Job Listings Scraper.
Update these settings before running.
"""

# ── Email Settings ──────────────────────────────────────────────────────────
EMAIL_CONFIG = {
    "smtp_server": "smtp.gmail.com",
    "smtp_port": 587,
    "sender_email": "your_email@gmail.com",        # Your Gmail address
    "sender_password": "your_app_password",          # Gmail App Password (not your login password)
    "recipient_email": "your_email@gmail.com",       # Where to receive job alerts
}

# ── Scheduling ──────────────────────────────────────────────────────────────
SCHEDULE_DAY = "monday"   # Day of the week to run
SCHEDULE_TIME = "09:00"   # 24h format, e.g. "09:00"

# ── Scraper Settings ────────────────────────────────────────────────────────
LOCATION_KEYWORDS = ["london", "uk", "united kingdom", "remote", "emea"]
MIN_MATCH_SCORE = 85      # Minimum match % to include a listing (0-100)

# ── API Keys ────────────────────────────────────────────────────────────────
# You need at least ONE of these. Both are free to sign up.
#
# Adzuna (recommended — UK-focused, best for London jobs):
#   1. Go to https://developer.adzuna.com/
#   2. Sign up for free → you get an App ID and API Key
#
# RapidAPI JSearch (aggregates LinkedIn, Indeed, Glassdoor):
#   1. Go to https://rapidapi.com/letscrape-6bRBa3QguO5/api/jsearch
#   2. Sign up for free → subscribe to the free plan (500 requests/month)
#   3. Copy your "X-RapidAPI-Key" from the dashboard
#
ADZUNA_APP_ID = ""          # e.g. "a1b2c3d4"
ADZUNA_API_KEY = ""         # e.g. "e5f6g7h8i9j0k1l2m3n4o5p6"
RAPIDAPI_KEY = ""           # e.g. "abc123def456..."

# ── Target Companies ────────────────────────────────────────────────────────
# The scrapers search for data science jobs at these companies specifically.
# Jobs from other companies are ignored.
TARGET_COMPANIES = [
    # ── FAANG / Big Tech ──
    "Google",
    "Alphabet",
    "DeepMind",
    "Meta",
    "Facebook",
    "Apple",
    "Amazon",
    "AWS",
    "Netflix",
    "Microsoft",
    # ── Top AI Companies ──
    "Anthropic",
    "OpenAI",
    "Mistral",
    "Cohere",
    "Stability AI",
    "Hugging Face",
    "xAI",
    # ── Top-Paying Tech ──
    "Spotify",
    "Stripe",
    "Palantir",
    "Databricks",
    "Snowflake",
    "Airbnb",
    "Uber",
    "Revolut",
    "Monzo",
    # ── Quant / Finance ──
    "Two Sigma",
    "Citadel",
    "Jane Street",
    "Bloomberg",
    "D.E. Shaw",
    "Point72",
    "Man Group",
    "G-Research",
]

# ── Search Queries ──────────────────────────────────────────────────────────
# Multiple queries to cast a wide net
SEARCH_QUERIES = [
    "data scientist",
    "staff data scientist",
    "senior data scientist",
    "machine learning scientist",
    "applied scientist",
    "research scientist",
    "analytics manager",
    "lead data scientist",
]

# ── Data Storage ────────────────────────────────────────────────────────────
SENT_JOBS_DB = "sent_jobs.json"      # Tracks previously sent listings
SCRAPED_JOBS_DB = "scraped_jobs.json"  # Latest scrape results
