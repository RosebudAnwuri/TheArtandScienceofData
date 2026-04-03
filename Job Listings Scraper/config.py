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

# ── Target Companies ────────────────────────────────────────────────────────
# Each entry: (company_name, careers_url, platform_type)
# Platform types: "greenhouse", "lever", "workday", "custom"
TARGET_COMPANIES = [
    # ── FAANG / Big Tech ──
    {
        "name": "Google / Alphabet",
        "platform": "custom",
        "url": "https://www.google.com/about/careers/applications/jobs/results/?location=London%2C%20UK&q=data%20scientist",
        "api_url": None,
    },
    {
        "name": "Meta",
        "platform": "custom",
        "url": "https://www.metacareers.com/jobs?offices[0]=London%2C%20UK&q=data%20scientist",
        "api_url": None,
    },
    {
        "name": "Apple",
        "platform": "custom",
        "url": "https://jobs.apple.com/en-gb/search?location=london-LON&team=machine-learning-and-ai-MLAI+software-and-services-SFTWR",
        "api_url": None,
    },
    {
        "name": "Amazon",
        "platform": "custom",
        "url": "https://www.amazon.jobs/en-gb/search?base_query=data+scientist&loc_query=London%2C+England%2C+GBR",
        "api_url": None,
    },
    {
        "name": "Netflix",
        "platform": "greenhouse",
        "url": "https://jobs.netflix.com/search?location=London%2C%20United%20Kingdom&team=Data%20Science%20and%20Engineering",
        "api_url": None,
    },
    # ── Top AI Companies ──
    {
        "name": "Anthropic",
        "platform": "greenhouse",
        "url": "https://boards.greenhouse.io/anthropic",
        "api_url": "https://boards-api.greenhouse.io/v1/boards/anthropic/jobs",
    },
    {
        "name": "OpenAI",
        "platform": "greenhouse",
        "url": "https://boards.greenhouse.io/openai",
        "api_url": "https://boards-api.greenhouse.io/v1/boards/openai/jobs",
    },
    {
        "name": "DeepMind",
        "platform": "custom",
        "url": "https://deepmind.google/about/careers/",
        "api_url": None,
    },
    {
        "name": "Mistral AI",
        "platform": "greenhouse",
        "url": "https://boards.greenhouse.io/mistral",
        "api_url": "https://boards-api.greenhouse.io/v1/boards/mistral/jobs",
    },
    {
        "name": "Cohere",
        "platform": "greenhouse",
        "url": "https://boards.greenhouse.io/cohere",
        "api_url": "https://boards-api.greenhouse.io/v1/boards/cohere/jobs",
    },
    {
        "name": "Stability AI",
        "platform": "greenhouse",
        "url": "https://boards.greenhouse.io/stabilityai",
        "api_url": "https://boards-api.greenhouse.io/v1/boards/stabilityai/jobs",
    },
    # ── Top-Paying Tech ──
    {
        "name": "Spotify",
        "platform": "lever",
        "url": "https://www.lifeatspotify.com/jobs?query=data%20scientist&location=london",
        "api_url": None,
    },
    {
        "name": "Stripe",
        "platform": "custom",
        "url": "https://stripe.com/jobs/search?office_locations=London&teams=Data+%26+Machine+Learning",
        "api_url": None,
    },
    {
        "name": "Palantir",
        "platform": "greenhouse",
        "url": "https://boards.greenhouse.io/palantir",
        "api_url": "https://boards-api.greenhouse.io/v1/boards/palantir/jobs",
    },
    {
        "name": "Databricks",
        "platform": "greenhouse",
        "url": "https://boards.greenhouse.io/databricks",
        "api_url": "https://boards-api.greenhouse.io/v1/boards/databricks/jobs",
    },
    {
        "name": "Snowflake",
        "platform": "greenhouse",
        "url": "https://boards.greenhouse.io/snowflake",
        "api_url": "https://boards-api.greenhouse.io/v1/boards/snowflake/jobs",
    },
    {
        "name": "Airbnb",
        "platform": "greenhouse",
        "url": "https://boards.greenhouse.io/airbnb",
        "api_url": "https://boards-api.greenhouse.io/v1/boards/airbnb/jobs",
    },
    {
        "name": "Uber",
        "platform": "greenhouse",
        "url": "https://boards.greenhouse.io/uber",
        "api_url": "https://boards-api.greenhouse.io/v1/boards/uber/jobs",
    },
    {
        "name": "Two Sigma",
        "platform": "greenhouse",
        "url": "https://boards.greenhouse.io/twosigma",
        "api_url": "https://boards-api.greenhouse.io/v1/boards/twosigma/jobs",
    },
    {
        "name": "Citadel",
        "platform": "greenhouse",
        "url": "https://boards.greenhouse.io/citadel",
        "api_url": "https://boards-api.greenhouse.io/v1/boards/citadel/jobs",
    },
    {
        "name": "Jane Street",
        "platform": "greenhouse",
        "url": "https://boards.greenhouse.io/janestreet",
        "api_url": "https://boards-api.greenhouse.io/v1/boards/janestreet/jobs",
    },
    {
        "name": "Bloomberg",
        "platform": "custom",
        "url": "https://careers.bloomberg.com/job/search?lc=London&qf=data+scientist",
        "api_url": None,
    },
]

# ── Data Storage ────────────────────────────────────────────────────────────
SENT_JOBS_DB = "sent_jobs.json"      # Tracks previously sent listings
SCRAPED_JOBS_DB = "scraped_jobs.json"  # Latest scrape results
