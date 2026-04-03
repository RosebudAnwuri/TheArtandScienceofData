"""
Base scraper class — defines the common interface for all scrapers.
"""

import logging
import time
from abc import ABC, abstractmethod
from dataclasses import dataclass, field

import requests

logger = logging.getLogger(__name__)


def _clean(text: str) -> str:
    """Remove non-ASCII characters that break terminals and email encoding."""
    if not isinstance(text, str):
        return str(text)
    return text.encode("ascii", errors="ignore").decode("ascii")


@dataclass
class JobListing:
    """Represents a single job listing scraped from a careers page."""

    job_id: str                     # Unique ID (platform-specific or generated)
    company: str
    title: str
    url: str
    location: str = ""
    description: str = ""
    requirements: str = ""          # Parsed qualifications / requirements
    salary_text: str = ""           # Raw salary string if present
    work_mode: str = ""             # "remote", "hybrid", "onsite", or raw text
    office_days: int = -1           # -1 = unknown
    benefits: list = field(default_factory=list)
    date_posted: str = ""
    department: str = ""

    def __post_init__(self):
        """Clean all string fields of non-ASCII characters."""
        self.title = _clean(self.title)
        self.company = _clean(self.company)
        self.location = _clean(self.location)
        self.description = _clean(self.description)
        self.requirements = _clean(self.requirements)
        self.salary_text = _clean(self.salary_text)
        self.work_mode = _clean(self.work_mode)
        self.date_posted = _clean(self.date_posted)
        self.department = _clean(self.department)
        self.benefits = [_clean(b) for b in self.benefits]

    def unique_key(self) -> str:
        """Key used for deduplication across runs."""
        return f"{self.company}||{self.title}||{self.url}"


class BaseScraper(ABC):
    """Abstract base for all company scrapers."""

    USER_AGENT = (
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/124.0.0.0 Safari/537.36"
    )

    def __init__(self, company_config: dict):
        self.company = company_config["name"]
        self.url = company_config["url"]
        self.api_url = company_config.get("api_url")
        self.session = requests.Session()
        self.session.headers.update({"User-Agent": self.USER_AGENT})

    def _get(self, url: str, params: dict | None = None, retries: int = 3) -> requests.Response | None:
        """GET with retries and back-off."""
        for attempt in range(retries):
            try:
                resp = self.session.get(url, params=params, timeout=30)
                resp.raise_for_status()
                return resp
            except requests.RequestException as exc:
                logger.warning(
                    "%s: request failed (attempt %d/%d): %s",
                    self.company, attempt + 1, retries, exc,
                )
                if attempt < retries - 1:
                    time.sleep(2 ** attempt)
        return None

    @abstractmethod
    def scrape(self) -> list[JobListing]:
        """Return a list of JobListings from this company."""
        ...
