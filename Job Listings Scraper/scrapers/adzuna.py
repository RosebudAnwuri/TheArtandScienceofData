"""
Scraper using the Adzuna API (free tier).
https://developer.adzuna.com/

Adzuna is a UK-based job aggregator — excellent coverage of London tech roles.
Free tier: 250 API calls/day.
"""

import logging
import re
import time

import requests

from config import ADZUNA_APP_ID, ADZUNA_API_KEY, TARGET_COMPANIES, SEARCH_QUERIES
from scrapers.base import JobListing

logger = logging.getLogger(__name__)

BASE_URL = "https://api.adzuna.com/v1/api/jobs/gb/search"


class AdzunaScraper:
    """Fetches data science jobs in London from the Adzuna API."""

    def __init__(self):
        self.app_id = ADZUNA_APP_ID
        self.api_key = ADZUNA_API_KEY
        self.target_companies = [c.lower() for c in TARGET_COMPANIES]

    def scrape(self) -> list[JobListing]:
        if not self.app_id or not self.api_key:
            logger.warning("Adzuna API keys not configured — skipping Adzuna scraper.")
            return []

        all_jobs: list[JobListing] = []
        seen_ids: set[str] = set()

        for query in SEARCH_QUERIES:
            jobs = self._search(query)
            for job in jobs:
                if job.job_id not in seen_ids:
                    seen_ids.add(job.job_id)
                    all_jobs.append(job)
            # Be polite — don't hammer the API
            time.sleep(0.5)

        logger.info("Adzuna: %d total unique jobs from target companies.", len(all_jobs))
        return all_jobs

    def _search(self, query: str, pages: int = 3) -> list[JobListing]:
        """Search Adzuna for a query, filtering to target companies in London."""
        results: list[JobListing] = []

        for page in range(1, pages + 1):
            params = {
                "app_id": self.app_id,
                "app_key": self.api_key,
                "what": query,
                "where": "London",
                "distance": 10,           # 10km radius from London
                "results_per_page": 50,
                "content-type": "application/json",
                "sort_by": "date",
            }

            try:
                resp = requests.get(f"{BASE_URL}/{page}", params=params, timeout=30)
                resp.raise_for_status()
                data = resp.json()
            except requests.RequestException as exc:
                logger.warning("Adzuna search failed for '%s' page %d: %s", query, page, exc)
                break

            jobs_raw = data.get("results", [])
            if not jobs_raw:
                break

            for job in jobs_raw:
                listing = self._parse_job(job)
                if listing and self._is_target_company(listing.company):
                    results.append(listing)

            logger.debug("Adzuna: page %d for '%s' → %d raw, %d matched target companies.",
                         page, query, len(jobs_raw), len(results))

        return results

    def _parse_job(self, job: dict) -> JobListing | None:
        """Parse a single Adzuna API result into a JobListing."""
        title = job.get("title", "")
        company_raw = job.get("company", {}).get("display_name", "")
        location_raw = job.get("location", {}).get("display_name", "")
        description = job.get("description", "")
        url = job.get("redirect_url", "")
        job_id = str(job.get("id", ""))
        created = job.get("created", "")

        # Extract salary if available
        salary_min = job.get("salary_min")
        salary_max = job.get("salary_max")
        salary_text = ""
        if salary_min and salary_max:
            salary_text = f"£{int(salary_min):,} – £{int(salary_max):,}"
        elif salary_min:
            salary_text = f"From £{int(salary_min):,}"

        work_mode = self._extract_work_mode(f"{title} {description} {location_raw}")

        return JobListing(
            job_id=f"adzuna_{job_id}",
            company=company_raw,
            title=self._clean_html(title),
            url=url,
            location=location_raw,
            description=self._clean_html(description),
            salary_text=salary_text,
            work_mode=work_mode,
            date_posted=created,
        )

    def _is_target_company(self, company_name: str) -> bool:
        """Check if the company matches any of our target companies."""
        name_lower = company_name.lower()
        return any(target in name_lower for target in self.target_companies)

    @staticmethod
    def _clean_html(text: str) -> str:
        text = re.sub(r"<[^>]+>", " ", text)
        return re.sub(r"\s+", " ", text).strip()

    @staticmethod
    def _extract_work_mode(text: str) -> str:
        text_lower = text.lower()
        if "remote" in text_lower and "hybrid" not in text_lower:
            return "remote"
        if "hybrid" in text_lower:
            return "hybrid"
        if "on-site" in text_lower or "onsite" in text_lower or "in-office" in text_lower:
            return "onsite"
        return "unknown"
