"""
Scraper using JSearch API via RapidAPI (free tier).
https://rapidapi.com/letscrape-6bRBa3QguO5/api/jsearch

Aggregates listings from LinkedIn, Indeed, Glassdoor, ZipRecruiter, and more.
Free tier: 500 requests/month.
"""

import logging
import re
import time

import requests

from config import RAPIDAPI_KEY, TARGET_COMPANIES, SEARCH_QUERIES
from scrapers.base import JobListing

logger = logging.getLogger(__name__)

BASE_URL = "https://jsearch.p.rapidapi.com/search"


class JSearchScraper:
    """Fetches data science jobs in London from JSearch (RapidAPI)."""

    def __init__(self):
        self.api_key = RAPIDAPI_KEY
        self.target_companies = [c.lower() for c in TARGET_COMPANIES]

    def scrape(self) -> list[JobListing]:
        if not self.api_key:
            logger.warning("RapidAPI key not configured — skipping JSearch scraper.")
            return []

        all_jobs: list[JobListing] = []
        seen_ids: set[str] = set()

        # Use fewer queries to stay within the free tier (500 req/month)
        priority_queries = SEARCH_QUERIES[:4]  # Top 4 queries

        for query in priority_queries:
            jobs = self._search(query)
            for job in jobs:
                if job.job_id not in seen_ids:
                    seen_ids.add(job.job_id)
                    all_jobs.append(job)
            time.sleep(1)  # Respect rate limits

        logger.info("JSearch: %d total unique jobs from target companies.", len(all_jobs))
        return all_jobs

    def _search(self, query: str, pages: int = 2) -> list[JobListing]:
        """Search JSearch for a query, filtering to target companies in London."""
        results: list[JobListing] = []

        headers = {
            "X-RapidAPI-Key": self.api_key,
            "X-RapidAPI-Host": "jsearch.p.rapidapi.com",
        }

        for page in range(1, pages + 1):
            params = {
                "query": f"{query} in London, UK",
                "page": str(page),
                "num_pages": "1",
                "date_posted": "month",  # Last 30 days
            }

            try:
                resp = requests.get(BASE_URL, headers=headers, params=params, timeout=30)
                resp.raise_for_status()
                data = resp.json()
            except requests.RequestException as exc:
                logger.warning("JSearch failed for '%s' page %d: %s", query, page, exc)
                break

            jobs_raw = data.get("data", [])
            if not jobs_raw:
                break

            for job in jobs_raw:
                listing = self._parse_job(job)
                if listing and self._is_target_company(listing.company):
                    results.append(listing)

            logger.debug("JSearch: page %d for '%s' → %d raw, %d matched target companies.",
                         page, query, len(jobs_raw), len(results))

            time.sleep(0.5)

        return results

    def _parse_job(self, job: dict) -> JobListing | None:
        """Parse a single JSearch API result into a JobListing."""
        title = job.get("job_title", "")
        company = job.get("employer_name", "")
        city = job.get("job_city", "")
        country = job.get("job_country", "")
        location = f"{city}, {country}" if city else country
        description = job.get("job_description", "")
        url = job.get("job_apply_link", "") or job.get("job_google_link", "")
        job_id = job.get("job_id", "")
        posted = job.get("job_posted_at_datetime_utc", "")

        # Salary
        salary_min = job.get("job_min_salary")
        salary_max = job.get("job_max_salary")
        salary_period = job.get("job_salary_period", "")
        salary_currency = job.get("job_salary_currency", "")
        salary_text = ""
        if salary_min and salary_max:
            currency_sym = {"GBP": "£", "USD": "$", "EUR": "€"}.get(salary_currency, salary_currency)
            salary_text = f"{currency_sym}{int(salary_min):,} – {currency_sym}{int(salary_max):,}"
            if salary_period:
                salary_text += f" ({salary_period})"
        elif salary_min:
            currency_sym = {"GBP": "£", "USD": "$", "EUR": "€"}.get(salary_currency, salary_currency)
            salary_text = f"From {currency_sym}{int(salary_min):,}"

        # Work mode
        is_remote = job.get("job_is_remote", False)
        work_mode = "remote" if is_remote else self._extract_work_mode(f"{title} {description}")

        # Benefits from qualifications / highlights
        benefits = []
        highlights = job.get("job_highlights", {})
        if isinstance(highlights, dict):
            benefits_raw = highlights.get("Benefits", [])
            if benefits_raw:
                benefits = benefits_raw[:6]

        return JobListing(
            job_id=f"jsearch_{job_id}",
            company=company,
            title=title,
            url=url,
            location=location,
            description=description[:3000],
            salary_text=salary_text,
            work_mode=work_mode,
            date_posted=posted,
            benefits=benefits,
        )

    def _is_target_company(self, company_name: str) -> bool:
        """Check if the company matches any of our target companies."""
        name_lower = company_name.lower()
        return any(target in name_lower for target in self.target_companies)

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
