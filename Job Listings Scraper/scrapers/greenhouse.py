"""
Scraper for companies using Greenhouse ATS.
Greenhouse exposes a public JSON API: https://boards-api.greenhouse.io/v1/boards/{board}/jobs
"""

import logging
import re

from scrapers.base import BaseScraper, JobListing

logger = logging.getLogger(__name__)

# Keywords that indicate a data-science-related role
DS_TITLE_KEYWORDS = [
    "data scien", "machine learning", "ml engineer", "applied scien",
    "research scien", "analytics", "decision scien", "quantitative",
    "data analy",
]

LOCATION_KEYWORDS = ["london", "uk", "united kingdom", "remote", "emea"]


class GreenhouseScraper(BaseScraper):
    """Scrapes jobs from a Greenhouse board via the public API."""

    def scrape(self) -> list[JobListing]:
        if not self.api_url:
            logger.info("%s: no Greenhouse API URL configured, skipping.", self.company)
            return []

        resp = self._get(self.api_url, params={"content": "true"})
        if resp is None:
            return []

        data = resp.json()
        jobs_raw = data.get("jobs", [])
        logger.info("%s: fetched %d total jobs from Greenhouse.", self.company, len(jobs_raw))

        results: list[JobListing] = []
        for job in jobs_raw:
            title = job.get("title", "")
            if not self._is_ds_role(title):
                continue

            location = self._extract_location(job)
            if not self._is_london_or_remote(location):
                continue

            description_html = job.get("content", "")
            description_text = self._strip_html(description_html)

            listing = JobListing(
                job_id=str(job.get("id", "")),
                company=self.company,
                title=title,
                url=job.get("absolute_url", self.url),
                location=location,
                description=description_text,
                date_posted=job.get("updated_at", ""),
                department=self._extract_department(job),
                salary_text=self._extract_salary(description_text),
                work_mode=self._extract_work_mode(description_text, location),
            )
            results.append(listing)

        logger.info("%s: %d relevant DS jobs in London/Remote.", self.company, len(results))
        return results

    # ── Helpers ──────────────────────────────────────────────────────────

    @staticmethod
    def _is_ds_role(title: str) -> bool:
        title_lower = title.lower()
        return any(kw in title_lower for kw in DS_TITLE_KEYWORDS)

    @staticmethod
    def _extract_location(job: dict) -> str:
        locs = job.get("location", {})
        return locs.get("name", "") if isinstance(locs, dict) else str(locs)

    @staticmethod
    def _is_london_or_remote(location: str) -> bool:
        loc_lower = location.lower()
        return any(kw in loc_lower for kw in LOCATION_KEYWORDS)

    @staticmethod
    def _extract_department(job: dict) -> str:
        depts = job.get("departments", [])
        if depts and isinstance(depts[0], dict):
            return depts[0].get("name", "")
        return ""

    @staticmethod
    def _strip_html(html: str) -> str:
        text = re.sub(r"<[^>]+>", " ", html)
        return re.sub(r"\s+", " ", text).strip()

    @staticmethod
    def _extract_salary(text: str) -> str:
        patterns = [
            r"[\$£€]\s?\d[\d,\.]+\s*[-–to]+\s*[\$£€]?\s?\d[\d,\.]+",
            r"(?:salary|compensation|pay)[:\s]+[\$£€]?\s?\d[\d,\.]+",
            r"\d[\d,]+\s*(?:GBP|USD|EUR)\s*(?:[-–to]+\s*\d[\d,]+)?",
        ]
        for pat in patterns:
            m = re.search(pat, text, re.IGNORECASE)
            if m:
                return m.group(0).strip()
        return ""

    @staticmethod
    def _extract_work_mode(text: str, location: str) -> str:
        combined = f"{text} {location}".lower()
        if "remote" in combined and "hybrid" not in combined:
            return "remote"
        if "hybrid" in combined:
            return "hybrid"
        if "on-site" in combined or "onsite" in combined or "in-office" in combined:
            return "onsite"
        return "unknown"
