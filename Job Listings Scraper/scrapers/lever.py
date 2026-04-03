"""
Scraper for companies using Lever ATS.
Lever exposes a public JSON API: https://api.lever.co/v0/postings/{company}
"""

import logging
import re

from scrapers.base import BaseScraper, JobListing

logger = logging.getLogger(__name__)

DS_TITLE_KEYWORDS = [
    "data scien", "machine learning", "ml engineer", "applied scien",
    "research scien", "analytics", "decision scien", "quantitative",
    "data analy",
]

LOCATION_KEYWORDS = ["london", "uk", "united kingdom", "remote", "emea"]


class LeverScraper(BaseScraper):
    """Scrapes jobs from a Lever postings API."""

    # Known Lever board slugs for target companies
    LEVER_SLUGS = {
        "Spotify": "spotify",
    }

    def scrape(self) -> list[JobListing]:
        slug = self.LEVER_SLUGS.get(self.company)
        if not slug:
            logger.info("%s: no Lever slug configured, skipping.", self.company)
            return []

        api_url = f"https://api.lever.co/v0/postings/{slug}"
        resp = self._get(api_url)
        if resp is None:
            return []

        jobs_raw = resp.json()
        if not isinstance(jobs_raw, list):
            logger.warning("%s: unexpected Lever response format.", self.company)
            return []

        logger.info("%s: fetched %d total jobs from Lever.", self.company, len(jobs_raw))

        results: list[JobListing] = []
        for job in jobs_raw:
            title = job.get("text", "")
            if not self._is_ds_role(title):
                continue

            location = job.get("categories", {}).get("location", "")
            if not self._is_london_or_remote(location):
                continue

            desc_plain = job.get("descriptionPlain", "")
            additional_plain = job.get("additionalPlain", "")
            full_text = f"{desc_plain}\n{additional_plain}"

            listing = JobListing(
                job_id=job.get("id", ""),
                company=self.company,
                title=title,
                url=job.get("hostedUrl", self.url),
                location=location,
                description=full_text,
                department=job.get("categories", {}).get("team", ""),
                date_posted=str(job.get("createdAt", "")),
                salary_text=self._extract_salary(full_text),
                work_mode=self._extract_work_mode(full_text, location),
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
    def _is_london_or_remote(location: str) -> bool:
        loc_lower = location.lower()
        return any(kw in loc_lower for kw in LOCATION_KEYWORDS)

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
