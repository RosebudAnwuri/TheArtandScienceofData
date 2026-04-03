"""
Generic scraper for companies without a structured API.
Uses Selenium to render JavaScript-heavy career pages, then parses with BeautifulSoup.
Falls back to requests + BeautifulSoup if Selenium is not available.
"""

import hashlib
import logging
import re

from bs4 import BeautifulSoup

from scrapers.base import BaseScraper, JobListing

logger = logging.getLogger(__name__)

DS_TITLE_KEYWORDS = [
    "data scien", "machine learning", "ml engineer", "applied scien",
    "research scien", "analytics", "decision scien", "quantitative",
    "data analy",
]

LOCATION_KEYWORDS = ["london", "uk", "united kingdom", "remote", "emea"]


def _try_selenium(url: str, timeout: int = 20) -> str | None:
    """Attempt to load a page with Selenium (headless Chrome). Returns HTML or None."""
    try:
        from selenium import webdriver
        from selenium.webdriver.chrome.options import Options
        from selenium.webdriver.chrome.service import Service

        opts = Options()
        opts.add_argument("--headless=new")
        opts.add_argument("--no-sandbox")
        opts.add_argument("--disable-dev-shm-usage")
        opts.add_argument(
            "user-agent=Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
            "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"
        )
        driver = webdriver.Chrome(options=opts)
        driver.set_page_load_timeout(timeout)
        driver.get(url)

        import time
        time.sleep(5)  # Let JS render

        html = driver.page_source
        driver.quit()
        return html
    except Exception as exc:
        logger.debug("Selenium not available or failed for %s: %s", url, exc)
        return None


class GenericScraper(BaseScraper):
    """Scrapes career pages that don't expose a clean API."""

    def scrape(self) -> list[JobListing]:
        html = self._fetch_page()
        if not html:
            return []

        soup = BeautifulSoup(html, "html.parser")
        listings = self._extract_listings(soup)
        logger.info("%s: found %d relevant DS listings.", self.company, len(listings))
        return listings

    def _fetch_page(self) -> str | None:
        # Try Selenium first for JS-rendered pages
        html = _try_selenium(self.url)
        if html:
            return html

        # Fallback to plain requests
        resp = self._get(self.url)
        if resp:
            return resp.text
        return None

    def _extract_listings(self, soup: BeautifulSoup) -> list[JobListing]:
        """
        Heuristic extraction: finds links whose text matches DS keywords
        and whose surrounding context mentions London/remote.
        """
        results: list[JobListing] = []
        seen_urls: set[str] = set()

        # Strategy: find all <a> tags whose text looks like a job title
        for link in soup.find_all("a", href=True):
            text = link.get_text(strip=True)
            if len(text) < 5 or len(text) > 200:
                continue
            if not self._is_ds_role(text):
                continue

            href = link["href"]
            if not href.startswith("http"):
                # Resolve relative URLs
                from urllib.parse import urljoin
                href = urljoin(self.url, href)

            if href in seen_urls:
                continue
            seen_urls.add(href)

            # Gather context: parent container text for location hints
            parent = link.find_parent(["div", "li", "tr", "article", "section"])
            context = parent.get_text(" ", strip=True) if parent else ""

            location = self._sniff_location(context)
            if not self._is_london_or_remote(location) and not self._is_london_or_remote(context):
                continue

            job_id = hashlib.md5(href.encode()).hexdigest()[:12]

            listing = JobListing(
                job_id=job_id,
                company=self.company,
                title=text,
                url=href,
                location=location or self._sniff_location(context),
                description=context[:2000],
                salary_text=self._extract_salary(context),
                work_mode=self._extract_work_mode(context, location),
            )
            results.append(listing)

        return results

    # ── Helpers ──────────────────────────────────────────────────────────

    @staticmethod
    def _is_ds_role(title: str) -> bool:
        title_lower = title.lower()
        return any(kw in title_lower for kw in DS_TITLE_KEYWORDS)

    @staticmethod
    def _is_london_or_remote(text: str) -> bool:
        text_lower = text.lower()
        return any(kw in text_lower for kw in LOCATION_KEYWORDS)

    @staticmethod
    def _sniff_location(text: str) -> str:
        """Try to find a location mention in surrounding text."""
        patterns = [
            r"London(?:\s*,\s*(?:UK|United Kingdom|England))?",
            r"Remote(?:\s*[-–/]\s*(?:UK|EMEA|Europe))?",
            r"Hybrid\s*[-–/]?\s*London",
        ]
        for pat in patterns:
            m = re.search(pat, text, re.IGNORECASE)
            if m:
                return m.group(0).strip()
        return ""

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
