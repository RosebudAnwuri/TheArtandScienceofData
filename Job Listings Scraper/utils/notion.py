"""
Notion integration - saves job listings to a Notion database.

Creates a rich job tracker with columns for status, match score,
salary, company, work mode, etc.
"""

import logging
import requests

from config import NOTION_API_KEY, NOTION_DATABASE_ID

logger = logging.getLogger(__name__)

NOTION_API_URL = "https://api.notion.com/v1"
NOTION_VERSION = "2022-06-28"


def _to_ascii(text):
    """Force anything to safe string."""
    if not isinstance(text, str):
        text = str(text)
    return text.encode("ascii", errors="replace").decode("ascii")


def save_to_notion(scored_jobs):
    """
    Save a list of ScoredJob objects to the configured Notion database.
    Returns the number of jobs successfully added.
    """
    if not NOTION_API_KEY or not NOTION_DATABASE_ID:
        logger.warning("Notion not configured - skipping. Set NOTION_API_KEY and NOTION_DATABASE_ID in config.py")
        return 0

    headers = {
        "Authorization": "Bearer " + NOTION_API_KEY,
        "Content-Type": "application/json",
        "Notion-Version": NOTION_VERSION,
    }

    added = 0
    for sj in scored_jobs:
        j = sj.listing

        # Build the page properties matching the Notion database schema
        properties = {
            "Job Title": {
                "title": [{"text": {"content": _to_ascii(j.title)[:100]}}]
            },
            "Company": {
                "select": {"name": _to_ascii(j.company)[:100]}
            },
            "Location": {
                "rich_text": [{"text": {"content": _to_ascii(j.location)[:100]}}]
            },
            "Match Score": {
                "number": sj.match_score
            },
            "Status": {
                "select": {"name": "New"}
            },
            "Work Mode": {
                "select": {"name": _to_ascii(sj.work_mode_rating).capitalize()}
            },
            "Base Salary": {
                "rich_text": [{"text": {"content": _to_ascii(sj.predicted_salary)[:100]}}]
            },
            "Total Comp": {
                "rich_text": [{"text": {"content": _to_ascii(sj.predicted_total_comp)[:100]}}]
            },
            "Stock/Equity": {
                "checkbox": sj.has_stock
            },
            "Benefits": {
                "rich_text": [{"text": {"content": _to_ascii(", ".join(sj.benefits[:6]))[:200]}}]
            },
            "Why I Fit": {
                "rich_text": [{"text": {"content": _to_ascii("; ".join(sj.match_reasons[:4]))[:200]}}]
            },
            "Apply Link": {
                "url": j.url if j.url.startswith("http") else None
            },
            "Date Found": {
                "date": {"start": _get_today()}
            },
        }

        payload = {
            "parent": {"database_id": NOTION_DATABASE_ID},
            "properties": properties,
        }

        try:
            resp = requests.post(
                NOTION_API_URL + "/pages",
                headers=headers,
                json=payload,
                timeout=30,
            )
            if resp.status_code == 200:
                added += 1
                logger.debug("Added to Notion: %s at %s", j.title, j.company)
            else:
                error_msg = resp.json().get("message", resp.text[:200])
                logger.warning(
                    "Failed to add '%s' to Notion (HTTP %d): %s",
                    j.title, resp.status_code, error_msg
                )
        except requests.RequestException as exc:
            logger.warning("Notion API request failed for '%s': %s", j.title, exc)

    if added > 0:
        print("[NOTION] Added " + str(added) + " jobs to your Notion database.")
    else:
        print("[NOTION] No jobs were added. Check your API key and database ID.")

    return added


def setup_notion_database():
    """
    Create the job tracker database in Notion.
    Call this once to set up the database, then copy the database ID to config.py.

    Requires NOTION_API_KEY and a parent page ID.
    """
    if not NOTION_API_KEY:
        print("Set NOTION_API_KEY in config.py first.")
        return

    print("To create the database automatically, you need a parent page ID.")
    print("Instead, create it manually in Notion (see README) and paste the database ID into config.py.")
    print("")
    print("Your database needs these columns:")
    print("  1. Job Title     (Title)")
    print("  2. Company       (Select)")
    print("  3. Location      (Text)")
    print("  4. Match Score   (Number)")
    print("  5. Status        (Select: New, Applied, Interview, Offer, Rejected, Skipped)")
    print("  6. Work Mode     (Select: Great, Good, Acceptable, Poor, Unknown)")
    print("  7. Base Salary   (Text)")
    print("  8. Total Comp    (Text)")
    print("  9. Stock/Equity  (Checkbox)")
    print(" 10. Benefits      (Text)")
    print(" 11. Why I Fit     (Text)")
    print(" 12. Apply Link    (URL)")
    print(" 13. Date Found    (Date)")


def _get_today():
    """Get today's date as ISO string."""
    from datetime import date
    return date.today().isoformat()
