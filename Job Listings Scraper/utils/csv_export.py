"""
CSV export - saves job listings to a local CSV file.
Zero setup required. Opens in Excel, Google Sheets, Numbers, etc.
"""

import csv
import os
import logging
from datetime import date

logger = logging.getLogger(__name__)

HEADERS = [
    "Date Found",
    "Company",
    "Job Title",
    "Match Score (%)",
    "Status",
    "Work Mode",
    "Location",
    "Base Salary",
    "Total Comp (est.)",
    "Stock/Equity",
    "Benefits",
    "Why I Fit",
    "Notes",
    "Apply Link",
]


def _to_safe(text):
    """Force safe ASCII string."""
    if not isinstance(text, str):
        text = str(text)
    return text.encode("ascii", errors="replace").decode("ascii")


def save_to_csv(scored_jobs, filepath="job_tracker.csv"):
    """
    Append matched jobs to a CSV file.
    Creates the file with headers if it doesn't exist.
    Returns the number of jobs added.
    """
    if not scored_jobs:
        return 0

    file_exists = os.path.exists(filepath)
    today = date.today().isoformat()

    try:
        with open(filepath, "a", newline="", encoding="utf-8") as f:
            writer = csv.writer(f)

            # Write headers if new file
            if not file_exists:
                writer.writerow(HEADERS)

            for sj in scored_jobs:
                j = sj.listing
                row = [
                    today,
                    _to_safe(j.company),
                    _to_safe(j.title),
                    sj.match_score,
                    "New",
                    _to_safe(sj.work_mode_rating).capitalize(),
                    _to_safe(j.location),
                    _to_safe(sj.predicted_salary),
                    _to_safe(sj.predicted_total_comp),
                    "Yes" if sj.has_stock else "No",
                    _to_safe(", ".join(sj.benefits[:6])) if sj.benefits else "",
                    _to_safe("; ".join(sj.match_reasons[:4])),
                    "",  # Notes - for user to fill in
                    j.url if j.url.startswith("http") else "",
                ]
                writer.writerow(row)

        print("[CSV] Saved " + str(len(scored_jobs)) + " jobs to " + filepath)
        if not file_exists:
            print("[CSV] Open this file in Excel or Google Sheets to view and track your applications.")
        return len(scored_jobs)

    except Exception as exc:
        print("[CSV] Failed to save: " + str(exc))
        return 0
