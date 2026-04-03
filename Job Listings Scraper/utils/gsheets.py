"""
Google Sheets integration - saves job listings to a Google Sheet.

Uses gspread + a Google Service Account for authentication.
"""

import logging
import gspread
from google.oauth2.service_account import Credentials

from config import GSHEET_CREDENTIALS_FILE, GSHEET_SPREADSHEET_NAME

logger = logging.getLogger(__name__)

SCOPES = [
    "https://www.googleapis.com/auth/spreadsheets",
    "https://www.googleapis.com/auth/drive",
]

# Column headers for the sheet
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


def save_to_gsheet(scored_jobs):
    """
    Save a list of ScoredJob objects to the configured Google Sheet.
    Returns the number of jobs successfully added.
    """
    if not GSHEET_CREDENTIALS_FILE or not GSHEET_SPREADSHEET_NAME:
        print("[GSHEET] Not configured - set GSHEET_CREDENTIALS_FILE and GSHEET_SPREADSHEET_NAME in config.py")
        return 0

    print("[GSHEET] Using credentials: " + GSHEET_CREDENTIALS_FILE)
    print("[GSHEET] Target spreadsheet: " + GSHEET_SPREADSHEET_NAME)

    try:
        creds = Credentials.from_service_account_file(
            GSHEET_CREDENTIALS_FILE, scopes=SCOPES
        )
        client = gspread.authorize(creds)
    except Exception as exc:
        print("[GSHEET] Failed to authenticate: " + str(exc))
        print("[GSHEET] Check that your credentials JSON file path is correct in config.py")
        return 0

    # Open or create spreadsheet
    try:
        spreadsheet = client.open(GSHEET_SPREADSHEET_NAME)
        print("[GSHEET] Opened spreadsheet: " + GSHEET_SPREADSHEET_NAME)
    except gspread.SpreadsheetNotFound:
        print("[GSHEET] Spreadsheet '" + GSHEET_SPREADSHEET_NAME + "' not found.")
        print("[GSHEET] Make sure you shared it with the service account email.")
        print("[GSHEET] (The email is in your credentials JSON file under 'client_email')")
        return 0
    except Exception as exc:
        print("[GSHEET] Failed to open spreadsheet: " + str(exc))
        return 0

    # Get or create the worksheet
    try:
        worksheet = spreadsheet.sheet1
    except Exception as exc:
        print("[GSHEET] Failed to access worksheet: " + str(exc))
        return 0

    # Add headers if sheet is empty
    try:
        existing = worksheet.get_all_values()
        if not existing:
            worksheet.append_row(HEADERS)
            print("[GSHEET] Added column headers.")
            # Bold the header row
            worksheet.format("1:1", {"textFormat": {"bold": True}})
    except Exception as exc:
        print("[GSHEET] Warning - could not check/add headers: " + str(exc))

    # Build rows
    from datetime import date
    today = date.today().isoformat()

    rows = []
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
            "",  # Notes column - for user to fill in
            j.url if j.url.startswith("http") else "",
        ]
        rows.append(row)

    # Append all rows at once
    try:
        worksheet.append_rows(rows, value_input_option="USER_ENTERED")
        print("[GSHEET] Added " + str(len(rows)) + " jobs to '" + GSHEET_SPREADSHEET_NAME + "'.")
        return len(rows)
    except Exception as exc:
        print("[GSHEET] Failed to add rows: " + str(exc))
        return 0
