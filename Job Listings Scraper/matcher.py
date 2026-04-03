"""
Job matching and ranking engine.

Scores each job listing against the candidate profile and returns
a ranked list of matches above the configured threshold.
"""

import re
from dataclasses import dataclass

from candidate_profile import CANDIDATE_PROFILE
from scrapers.base import JobListing

# ── Salary Estimation Table (London, annual GBP) ────────────────────────────
# Based on 2024-2025 market data for top-1% paying tech companies in London.
SALARY_ESTIMATES = {
    "Google / Alphabet":  {"base": "£120k–£180k", "total": "£180k–£350k+", "stock": True},
    "Meta":               {"base": "£130k–£190k", "total": "£200k–£400k+", "stock": True},
    "Apple":              {"base": "£110k–£170k", "total": "£160k–£300k+", "stock": True},
    "Amazon":             {"base": "£100k–£150k", "total": "£140k–£280k+", "stock": True},
    "Netflix":            {"base": "£150k–£250k", "total": "£150k–£250k",  "stock": False},
    "Anthropic":          {"base": "£130k–£200k", "total": "£200k–£450k+", "stock": True},
    "OpenAI":             {"base": "£130k–£200k", "total": "£200k–£500k+", "stock": True},
    "DeepMind":           {"base": "£120k–£200k", "total": "£180k–£400k+", "stock": True},
    "Mistral AI":         {"base": "£100k–£160k", "total": "£150k–£300k+", "stock": True},
    "Cohere":             {"base": "£100k–£150k", "total": "£130k–£250k+", "stock": True},
    "Stability AI":       {"base": "£90k–£150k",  "total": "£120k–£250k+", "stock": True},
    "Spotify":            {"base": "£100k–£150k", "total": "£130k–£230k+", "stock": True},
    "Stripe":             {"base": "£120k–£180k", "total": "£180k–£350k+", "stock": True},
    "Palantir":           {"base": "£100k–£160k", "total": "£150k–£300k+", "stock": True},
    "Databricks":         {"base": "£110k–£170k", "total": "£160k–£350k+", "stock": True},
    "Snowflake":          {"base": "£100k–£160k", "total": "£150k–£300k+", "stock": True},
    "Airbnb":             {"base": "£110k–£170k", "total": "£160k–£320k+", "stock": True},
    "Uber":               {"base": "£100k–£160k", "total": "£150k–£300k+", "stock": True},
    "Two Sigma":          {"base": "£120k–£200k", "total": "£200k–£500k+", "stock": False},
    "Citadel":            {"base": "£130k–£220k", "total": "£250k–£600k+", "stock": False},
    "Jane Street":        {"base": "£150k–£250k", "total": "£300k–£700k+", "stock": False},
    "Bloomberg":          {"base": "£90k–£150k",  "total": "£120k–£250k+", "stock": False},
}

# ── Benefits by Company (commonly reported) ─────────────────────────────────
KNOWN_BENEFITS = {
    "Google / Alphabet": ["RSUs", "bonus", "gym / wellness", "free meals", "generous PTO", "learning budget", "parental leave"],
    "Meta":              ["RSUs", "bonus", "gym reimbursement", "free meals", "generous PTO", "parental leave"],
    "Apple":             ["RSUs", "bonus", "fitness center", "employee discount", "education reimbursement"],
    "Amazon":            ["RSUs", "bonus", "relocation", "employee discount"],
    "Netflix":           ["top-of-market salary (no bonus/stock)", "unlimited PTO", "parental leave"],
    "Anthropic":         ["equity", "bonus", "health", "generous PTO", "learning budget"],
    "OpenAI":            ["equity (PPUs)", "bonus", "health", "generous PTO", "learning budget"],
    "DeepMind":          ["RSUs (Alphabet)", "bonus", "gym", "free meals", "generous PTO"],
    "Spotify":           ["RSUs", "bonus", "gym", "flexible work", "parental leave"],
    "Stripe":            ["RSUs", "bonus", "wellness stipend", "learning budget", "remote-friendly"],
    "Palantir":          ["RSUs", "bonus", "gym reimbursement"],
    "Databricks":        ["RSUs", "bonus", "health", "generous PTO"],
    "Snowflake":         ["RSUs", "bonus", "health"],
    "Two Sigma":         ["cash bonus (large)", "health", "gym", "free meals"],
    "Citadel":           ["cash bonus (large)", "health", "gym", "relocation"],
    "Jane Street":       ["cash bonus (very large)", "health", "gym", "free meals"],
    "Bloomberg":         ["bonus", "health", "gym on-site", "free snacks"],
}


@dataclass
class ScoredJob:
    """A job listing enriched with match score, salary, and benefit info."""
    listing: JobListing
    match_score: int            # 0-100
    match_reasons: list[str]
    penalty_reasons: list[str]
    predicted_salary: str
    predicted_total_comp: str
    has_stock: bool
    benefits: list[str]
    work_mode_rating: str       # "great", "good", "acceptable", "poor"


def score_job(listing: JobListing) -> ScoredJob:
    """Score a single listing against the candidate profile."""
    score = 0
    reasons: list[str] = []
    penalties: list[str] = []

    desc_lower = listing.description.lower()
    title_lower = listing.title.lower()
    combined = f"{title_lower} {desc_lower}"

    # ── 1. Title match (0-25 pts) ───────────────────────────────────────
    title_score = _score_title(title_lower)
    score += title_score
    if title_score >= 20:
        reasons.append(f"Strong title match: '{listing.title}'")
    elif title_score >= 10:
        reasons.append(f"Good title match: '{listing.title}'")

    # ── 2. Seniority alignment (0-20 pts) ───────────────────────────────
    seniority_score, seniority_note = _score_seniority(title_lower, combined)
    score += seniority_score
    if seniority_note:
        (reasons if seniority_score >= 10 else penalties).append(seniority_note)

    # ── 3. Technical skill overlap (0-30 pts) ──────────────────────────
    tech_score, matched_skills = _score_technical_skills(combined)
    score += tech_score
    if matched_skills:
        reasons.append(f"Skills matched: {', '.join(matched_skills[:8])}")

    # ── 4. Domain / experience relevance (0-15 pts) ────────────────────
    domain_score = _score_domain(combined)
    score += domain_score
    if domain_score >= 8:
        reasons.append("Strong domain alignment with your experience")

    # ── 5. Work mode (0-10 pts bonus / penalty) ────────────────────────
    wm_score, wm_rating, wm_note = _score_work_mode(listing)
    score += wm_score
    if wm_note:
        (reasons if wm_score > 0 else penalties).append(wm_note)

    # Clamp
    score = max(0, min(100, score))

    # ── Salary & benefits lookup ────────────────────────────────────────
    sal_info = SALARY_ESTIMATES.get(listing.company, {})
    benefits_list = KNOWN_BENEFITS.get(listing.company, [])

    return ScoredJob(
        listing=listing,
        match_score=score,
        match_reasons=reasons,
        penalty_reasons=penalties,
        predicted_salary=listing.salary_text or sal_info.get("base", "Not listed"),
        predicted_total_comp=sal_info.get("total", "Not listed"),
        has_stock=sal_info.get("stock", False),
        benefits=listing.benefits or benefits_list,
        work_mode_rating=wm_rating,
    )


def rank_jobs(listings: list[JobListing], min_score: int = 85) -> list[ScoredJob]:
    """Score, filter (>= min_score), and rank all listings."""
    scored = [score_job(j) for j in listings]
    filtered = [s for s in scored if s.match_score >= min_score]
    filtered.sort(key=lambda s: s.match_score, reverse=True)
    return filtered


# ═══════════════════════════════════════════════════════════════════════════
# Internal scoring helpers
# ═══════════════════════════════════════════════════════════════════════════

def _score_title(title_lower: str) -> int:
    """Score based on how closely the title matches target roles."""
    exact_matches = [
        ("staff data scientist", 25),
        ("principal data scientist", 23),
        ("lead data scientist", 22),
        ("senior data scientist", 20),
        ("head of data science", 20),
        ("data science manager", 20),
        ("senior analytics manager", 18),
        ("analytics manager", 16),
        ("applied scientist", 18),
        ("research scientist", 16),
        ("machine learning scientist", 18),
        ("decision scientist", 16),
        ("data scientist", 15),
        ("ml engineer", 14),
        ("machine learning engineer", 14),
        ("analytics lead", 15),
        ("quantitative analyst", 12),
        ("data analy", 10),
    ]
    for pattern, pts in exact_matches:
        if pattern in title_lower:
            return pts
    return 0


def _score_seniority(title_lower: str, combined: str) -> tuple[int, str]:
    """Check seniority alignment — candidate is Staff level (~10 yrs exp)."""
    senior_keywords = ["staff", "principal", "lead", "senior", "head of", "director", "manager"]
    junior_keywords = ["junior", "intern", "entry", "associate", "graduate", "new grad"]

    for kw in junior_keywords:
        if kw in title_lower:
            return (0, f"Role appears too junior ('{kw}' in title)")

    for kw in senior_keywords:
        if kw in title_lower:
            return (20, f"Seniority aligned: '{kw}' in title")

    # No explicit seniority — check description for years requirement
    yoe_match = re.search(r"(\d+)\+?\s*years?\s*(?:of\s+)?(?:experience|exp)", combined)
    if yoe_match:
        years_req = int(yoe_match.group(1))
        if years_req <= 12:
            return (15, f"Experience requirement ({years_req}+ yrs) fits your profile")
        else:
            return (5, f"High experience bar ({years_req}+ yrs) — may still fit")

    return (10, "")  # Neutral — no seniority signal


def _score_technical_skills(combined: str) -> tuple[int, list[str]]:
    """Score based on how many candidate skills appear in the job description."""
    candidate_skills = CANDIDATE_PROFILE["technical_skills"]
    matched = [s for s in candidate_skills if s in combined]
    ratio = len(matched) / max(len(candidate_skills), 1)
    pts = min(30, int(ratio * 45))  # Generous scaling since not all skills will appear
    return (pts, matched)


def _score_domain(combined: str) -> int:
    """Score domain / industry alignment."""
    domain_skills = CANDIDATE_PROFILE["domain_skills"]
    matched = sum(1 for s in domain_skills if s in combined)
    ratio = matched / max(len(domain_skills), 1)
    return min(15, int(ratio * 40))


def _score_work_mode(listing: JobListing) -> tuple[int, str, str]:
    """
    Score work mode:
      remote → +10, great
      hybrid → +5, good
      onsite (≤4 days) → 0, acceptable
      onsite (5 days) → -15, poor (reject)
      unknown → +2, unknown
    """
    wm = listing.work_mode.lower()
    desc_lower = listing.description.lower()

    # Check for explicit 5-days-in-office
    if re.search(r"5\s*days?\s*(?:in[- ]?office|on[- ]?site|per\s*week)", desc_lower):
        return (-15, "poor", "5 days in office — does not meet your preference")

    if wm == "remote":
        return (10, "great", "Remote role — ideal match")
    if wm == "hybrid":
        return (5, "good", "Hybrid role")
    if wm == "onsite":
        return (0, "acceptable", "On-site role (not 5-day)")
    return (2, "unknown", "")
