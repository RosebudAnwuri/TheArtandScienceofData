"""
Job matching and ranking engine.

Scores each job listing against the candidate profile and returns
a ranked list of matches above the configured threshold.
"""

import re
from dataclasses import dataclass

from candidate_profile import CANDIDATE_PROFILE
from scrapers.base import JobListing

# -- Salary Estimation Table (London, annual GBP) ----------------------------
SALARY_ESTIMATES = {
    "Google / Alphabet":  {"base": "GBP 120k-180k", "total": "GBP 180k-350k+", "stock": True},
    "Google":             {"base": "GBP 120k-180k", "total": "GBP 180k-350k+", "stock": True},
    "Alphabet":           {"base": "GBP 120k-180k", "total": "GBP 180k-350k+", "stock": True},
    "Meta":               {"base": "GBP 130k-190k", "total": "GBP 200k-400k+", "stock": True},
    "Facebook":           {"base": "GBP 130k-190k", "total": "GBP 200k-400k+", "stock": True},
    "Apple":              {"base": "GBP 110k-170k", "total": "GBP 160k-300k+", "stock": True},
    "Amazon":             {"base": "GBP 100k-150k", "total": "GBP 140k-280k+", "stock": True},
    "AWS":                {"base": "GBP 100k-150k", "total": "GBP 140k-280k+", "stock": True},
    "Netflix":            {"base": "GBP 150k-250k", "total": "GBP 150k-250k",  "stock": False},
    "Microsoft":          {"base": "GBP 100k-160k", "total": "GBP 150k-300k+", "stock": True},
    "Anthropic":          {"base": "GBP 130k-200k", "total": "GBP 200k-450k+", "stock": True},
    "OpenAI":             {"base": "GBP 130k-200k", "total": "GBP 200k-500k+", "stock": True},
    "DeepMind":           {"base": "GBP 120k-200k", "total": "GBP 180k-400k+", "stock": True},
    "Mistral AI":         {"base": "GBP 100k-160k", "total": "GBP 150k-300k+", "stock": True},
    "Mistral":            {"base": "GBP 100k-160k", "total": "GBP 150k-300k+", "stock": True},
    "Cohere":             {"base": "GBP 100k-150k", "total": "GBP 130k-250k+", "stock": True},
    "Stability AI":       {"base": "GBP 90k-150k",  "total": "GBP 120k-250k+", "stock": True},
    "Hugging Face":       {"base": "GBP 100k-160k", "total": "GBP 140k-280k+", "stock": True},
    "xAI":                {"base": "GBP 130k-200k", "total": "GBP 200k-450k+", "stock": True},
    "Spotify":            {"base": "GBP 100k-150k", "total": "GBP 130k-230k+", "stock": True},
    "Stripe":             {"base": "GBP 120k-180k", "total": "GBP 180k-350k+", "stock": True},
    "Palantir":           {"base": "GBP 100k-160k", "total": "GBP 150k-300k+", "stock": True},
    "Databricks":         {"base": "GBP 110k-170k", "total": "GBP 160k-350k+", "stock": True},
    "Snowflake":          {"base": "GBP 100k-160k", "total": "GBP 150k-300k+", "stock": True},
    "Airbnb":             {"base": "GBP 110k-170k", "total": "GBP 160k-320k+", "stock": True},
    "Uber":               {"base": "GBP 100k-160k", "total": "GBP 150k-300k+", "stock": True},
    "Revolut":            {"base": "GBP 90k-140k",  "total": "GBP 120k-220k+", "stock": True},
    "Monzo":              {"base": "GBP 80k-130k",  "total": "GBP 100k-180k+", "stock": True},
    "Two Sigma":          {"base": "GBP 120k-200k", "total": "GBP 200k-500k+", "stock": False},
    "Citadel":            {"base": "GBP 130k-220k", "total": "GBP 250k-600k+", "stock": False},
    "Jane Street":        {"base": "GBP 150k-250k", "total": "GBP 300k-700k+", "stock": False},
    "Bloomberg":          {"base": "GBP 90k-150k",  "total": "GBP 120k-250k+", "stock": False},
    "D.E. Shaw":          {"base": "GBP 120k-200k", "total": "GBP 200k-450k+", "stock": False},
    "Point72":            {"base": "GBP 120k-180k", "total": "GBP 180k-400k+", "stock": False},
    "Man Group":          {"base": "GBP 100k-160k", "total": "GBP 150k-300k+", "stock": False},
    "G-Research":         {"base": "GBP 100k-170k", "total": "GBP 160k-350k+", "stock": False},
}

# -- Benefits by Company (commonly reported) ---------------------------------
KNOWN_BENEFITS = {
    "Google / Alphabet": ["RSUs", "bonus", "gym / wellness", "free meals", "generous PTO", "learning budget", "parental leave"],
    "Google":            ["RSUs", "bonus", "gym / wellness", "free meals", "generous PTO", "learning budget", "parental leave"],
    "Meta":              ["RSUs", "bonus", "gym reimbursement", "free meals", "generous PTO", "parental leave"],
    "Facebook":          ["RSUs", "bonus", "gym reimbursement", "free meals", "generous PTO", "parental leave"],
    "Apple":             ["RSUs", "bonus", "fitness center", "employee discount", "education reimbursement"],
    "Amazon":            ["RSUs", "bonus", "relocation", "employee discount"],
    "AWS":               ["RSUs", "bonus", "relocation", "employee discount"],
    "Netflix":           ["top-of-market salary (no bonus/stock)", "unlimited PTO", "parental leave"],
    "Microsoft":         ["RSUs", "bonus", "gym", "generous PTO", "parental leave"],
    "Anthropic":         ["equity", "bonus", "health", "generous PTO", "learning budget"],
    "OpenAI":            ["equity (PPUs)", "bonus", "health", "generous PTO", "learning budget"],
    "DeepMind":          ["RSUs (Alphabet)", "bonus", "gym", "free meals", "generous PTO"],
    "Spotify":           ["RSUs", "bonus", "gym", "flexible work", "parental leave"],
    "Stripe":            ["RSUs", "bonus", "wellness stipend", "learning budget", "remote-friendly"],
    "Palantir":          ["RSUs", "bonus", "gym reimbursement"],
    "Databricks":        ["RSUs", "bonus", "health", "generous PTO"],
    "Snowflake":         ["RSUs", "bonus", "health"],
    "Revolut":           ["RSUs", "bonus", "health", "gym"],
    "Two Sigma":         ["cash bonus (large)", "health", "gym", "free meals"],
    "Citadel":           ["cash bonus (large)", "health", "gym", "relocation"],
    "Jane Street":       ["cash bonus (very large)", "health", "gym", "free meals"],
    "Bloomberg":         ["bonus", "health", "gym on-site", "free snacks"],
    "G-Research":        ["cash bonus (large)", "health", "gym"],
}

# -- Expanded skill keywords for matching ------------------------------------
# These are grouped so that matching ANY word in the group counts as a match.
# This handles cases where short descriptions use slightly different wording.
SKILL_GROUPS = [
    (["python", "pandas", "numpy", "scipy", "scikit"], "Python"),
    (["sql", "database", "queries", "postgresql", "mysql", "bigquery"], "SQL"),
    (["r ", " r,", "r programming", "rstudio", "tidyverse"], "R"),
    (["machine learning", "ml ", "ml,", "ml/", "modeling", "models"], "Machine Learning"),
    (["deep learning", "neural network", "pytorch", "tensorflow", "keras"], "Deep Learning"),
    (["a/b test", "ab test", "experiment", "hypothesis", "causal"], "Experimentation/Causal"),
    (["statistic", "bayesian", "regression", "probability", "inference"], "Statistics"),
    (["nlp", "natural language", "text mining", "llm", "language model"], "NLP"),
    (["data analy", "analytics", "insight", "metric", "kpi", "dashboard"], "Analytics"),
    (["product", "stakeholder", "cross-functional", "business"], "Product/Business"),
    (["etl", "pipeline", "data engineer", "airflow", "spark", "hadoop"], "Data Engineering"),
    (["visualization", "visualisation", "tableau", "looker", "power bi"], "Data Visualization"),
    (["cloud", "aws", "gcp", "azure"], "Cloud"),
    (["leadership", "mentor", "lead", "manage", "team"], "Leadership"),
    (["communication", "present", "executive", "stakeholder"], "Communication"),
]


@dataclass
class ScoredJob:
    """A job listing enriched with match score, salary, and benefit info."""
    listing: JobListing
    match_score: int            # 0-100
    match_reasons: list
    penalty_reasons: list
    predicted_salary: str
    predicted_total_comp: str
    has_stock: bool
    benefits: list
    work_mode_rating: str       # "great", "good", "acceptable", "poor"


def score_job(listing):
    """Score a single listing against the candidate profile."""
    score = 0
    reasons = []
    penalties = []

    desc_lower = listing.description.lower()
    title_lower = listing.title.lower()
    combined = title_lower + " " + desc_lower

    # How much text do we have? Short descriptions get a boost since we
    # can't fully assess skills from a snippet.
    desc_length = len(desc_lower.split())
    is_short_desc = desc_length < 100

    # -- 1. Title match (0-25 pts) -------------------------------------------
    title_score = _score_title(title_lower)
    score += title_score
    if title_score >= 20:
        reasons.append("Strong title match: '" + listing.title + "'")
    elif title_score >= 10:
        reasons.append("Good title match: '" + listing.title + "'")

    # -- 2. Seniority alignment (0-20 pts) -----------------------------------
    seniority_score, seniority_note = _score_seniority(title_lower, combined)
    score += seniority_score
    if seniority_note:
        if seniority_score >= 10:
            reasons.append(seniority_note)
        else:
            penalties.append(seniority_note)

    # -- 3. Technical skill overlap (0-30 pts) -------------------------------
    tech_score, matched_skills = _score_technical_skills(combined, is_short_desc)
    score += tech_score
    if matched_skills:
        reasons.append("Skills matched: " + ", ".join(matched_skills[:8]))

    # -- 4. Domain / experience relevance (0-15 pts) -------------------------
    domain_score = _score_domain(combined, is_short_desc)
    score += domain_score
    if domain_score >= 5:
        reasons.append("Domain alignment with your experience")

    # -- 5. Work mode (0-10 pts bonus / penalty) -----------------------------
    wm_score, wm_rating, wm_note = _score_work_mode(listing)
    score += wm_score
    if wm_note:
        if wm_score > 0:
            reasons.append(wm_note)
        else:
            penalties.append(wm_note)

    # Clamp
    score = max(0, min(100, score))

    # -- Salary & benefits lookup --------------------------------------------
    sal_info = _lookup_salary(listing.company)
    benefits_list = _lookup_benefits(listing.company)

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


def rank_jobs(listings, min_score=85):
    """Score, filter (>= min_score), and rank all listings."""
    scored = [score_job(j) for j in listings]
    filtered = [s for s in scored if s.match_score >= min_score]
    filtered.sort(key=lambda s: s.match_score, reverse=True)
    return filtered


# ===========================================================================
# Internal scoring helpers
# ===========================================================================

def _score_title(title_lower):
    """Score based on how closely the title matches target roles."""
    exact_matches = [
        ("staff data scientist", 25),
        ("principal data scientist", 23),
        ("lead data scientist", 22),
        ("senior data scientist", 22),
        ("head of data science", 22),
        ("data science manager", 20),
        ("senior analytics manager", 18),
        ("analytics manager", 16),
        ("applied scientist", 20),
        ("research scientist", 18),
        ("machine learning scientist", 20),
        ("decision scientist", 18),
        ("data scientist", 18),
        ("ml engineer", 16),
        ("machine learning engineer", 16),
        ("analytics lead", 17),
        ("quantitative analyst", 14),
        ("data analy", 12),
    ]
    for pattern, pts in exact_matches:
        if pattern in title_lower:
            return pts
    return 0


def _score_seniority(title_lower, combined):
    """Check seniority alignment - candidate is Staff level (~10 yrs exp)."""
    senior_keywords = ["staff", "principal", "lead", "senior", "head of", "director", "manager"]
    junior_keywords = ["junior", "intern", "entry level", "associate", "graduate", "new grad"]

    for kw in junior_keywords:
        if kw in title_lower:
            return (0, "Role appears too junior ('" + kw + "' in title)")

    for kw in senior_keywords:
        if kw in title_lower:
            return (20, "Seniority aligned: '" + kw + "' in title")

    # No explicit seniority - check description for years requirement
    yoe_match = re.search(r"(\d+)\+?\s*years?\s*(?:of\s+)?(?:experience|exp)", combined)
    if yoe_match:
        years_req = int(yoe_match.group(1))
        if years_req <= 12:
            return (18, "Experience requirement (" + str(years_req) + "+ yrs) fits your profile")
        else:
            return (8, "High experience bar (" + str(years_req) + "+ yrs) - may still fit")

    # No seniority signal at all - give benefit of the doubt for target companies
    return (15, "")


def _score_technical_skills(combined, is_short_desc=False):
    """Score based on how many skill groups match in the job description."""
    matched_names = []
    for keywords, group_name in SKILL_GROUPS:
        for kw in keywords:
            if kw in combined:
                matched_names.append(group_name)
                break

    total_groups = len(SKILL_GROUPS)
    match_count = len(matched_names)

    if is_short_desc:
        # Short descriptions can't show all skills - be generous
        # If we matched even a few, assume good overlap
        if match_count >= 5:
            pts = 30
        elif match_count >= 3:
            pts = 25
        elif match_count >= 2:
            pts = 20
        elif match_count >= 1:
            pts = 15
        else:
            pts = 8  # It's a DS role at a target company - assume baseline fit
    else:
        ratio = match_count / max(total_groups, 1)
        pts = min(30, int(ratio * 50))

    return (pts, matched_names)


def _score_domain(combined, is_short_desc=False):
    """Score domain / industry alignment."""
    domain_keywords = [
        "product", "ads", "advertising", "growth", "engagement",
        "integrity", "trust", "safety", "abuse", "fraud",
        "partnerships", "messaging", "communication",
        "experimentation", "a/b test", "measurement",
        "revenue", "monetization", "roi",
        "consulting", "financial", "fintech",
        "recommendation", "personalization",
    ]
    matched = sum(1 for s in domain_keywords if s in combined)

    if is_short_desc:
        # Short desc - be generous
        if matched >= 2:
            return 12
        elif matched >= 1:
            return 8
        else:
            return 5  # DS at target company = likely some overlap
    else:
        ratio = matched / max(len(domain_keywords), 1)
        return min(15, int(ratio * 50))


def _score_work_mode(listing):
    """
    Score work mode:
      remote: +10, great
      hybrid: +5, good
      onsite (not 5-day): 0, acceptable
      onsite (5 days): -15, poor (reject)
      unknown: +2, unknown
    """
    wm = listing.work_mode.lower()
    desc_lower = listing.description.lower()

    # Check for explicit 5-days-in-office
    if re.search(r"5\s*days?\s*(?:in[- ]?office|on[- ]?site|per\s*week)", desc_lower):
        return (-15, "poor", "5 days in office - does not meet your preference")

    if wm == "remote":
        return (10, "great", "Remote role - ideal match")
    if wm == "hybrid":
        return (5, "good", "Hybrid role")
    if wm == "onsite":
        return (0, "acceptable", "On-site role (not 5-day)")
    return (2, "unknown", "")


def _lookup_salary(company_name):
    """Look up salary estimates, trying partial match on company name."""
    # Direct match first
    if company_name in SALARY_ESTIMATES:
        return SALARY_ESTIMATES[company_name]
    # Partial match
    company_lower = company_name.lower()
    for key, val in SALARY_ESTIMATES.items():
        if key.lower() in company_lower or company_lower in key.lower():
            return val
    return {}


def _lookup_benefits(company_name):
    """Look up benefits, trying partial match on company name."""
    if company_name in KNOWN_BENEFITS:
        return KNOWN_BENEFITS[company_name]
    company_lower = company_name.lower()
    for key, val in KNOWN_BENEFITS.items():
        if key.lower() in company_lower or company_lower in key.lower():
            return val
    return []
