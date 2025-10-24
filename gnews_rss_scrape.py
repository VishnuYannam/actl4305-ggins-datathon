"""
News sentiment over a date window for lists of countries in a DataFrame,
using Google News RSS + VADER sentiment on headlines/summaries.

Requirements:
    pip install pandas feedparser nltk tqdm

If VADER lexicon hasn't been downloaded before, the script will download it once.

Input:
    A DataFrame `df` with a column `countries`, where each row is a list of str:
        df = pd.DataFrame({
            "id": [1, 2],
            "countries": [["Japan", "South Korea"], ["Italy", "France", "Spain"]]
        })

Output:
    The same DataFrame with added columns:
        - sent_mean: mean compound sentiment for the row (across all matched articles)
        - articles_count: total number of articles used for that row
        - per_country_stats: dict {country: {"mean": float, "n": int}}
        - errors: list of errors (if any) encountered for that row
"""

from __future__ import annotations
import time
import math
import json
from typing import List, Dict, Tuple, Any, Iterable

import pandas as pd
import feedparser
from urllib.parse import quote_plus
from datetime import datetime
from tqdm import tqdm

# Sentiment
import nltk
from nltk.sentiment import SentimentIntensityAnalyzer

# ---------- Config ----------
# Date window (inclusive for query; Google News RSS uses query operators)
DATE_START = "2024-5-01"
DATE_END   = "2024-9-31"

# Column name containing a list of countries per row
COUNTRY_LIST_COL = "cleaned_dest_list"

# Google News RSS base
# We'll use the "before:" and "after:" operators inside the query.
# We request English results (change hl/gl/ceid if you want a different locale).
GOOG_NEWS_RSS_BASE = "https://news.google.com/rss/search"

# Politeness
REQUEST_SLEEP_SECONDS = 0.4  # short pause between RSS requests

# Max items to pull per country (RSS returns a lot; cap if you want)
MAX_ITEMS_PER_COUNTRY = 100  # set None for no cap

# ---------- Helpers ----------
def ensure_vader() -> SentimentIntensityAnalyzer:
    try:
        nltk.data.find("sentiment/vader_lexicon.zip")
    except LookupError:
        nltk.download("vader_lexicon")
    return SentimentIntensityAnalyzer()

def build_rss_url_for_country(country: str, start: str = DATE_START, end: str = DATE_END,
                              hl: str = "en-AU", gl: str = "AU", ceid: str = "AU:en") -> str:
    """
    Build a Google News RSS URL using keyword + date window.
    We include `after:` and `before:` operators in the query.
    """
    # Example query:  Japan after:2024-10-01 before:2024-12-31
    # You can add additional keywords if needed, e.g. +("travel insurance")
    q = f'{country} after:{start} before:{end}'
    params = f"?q={quote_plus(q)}&hl={hl}&gl={gl}&ceid={ceid}"
    return GOOG_NEWS_RSS_BASE + params

def parse_entries_from_rss(url: str, cap: int | None = MAX_ITEMS_PER_COUNTRY) -> List[dict]:
    """
    Fetch and parse RSS entries from Google News.
    Returns a list of entry dicts with title, summary, link, published (if present).
    """
    parsed = feedparser.parse(url)
    entries = parsed.get("entries", []) or []
    if cap is not None:
        entries = entries[:cap]
    out = []
    for e in entries:
        title = e.get("title", "") or ""
        summary = e.get("summary", "") or ""
        link = e.get("link", "") or ""
        pub = e.get("published", "") or e.get("updated", "") or ""
        out.append({"title": title, "summary": summary, "link": link, "published": pub})
    return out

def sentiment_for_entries(entries: List[dict], sia: SentimentIntensityAnalyzer) -> Tuple[float, int]:
    """
    Compute mean VADER compound sentiment across entries, using title+summary.
    Returns (mean_compound, count_used). If no entries -> (nan, 0)
    """
    if not entries:
        return (math.nan, 0)

    scores = []
    for e in entries:
        text = (e.get("title","") or "") + "\n" + (e.get("summary","") or "")
        text = text.strip()
        if not text:
            continue
        comp = sia.polarity_scores(text).get("compound", 0.0)
        scores.append(comp)

    if not scores:
        return (math.nan, 0)
    return (float(sum(scores) / len(scores)), len(scores))

def aggregate_row_sentiment(country_list: Iterable[str],
                            sia: SentimentIntensityAnalyzer,
                            cache: dict) -> Tuple[float, int, Dict[str, dict], list]:
    """
    For a list of countries, fetch Google News RSS per country (with date filters),
    compute per-country mean compound sentiment, then compute the row-level mean
    across all articles pooled, and return diagnostics.

    cache: dict[str, dict] mapping URL -> {"entries": [...], "ts": epoch}
    """
    per_country_stats: Dict[str, dict] = {}
    total_scores = []
    total_n = 0
    errors = []

    for country in country_list:
        try:
            url = build_rss_url_for_country(country)
            if url in cache:
                entries = cache[url]["entries"]
            else:
                entries = parse_entries_from_rss(url)
                cache[url] = {"entries": entries, "ts": time.time()}

            mean_c, n_c = sentiment_for_entries(entries, sia)
            per_country_stats[country] = {"mean": mean_c, "n": n_c}

            # pool all articles across the country list for row-level mean
            if n_c > 0 and not math.isnan(mean_c):
                # Instead of just averaging means, pool article scores directly:
                # recompute per-entry and extend (so large-country coverage weighs more)
                # To avoid recomputing, we re-score here quickly (still cheap).
                for e in entries:
                    text = (e.get("title","") or "") + "\n" + (e.get("summary","") or "")
                    if not text.strip():
                        continue
                    comp = sia.polarity_scores(text).get("compound", 0.0)
                    total_scores.append(comp)
                total_n += n_c

            time.sleep(REQUEST_SLEEP_SECONDS)

        except Exception as ex:
            errors.append(f"{country}: {type(ex).__name__}: {ex}")

    row_mean = float(sum(total_scores) / len(total_scores)) if total_scores else math.nan
    return row_mean, total_n, per_country_stats, errors

# ---------- Main API ----------
def compute_news_sentiment_for_dataframe(df: pd.DataFrame,
                                         country_list_col: str = COUNTRY_LIST_COL,
                                         start: str = DATE_START,
                                         end: str = DATE_END) -> pd.DataFrame:
    """
    Adds columns to the DataFrame:
        - sent_mean: mean compound sentiment (pooled across articles for all countries in the row)
        - articles_count: total number of articles used
        - per_country_stats: dict {country: {"mean": float, "n": int}}
        - errors: list of error strings (if any)
    """
    # init sentiment
    sia = ensure_vader()

    # simple in-memory cache to avoid repeating the same country/date query
    rss_cache: Dict[str, dict] = {}

    results = {
        "sent_mean": [],
        "articles_count": [],
        "per_country_stats": [],
        "errors": [],
    }

    # validate column
    if country_list_col not in df.columns:
        raise KeyError(f"DataFrame is missing required column: '{country_list_col}'")

    # iterate rows
    for _, row in tqdm(df.iterrows(), total=len(df), desc="Scoring rows"):
        countries = row[country_list_col]
        # normalize types (expect list of strings)
        if isinstance(countries, str):
            # try to load stringified list
            try:
                countries = json.loads(countries)
            except Exception:
                countries = [countries]
        if not isinstance(countries, (list, tuple)):
            countries = [str(countries)]
        countries = [str(c).strip() for c in countries if str(c).strip()]

        row_mean, total_n, per_country, errs = aggregate_row_sentiment(countries, sia, rss_cache)

        results["sent_mean"].append(row_mean)
        results["articles_count"].append(total_n)
        results["per_country_stats"].append(per_country)
        results["errors"].append(errs)

    # attach outputs
    out = df.copy()
    out["sent_mean"] = results["sent_mean"]
    out["articles_count"] = results["articles_count"]
    out["per_country_stats"] = results["per_country_stats"]
    out["errors"] = results["errors"]
    return out

if __name__ == "__main__":
    df = pd.read_csv("/Users/michaelwang/Desktop/unsw/unsw actl/4305/Assignment/actl4305-ggins-datathon/cleaned_destinations_data.csv")

    enriched = compute_news_sentiment_for_dataframe(df, country_list_col="cleaned_dest_list")
    # Show results
    pd.set_option("display.max_colwidth", 120)
    print(enriched)

    enriched.to_csv("google_news_sentiment_attached_data_may_to_oct.csv")
