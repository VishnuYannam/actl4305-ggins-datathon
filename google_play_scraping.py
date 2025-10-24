#!/usr/bin/env python3
"""
Fetch Google Play reviews for a package and add VADER sentiment.
Example:
  python gp_reviews.py --package com.freely.prod --country au --out gp_reviews_freely_au.csv --max 2000
"""

import argparse, csv, time
from datetime import datetime, timezone
from collections import OrderedDict
from google_play_scraper import reviews, Sort
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer

def fetch_play_reviews(package: str, country: str = "au", lang: str = "en",
                       n: int = 1000, batch: int = 200, sleep_s: float = 1.0):
    """
    Pull up to n reviews, newest-first, with continuation tokens.
    """
    all_rows, token = [], None
    got = 0
    while got < n:
        count = min(batch, n - got)
        result, token = reviews(
            package,
            lang=lang,
            country=country,
            sort=Sort.NEWEST,
            count=count,
            continuation_token=token
        )
        if not result:
            break
        for r in result:
            all_rows.append({
                "store": "google_play",
                "country": country,
                "package": package,
                "reviewId": r.get("reviewId"),
                "userName": r.get("userName"),
                "score": r.get("score"),
                "at": r.get("at").astimezone(timezone.utc).isoformat() if r.get("at") else "",
                "content": r.get("content"),
                "replyContent": r.get("replyContent"),
                "version": r.get("reviewCreatedVersion"),
                "thumbsUp": r.get("thumbsUpCount"),
            })
        got += len(result)
        if token is None:   # no more pages
            break
        time.sleep(sleep_s)  # be polite
    return all_rows

def add_sentiment(rows):
    sia = SentimentIntensityAnalyzer()
    for r in rows:
        text = (r.get("content") or "").strip()
        vs = sia.polarity_scores(text)
        r["sent_compound"] = vs["compound"]
        r["sent_pos"] = vs["pos"]
        r["sent_neu"] = vs["neu"]
        r["sent_neg"] = vs["neg"]
        r["sent_label"] = "pos" if vs["compound"] >= 0.05 else ("neg" if vs["compound"] <= -0.05 else "neu")
    return rows

def dedupe_by_id(rows, key="reviewId"):
    seen = OrderedDict()
    for r in rows:
        k = r.get(key)
        if k and k not in seen:
            seen[k] = r
    return list(seen.values())

def save_csv(rows, out_csv):
    if not rows:
        print("No reviews found; nothing to write.")
        return
    cols = ["store","country","package","reviewId","userName","score","at","content",
            "replyContent","version","thumbsUp","sent_compound","sent_pos","sent_neu","sent_neg","sent_label"]
    with open(out_csv, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=cols)
        w.writeheader()
        for r in rows:
            w.writerow({k: r.get(k, "") for k in cols})
    print(f"Wrote {len(rows)} reviews -> {out_csv}")

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--package", required=True, help="Google Play package name, e.g., com.freely.prod")
    ap.add_argument("--country", default="au", help="2-letter country code, e.g., au, us, gb")
    ap.add_argument("--lang", default="en", help="Language code, e.g., en, en_AU")
    ap.add_argument("--out", default="gp_reviews.csv")
    ap.add_argument("--max", type=int, default=1000, help="Max reviews to fetch")
    ap.add_argument("--batch", type=int, default=200, help="Per-call fetch size")
    ap.add_argument("--sleep-s", type=float, default=1.0)
    args = ap.parse_args()

    rows = fetch_play_reviews(args.package, args.country, args.lang, args.max, args.batch, args.sleep_s)
    rows = dedupe_by_id(rows)
    rows = add_sentiment(rows)
    save_csv(rows, args.out)

if __name__ == "__main__":
    main()
