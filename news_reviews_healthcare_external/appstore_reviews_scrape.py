#!/usr/bin/env python3
"""
Fetch Apple App Store reviews via public RSS JSON and add sentiment.
Example:
  python apple_reviews_rss.py --app-id 1542672696 --country au --out freely_reviews_au.csv --max-pages 10
"""

import argparse, csv, time, requests, sys
from datetime import datetime
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer

def fetch_apple_reviews(app_id: str, country: str = "au", max_pages: int = 10, delay_s: float = 0.8, sortby: str = "mostrecent"):
    rows = []
    headers = {"User-Agent": "Mozilla/5.0 (compatible; reviews-fetcher/1.1)"}
    for page in range(1, max_pages + 1):
        url = f"https://itunes.apple.com/{country}/rss/customerreviews/page={page}/id={app_id}/sortby={sortby}/json"
        r = requests.get(url, headers=headers, timeout=25)
        if r.status_code != 200:
            break
        data = r.json()
        entries = data.get("feed", {}).get("entry", [])
        if not entries:
            break
        review_entries = entries[1:] if len(entries) > 1 else []
        if not review_entries:
            break

        for e in review_entries:
            row = {
                "store": "apple",
                "country": country,
                "app_id": app_id,
                "review_id": (e.get("id", {}) or {}).get("label"),
                "author": ((e.get("author", {}) or {}).get("name", {}) or {}).get("label"),
                "title": (e.get("title", {}) or {}).get("label"),
                "rating": (e.get("im:rating", {}) or {}).get("label"),
                "version": (e.get("im:version", {}) or {}).get("label"),
                "vote_count": (e.get("im:voteCount", {}) or {}).get("label"),
                "updated_raw": (e.get("updated", {}) or {}).get("label"),
                "content": (e.get("content", {}) or {}).get("label"),
                "page": page,
            }
            dt = row["updated_raw"]
            if dt:
                try:
                    row["updated_iso"] = datetime.fromisoformat(dt.replace("Z", "+00:00")).isoformat()
                except Exception:
                    row["updated_iso"] = dt
            else:
                row["updated_iso"] = ""
            rows.append(row)

        time.sleep(delay_s)
    return rows

def add_sentiment(rows):
    sia = SentimentIntensityAnalyzer()
    for r in rows:
        text = f"{(r.get('title') or '').strip()} {(r.get('content') or '').strip()}".strip()
        vs = sia.polarity_scores(text or "")
        r["sent_compound"] = vs["compound"]                # -1 .. 1
        r["sent_pos"] = vs["pos"]
        r["sent_neu"] = vs["neu"]
        r["sent_neg"] = vs["neg"]
        r["sent_label"] = "pos" if vs["compound"] >= 0.05 else ("neg" if vs["compound"] <= -0.05 else "neu")
    return rows

def save_csv(rows, out_csv: str):
    if not rows:
        print("No reviews found; nothing to write.")
        return
    cols = [
        "store","country","app_id","review_id","author","title","rating","version","vote_count",
        "updated_iso","updated_raw","content","page",
        "sent_compound","sent_pos","sent_neu","sent_neg","sent_label"
    ]
    with open(out_csv, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=cols)
        w.writeheader()
        for r in rows:
            w.writerow({k: r.get(k, "") for k in cols})
    print(f"Wrote {len(rows)} reviews -> {out_csv}")

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--app-id", required=True)
    ap.add_argument("--country", default="au")
    ap.add_argument("--out", default="apple_reviews.csv")
    ap.add_argument("--max-pages", type=int, default=10)
    ap.add_argument("--delay-s", type=float, default=0.8)
    ap.add_argument("--sortby", default="mostrecent", choices=["mostrecent","mosthelpful"])
    args = ap.parse_args()

    rows = fetch_apple_reviews(
        app_id=args.app_id,
        country=args.country,
        max_pages=args.max_pages,
        delay_s=args.delay_s,
        sortby=args.sortby,
    )
    rows = add_sentiment(rows)
    save_csv(rows, args.out)

if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        sys.exit(130)
