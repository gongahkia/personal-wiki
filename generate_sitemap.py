#!/usr/bin/env python3
"""generate sitemap.xml from all HTML pages in the site"""
import os
import subprocess
from pathlib import Path
from datetime import datetime, timezone
BASE_URL = "https://gabrielongzm.com"
ROOT = Path(__file__).parent
def git_lastmod(filepath):
    """get last modified date from git history"""
    try:
        result = subprocess.run(
            ["git", "log", "-1", "--format=%aI", "--", str(filepath)],
            capture_output=True, text=True, cwd=ROOT
        )
        if result.stdout.strip():
            return result.stdout.strip()[:10] # YYYY-MM-DD
    except Exception:
        pass
    return datetime.now(timezone.utc).strftime("%Y-%m-%d")
def collect_urls():
    """collect all indexable URLs with lastmod dates"""
    urls = []
    static_pages = [ # (filepath, url_path, priority, changefreq)
        ("index.html", "/", "1.0", "weekly"),
        ("blog/index.html", "/blog/", "0.8", "weekly"),
        ("personal-wiki/index.html", "/personal-wiki/", "0.8", "weekly"),
    ]
    for filepath, url_path, priority, changefreq in static_pages:
        full_path = ROOT / filepath
        if full_path.exists():
            urls.append({
                "loc": f"{BASE_URL}{url_path}",
                "lastmod": git_lastmod(full_path),
                "priority": priority,
                "changefreq": changefreq,
            })
    blog_posts = sorted((ROOT / "blog" / "posts").glob("*.html"))
    for post in blog_posts:
        urls.append({
            "loc": f"{BASE_URL}/blog/posts/{post.name}",
            "lastmod": git_lastmod(post),
            "priority": "0.6",
            "changefreq": "monthly",
        })
    wiki_pages = sorted((ROOT / "personal-wiki" / "pages").glob("*.html"))
    for page in wiki_pages:
        urls.append({
            "loc": f"{BASE_URL}/personal-wiki/pages/{page.name}",
            "lastmod": git_lastmod(page),
            "priority": "0.5",
            "changefreq": "monthly",
        })
    return urls
def generate_xml(urls):
    """generate sitemap XML string"""
    lines = ['<?xml version="1.0" encoding="UTF-8"?>']
    lines.append('<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">')
    for url in urls:
        lines.append("  <url>")
        lines.append(f"    <loc>{url['loc']}</loc>")
        lines.append(f"    <lastmod>{url['lastmod']}</lastmod>")
        lines.append(f"    <changefreq>{url['changefreq']}</changefreq>")
        lines.append(f"    <priority>{url['priority']}</priority>")
        lines.append("  </url>")
    lines.append("</urlset>")
    return "\n".join(lines) + "\n"
def main():
    urls = collect_urls()
    xml = generate_xml(urls)
    output = ROOT / "sitemap.xml"
    with open(output, "w", encoding="utf-8") as f:
        f.write(xml)
    print(f"Generated sitemap.xml with {len(urls)} URLs")
if __name__ == "__main__":
    main()
