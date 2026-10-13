#!/usr/bin/env python3
"""Smoke-check that the emma_envdata release/STAC contract assets resolve.

Run from repo root:
  python3 scripts/validate_envdata_contract.py
"""

from __future__ import annotations

import json
import sys
import urllib.error
import urllib.request

BASE = "https://github.com/eco-emma/emma_envdata/releases/download"

REQUIRED = [
    f"{BASE}/stac/catalog.json",
    f"{BASE}/stac-vi/vi_collection.json",
    f"{BASE}/stac-burn/burn_recentburn.json",
    f"{BASE}/static_data/domain.tif",
    f"{BASE}/static_data/static_covariates.parquet",
    f"{BASE}/fireage/fireage_current.tif",
]


def head_ok(url: str, timeout: int = 60) -> tuple[bool, str]:
    req = urllib.request.Request(url, method="GET")
    try:
        with urllib.request.urlopen(req, timeout=timeout) as resp:
            # Read a small prefix so we fail on empty / HTML error pages
            chunk = resp.read(256)
            if not chunk:
                return False, "empty body"
            return True, f"ok ({resp.status}, {len(chunk)}+ bytes)"
    except urllib.error.HTTPError as e:
        return False, f"HTTP {e.code}"
    except Exception as e:  # noqa: BLE001
        return False, str(e)


def main() -> int:
    failures = 0
    for url in REQUIRED:
        ok, detail = head_ok(url)
        print(f"{'PASS' if ok else 'FAIL'}: {url} — {detail}")
        failures += int(not ok)

    # Discover latest Terra VI from STAC and check its asset
    coll_url = f"{BASE}/stac-vi/vi_collection.json"
    try:
        with urllib.request.urlopen(coll_url, timeout=60) as resp:
            collection = json.load(resp)
        items = [
            link["href"]
            for link in collection.get("links", [])
            if link.get("rel") == "item"
        ]
        if not items:
            print("FAIL: no VI STAC items")
            failures += 1
        else:
            latest = sorted(items)[-1]
            with urllib.request.urlopen(latest, timeout=60) as resp:
                item = json.load(resp)
            href = item["assets"]["modis_terra_vi"]["href"]
            ok, detail = head_ok(href)
            print(f"{'PASS' if ok else 'FAIL'}: latest Terra VI {href} — {detail}")
            failures += int(not ok)
    except Exception as e:  # noqa: BLE001
        print(f"FAIL: STAC VI discovery — {e}")
        failures += 1

    if failures:
        print(f"\n{failures} check(s) failed", file=sys.stderr)
        return 1
    print("\nAll envdata contract checks passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
