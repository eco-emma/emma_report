#!/usr/bin/env bash
# Orphan-reset a bloated *-runs cache branch to a single thin tip.
#
# Phase D0 / R4b follow-up (ops). The CI upload step already stops further
# Quarto bloat; this script reclaims historical blobs on a runs ref by
# force-pushing a new orphan history that contains ONLY:
#   - _targets/   (Git LFS objects)
#   - .gitattributes
#
# Does NOT rewrite main / master / feature branches.
#
# Usage (from a clone of eco-emma/emma_report, with write access):
#   ./scripts/orphan-reset-runs-branch.sh main-runs
#   ./scripts/orphan-reset-runs-branch.sh 2025_website-runs
#   ./scripts/orphan-reset-runs-branch.sh --delete-stale targets-runs
#
# Dry-run (no push):
#   DRY_RUN=1 ./scripts/orphan-reset-runs-branch.sh main-runs
#
# Safety:
#   - Requires explicit branch name ending in -runs
#   - Requires CONFIRM=yes for the force-push
#   - Refuses main/master/gh-pages and any non-*-runs ref

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

DRY_RUN="${DRY_RUN:-0}"
CONFIRM="${CONFIRM:-}"
DELETE_STALE=0

usage() {
  sed -n '2,25p' "$0" | sed 's/^# \{0,1\}//'
  exit 1
}

if [[ "${1:-}" == "--delete-stale" ]]; then
  DELETE_STALE=1
  shift
fi

BRANCH="${1:-}"
if [[ -z "$BRANCH" ]]; then
  usage
fi

if [[ ! "$BRANCH" =~ -runs$ ]]; then
  echo "ERROR: ref must end in -runs (got: $BRANCH)" >&2
  exit 1
fi

case "$BRANCH" in
  main|master|gh-pages|dev-adam|2025_website)
    echo "ERROR: refusing to touch source/Pages branch: $BRANCH" >&2
    exit 1
    ;;
esac

if [[ "$DELETE_STALE" -eq 1 ]]; then
  echo "Will delete origin/$BRANCH (stale runs cache)."
  if [[ "$DRY_RUN" == "1" ]]; then
    echo "DRY_RUN=1: would run: git push origin --delete $BRANCH"
    exit 0
  fi
  if [[ "$CONFIRM" != "yes" ]]; then
    echo "Re-run with CONFIRM=yes to delete origin/$BRANCH" >&2
    exit 1
  fi
  git push origin --delete "$BRANCH"
  echo "Deleted origin/$BRANCH"
  exit 0
fi

TMP="$(mktemp -d "${TMPDIR:-/tmp}/emma-runs-orphan.XXXXXX")"
cleanup() { rm -rf "$TMP"; }
trap cleanup EXIT

echo "==> Fetching origin/$BRANCH (shallow tip for _targets copy)"
git fetch --depth=1 origin "$BRANCH"

if ! git show-ref --verify --quiet "refs/remotes/origin/$BRANCH"; then
  echo "ERROR: origin/$BRANCH not found" >&2
  exit 1
fi

echo "==> Cloning tip worktree into $TMP"
git worktree add --detach "$TMP" "origin/$BRANCH"
(
  cd "$TMP"
  git lfs checkout || true
  if [[ ! -d _targets ]]; then
    echo "ERROR: origin/$BRANCH tip has no _targets/; aborting (would wipe cache)" >&2
    exit 1
  fi
)

WORK="$TMP/orphan-build"
mkdir -p "$WORK"
cp -a "$TMP/_targets" "$WORK/_targets"
(
  cd "$WORK"
  git init -b "$BRANCH"
  git lfs install --local
  git lfs track "_targets/objects/**"
  git add -f .gitattributes _targets
  git -c user.name="EMMA ops" -c user.email="actions@github.com" \
    commit -m "Orphan-reset: thin targets cache only (Phase D0 / R4b)"

  echo "==> New orphan tip tree:"
  git ls-tree -r --name-only HEAD | head -50
  echo "… ($(git ls-tree -r --name-only HEAD | wc -l) paths)"

  if git ls-tree -r --name-only HEAD | grep -E '^(docs/|reports/|\.quarto/|site_libs/)' ; then
    echo "ERROR: orphan tip still contains website paths" >&2
    exit 1
  fi

  if [[ "$DRY_RUN" == "1" ]]; then
    echo "DRY_RUN=1: skipping force-push of $BRANCH"
    exit 0
  fi
  if [[ "$CONFIRM" != "yes" ]]; then
    echo "Re-run with CONFIRM=yes to force-push orphan $BRANCH to origin" >&2
    echo "  CONFIRM=yes $0 $BRANCH" >&2
    exit 1
  fi

  git remote add origin "$(git -C "$REPO_ROOT" remote get-url origin)"
  echo "==> Force-pushing orphan $BRANCH (history rewrite of runs ref only)"
  git push --force origin "HEAD:refs/heads/$BRANCH"
)

echo "Done. origin/$BRANCH tip is thin _targets-only."
echo "GitHub GC / LFS unreferenced cleanup may take time to shrink diskUsage."
echo "Optional: delete other stale *-runs with:"
echo "  CONFIRM=yes $0 --delete-stale targets-runs"
