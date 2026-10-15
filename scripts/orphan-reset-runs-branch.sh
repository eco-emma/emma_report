#!/usr/bin/env bash
# Orphan-reset a bloated *-runs cache branch to a single thin tip.
#
# Phase D0 / R4b follow-up (ops). The CI upload step already stops further
# Quarto bloat; this script reclaims historical blobs on a runs ref by
# force-pushing a new orphan history that contains ONLY:
#   - _targets/   (regular git blobs — not Git LFS)
#   - .gitattributes (no LFS filters)
#
# Does NOT rewrite main / master / feature branches.
#
# Why no LFS: eco-emma org LFS budget can be exhausted. Re-tracking
# _targets/objects/** as LFS makes force-push attempt LFS uploads and fail
# with "This repository exceeded its LFS budget". Thin tips push as normal
# git objects instead. Raising/clearing the org LFS budget is still needed
# if you want GitHub to drop *old* unreferenced LFS blobs from billing.
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

# Skip LFS smudge on fetch/clone so missing git-lfs (or exhausted LFS budget)
# cannot abort the tip copy. Optional smudge runs later only if git-lfs exists.
export GIT_LFS_SKIP_SMUDGE="${GIT_LFS_SKIP_SMUDGE:-1}"

usage() {
  sed -n '2,30p' "$0" | sed 's/^# \{0,1\}//'
  exit 1
}

have_git_lfs() {
  command -v git-lfs >/dev/null 2>&1 || git lfs version >/dev/null 2>&1
}

is_lfs_pointer() {
  local f="$1"
  [[ -f "$f" ]] || return 1
  # Git LFS pointer files start with this header (small text files).
  head -n 1 "$f" 2>/dev/null | grep -qx 'version https://git-lfs.github.com/spec/v1'
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

echo "==> Fetching origin/$BRANCH (shallow tip for _targets copy; LFS smudge skipped)"
git fetch --depth=1 origin "$BRANCH"

if ! git show-ref --verify --quiet "refs/remotes/origin/$BRANCH"; then
  echo "ERROR: origin/$BRANCH not found" >&2
  exit 1
fi

echo "==> Cloning tip worktree into $TMP"
git worktree add --detach "$TMP" "origin/$BRANCH"
(
  cd "$TMP"
  # Best-effort materialize LFS payloads when git-lfs is installed and budget
  # still allows downloads. Never fail the script if LFS is missing/broken.
  if have_git_lfs; then
    git lfs install --local --skip-smudge >/dev/null 2>&1 || true
    git lfs checkout 2>/dev/null || true
  else
    echo "NOTE: git-lfs not installed; keeping pointer files as-is (GIT_LFS_SKIP_SMUDGE=1)"
  fi
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

  # Explicitly do NOT run git lfs track. Write a non-LFS .gitattributes so the
  # force-push is a normal git push (no LFS batch upload → works when org LFS
  # budget is exhausted).
  cat > .gitattributes <<'EOF'
# *-runs thin tip: store _targets as regular git blobs (no Git LFS).
# Orphan-reset and CI upload avoid LFS so force-push/upload works when the
# org LFS budget is exhausted. Old LFS history may still bill until the
# org budget is raised/cleared and GitHub GCs unreferenced LFS objects.
EOF

  # If objects are still LFS pointers, strip the filter attrs is not enough —
  # the file contents are pointer text. Refuse to force-push that tip: CI would
  # restore pointer bytes and tar_load → readRDS "unknown input format".
  pointer_count=0
  if [[ -d _targets/objects ]]; then
    while IFS= read -r -d '' f; do
      if is_lfs_pointer "$f"; then
        pointer_count=$((pointer_count + 1))
      fi
    done < <(find _targets/objects -type f -print0 2>/dev/null || true)
  fi
  if [[ "$pointer_count" -gt 0 ]]; then
    echo "ERROR: $pointer_count _targets/objects file(s) are still Git LFS pointers." >&2
    echo "  Refusing to orphan-reset a tip that would ship pointer text as blobs." >&2
    echo "  Do not re-orphan yet. Run targets-data (bash upload) so *-runs gets" >&2
    echo "  real objects as normal git blobs, then orphan-reset that clean tip." >&2
    exit 1
  fi

  git add -f .gitattributes _targets
  git -c user.name="EMMA ops" -c user.email="actions@github.com" \
    commit -m "Orphan-reset: thin targets cache only (no Git LFS; Phase D0 / R4b)"

  echo "==> New orphan tip tree:"
  git ls-tree -r --name-only HEAD | head -50
  echo "… ($(git ls-tree -r --name-only HEAD | wc -l) paths)"

  if git ls-tree -r --name-only HEAD | grep -E '^(docs/|reports/|\.quarto/|site_libs/)' ; then
    echo "ERROR: orphan tip still contains website paths" >&2
    exit 1
  fi

  # Refuse to ship LFS filter rules on the orphan tip (would re-trigger uploads).
  if grep -E 'filter=lfs|diff=lfs|merge=lfs' .gitattributes >/dev/null 2>&1; then
    echo "ERROR: .gitattributes still contains LFS filters; aborting" >&2
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
  echo "==> Force-pushing orphan $BRANCH (history rewrite of runs ref only; no LFS)"
  # Disable LFS push hooks if somehow present in the environment.
  git -c lfs.allowincompletepush=true push --force origin "HEAD:refs/heads/$BRANCH"
)

echo "Done. origin/$BRANCH tip is thin _targets-only (regular git blobs, no LFS)."
echo "Git history bloat (Quarto on old commits) is dropped from the ref tip."
echo "Org LFS billing for *old* unreferenced LFS objects may remain until the"
echo "eco-emma LFS budget is raised/cleared and GitHub GCs them — that is separate"
echo "from this force-push succeeding."
echo "Optional: delete other stale *-runs with:"
echo "  CONFIRM=yes $0 --delete-stale targets-runs"
