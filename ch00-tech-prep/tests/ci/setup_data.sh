#!/usr/bin/env bash
set -euo pipefail

usage() {
  echo "Usage: $0 --kind r|python --os-name linux|macos|windows" >&2
}

KIND=""
OS_NAME=""

while [ "$#" -gt 0 ]; do
  case "$1" in
    --kind)
      KIND="$2"
      shift 2
      ;;
    --os-name)
      OS_NAME="$2"
      shift 2
      ;;
    *)
      usage
      exit 2
      ;;
  esac
done

if [ -z "$KIND" ] || [ -z "$OS_NAME" ]; then
  usage
  exit 2
fi

if [ "$KIND" != "r" ] && [ "$KIND" != "python" ]; then
  usage
  exit 2
fi

REPO_DIR="${GITHUB_WORKSPACE:-$(pwd)}"
DATA_URL="https://files.osf.io/v1/resources/3u5em/providers/osfstorage/?zip="

cd "$REPO_DIR"

curl -sL "$DATA_URL" -o data.zip
unzip -q data.zip
unzip -q da_data_repo.zip

if [ "$KIND" = "r" ]; then
  TARGET_ROOT="$(dirname "$REPO_DIR")"
else
  REPO_DIR_NORMALIZED="$(echo "$REPO_DIR" | sed 's|\\|/|g')"

  if [ "$OS_NAME" = "windows" ]; then
    TARGET_ROOT="/d/a"
  else
    PARENT="$(dirname "$REPO_DIR_NORMALIZED")"
    TARGET_ROOT="$(dirname "$PARENT")"
  fi
fi

rm -rf "$TARGET_ROOT/da_data_repo"
mv da_data_repo "$TARGET_ROOT/"

rm data.zip da_data_repo.zip
rm -rf __MACOSX

if [ "$KIND" = "r" ]; then
  cp ch00-tech-prep/set-data-directory-example.R ch00-tech-prep/set-data-directory.R
  if [ -n "${GITHUB_ENV:-}" ]; then
    echo "DA_DATA_DIR=$TARGET_ROOT/da_data_repo" >> "$GITHUB_ENV"
  fi
else
  if [ "$OS_NAME" != "windows" ]; then
    cd "$PARENT"
    ln -s da_case_studies/ch* . || true
    ln -s da_case_studies/da* . || true
    ln -s da_case_studies/pre* . || true
  fi
fi
