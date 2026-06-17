#!/usr/bin/env python3

import argparse
import fnmatch
import subprocess
import sys
from pathlib import PurePosixPath


ZERO_SHA = "0" * 40

SHARED_PATTERNS = {
    "r": (
        "renv.lock",
        ".github/workflows/test_r_scripts.yml",
        "ch00-tech-prep/tests/run_all_r.sh",
        "ch00-tech-prep/*.R",
        "ch00-tech-prep/*.r",
        "ch00-tech-prep/tests/ci/*",
    ),
    "notebooks": (
        ".python-version",
        "pyproject.toml",
        "uv.lock",
        ".github/workflows/test_notebooks.yml",
        "ch00-tech-prep/*.py",
        "ch00-tech-prep/tests/ci/*",
    ),
}

TARGET_PATTERNS = {
    "r": ("*.R", "*.r"),
    "notebooks": ("*.ipynb", "*.py"),
}


def log(message):
    print(message, file=sys.stderr)


def is_zero_sha(value):
    return bool(value) and set(value) == {"0"}


def run_git(args):
    return subprocess.run(
        ["git", *args],
        check=False,
        capture_output=True,
        text=True,
    )


def changed_files(base, head):
    if not base or is_zero_sha(base):
        result = run_git(["diff-tree", "--no-commit-id", "--name-only", "-r", head])
        if result.returncode != 0:
            log(f"Could not inspect {head}; running full suite.")
            return None
        return result.stdout.splitlines()

    result = run_git(["diff", "--name-only", base, head])
    if result.returncode != 0:
        log(f"Could not diff {base}..{head}; running full suite.")
        return None

    return result.stdout.splitlines()


def matches_any(path, patterns):
    return any(fnmatch.fnmatchcase(path, pattern) for pattern in patterns)


def target_directory(path):
    parent = PurePosixPath(path).parent
    return "." if str(parent) == "." else str(parent)


def select_targets(kind, files):
    targets = set()

    for path in files:
        if not path:
            continue

        if matches_any(path, SHARED_PATTERNS[kind]):
            return True, []

        if matches_any(path, TARGET_PATTERNS[kind]):
            targets.add(target_directory(path))

    return False, sorted(targets)


def emit_outputs(run_all, targets):
    print(f"run_all={str(run_all).lower()}")

    if targets:
        print("targets<<EOF")
        for target in targets:
            print(target)
        print("EOF")

    if run_all:
        log("Target mode: all")
    elif targets:
        log("Target directories:")
        for target in targets:
            log(f"  {target}")
    else:
        log("No eligible test targets selected.")


def parse_args():
    parser = argparse.ArgumentParser(
        description="Select targeted CI tests for Data Analysis case studies."
    )
    parser.add_argument("--kind", choices=sorted(SHARED_PATTERNS), required=True)
    parser.add_argument("--event-name", required=True)
    parser.add_argument("--base", default="")
    parser.add_argument("--head", required=True)
    return parser.parse_args()


def main():
    args = parse_args()

    if args.event_name == "workflow_dispatch":
        emit_outputs(run_all=True, targets=[])
        return 0

    files = changed_files(args.base, args.head)
    if files is None:
        emit_outputs(run_all=True, targets=[])
        return 0

    run_all, targets = select_targets(args.kind, files)
    emit_outputs(run_all=run_all, targets=targets)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
