#!/usr/bin/env bash
set -uo pipefail

# This script installs system dependencies (depexts) listed by `dune show depexts`.
# It is intended to be called from the setup-dune composite action.

echo "[install-depexts] Starting."

: "${RUNNER_OS:?RUNNER_OS must be set}"

DEPEXT_OUTPUT="$(dune show depexts 2>&1)"
DEPEXT_EXIT_CODE=$?

if [ "$DEPEXT_EXIT_CODE" -ne 0 ]; then
  echo "$DEPEXT_OUTPUT"
  echo "[install-depexts] dune show depexts exit code: $DEPEXT_EXIT_CODE"
  exit "$DEPEXT_EXIT_CODE"
fi

DEPEXT_LIST="$DEPEXT_OUTPUT"

echo "[install-depexts] DEPEXT_LIST='$(printf '%s' "$DEPEXT_LIST" | sed ':a;N;$!ba;s/\n/\\n/g')'"

if [ -z "$DEPEXT_LIST" ]; then
  echo "[install-depexts] No depexts to install."
  exit 0
fi

if [ "${RUNNER_OS}" = "Linux" ]; then
  echo "[install-depexts] Installing depexts with apt-get:"
  echo "$DEPEXT_LIST"
  sudo apt-get update
  xargs -r sudo apt-get install -y <<< "$DEPEXT_LIST"
elif [ "${RUNNER_OS}" = "macOS" ]; then
  echo "[install-depexts] Installing depexts with brew:"
  echo "$DEPEXT_LIST"
  brew update
  xargs -r brew install <<< "$DEPEXT_LIST"
else
  echo "[install-depexts] Depext installation is not supported on this OS: ${RUNNER_OS}"
  exit 0
fi
