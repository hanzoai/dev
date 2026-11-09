# Every call release.yml makes to GitHub goes through ghapi, so no step parses
# an answer it has not checked and every failure says what GitHub said. Sourced,
# not run; it needs GH_PAT in the environment.
#
#   ghapi METHOD URL [curl options]   the body on stdout, when the status is 2xx
#   ghapi_budget NEED                 logs what is left of the hourly budget
#
#   GHAPI_ACCEPT  the Accept header; a download needs application/octet-stream
#                 ALONE, beside the default GitHub answers with metadata JSON
#   GHAPI_TO      write the body to this file instead of stdout
#   GHAPI_TRIES   attempts, default 5; 1 for a call a repeat would double
#   GHAPI_WAIT    the longest single wait in seconds, default 600
#
# A 2xx returns. A 403 or 429 that carries retry-after, or x-ratelimit-remaining
# of 0, is the rate limit and was refused unprocessed: ghapi waits for what
# GitHub says (retry-after, else x-ratelimit-reset) and asks again. It never
# waits longer than GHAPI_WAIT at once: a runner that holds a node for the hour
# the budget needs is worse than a rerun, so past that the call fails and says
# when the limit lifts. A 5xx or a lost connection is asked again after 2, 8, 18
# and 32 seconds. Anything else (a 403 that is not the limit, a 404, a 422) is
# GitHub's answer and fails at once, naming the status and its message.
#
# The token is the account's, 5,000 calls an hour shared with every other tool on
# it. One release makes about 66: 3 in `version`, 33 in the five builds (a list
# and an upload per file, 6 files each, windows 4), 30 in `publish` (a list, 28
# downloads, the publish itself).

ghapi() {
  local method=$1 url=$2; shift 2
  local tries=${GHAPI_TRIES:-5} cap=${GHAPI_WAIT:-600} try=1
  local hdr body out code msg note pause retry left reset
  hdr=$(mktemp); body=$(mktemp); out=${GHAPI_TO:-$body}
  while :; do
    code=$(curl -sS -m 900 -D "$hdr" -o "$out" -w '%{http_code}' -X "$method" \
      -H "Authorization: Bearer $GH_PAT" -H "Accept: ${GHAPI_ACCEPT:-application/vnd.github+json}" \
      "$@" "$url" 2>"$body.err") || code=000
    case $code in
      2??) [ -n "${GHAPI_TO:-}" ] || cat "$body"; rm -f "$hdr" "$body" "$body.err"; return 0 ;;
    esac
    msg=$(jq -r 'if type == "object" then .message // empty else empty end' "$out" 2>/dev/null || true)
    [ -n "$msg" ] || msg=$(cat "$out" "$body.err" 2>/dev/null | head -c 200 | tr '\r\n' '  ')
    pause=-1 note=
    case $code in
      403|429)
        retry=$(awk 'tolower($1) == "retry-after:" { v = $2 + 0 } END { print v }' "$hdr")
        left=$(awk 'tolower($1) == "x-ratelimit-remaining:" { v = $2 + 0 } END { print v }' "$hdr")
        reset=$(awk 'tolower($1) == "x-ratelimit-reset:" { v = $2 + 0 } END { print v }' "$hdr")
        if [ -n "$retry" ]; then
          pause=$((retry + 1)); note=" (rate limited; retry-after ${retry}s)"
        elif [ "$left" = 0 ] && [ -n "$reset" ]; then
          pause=$((reset - $(date +%s) + 1)); [ "$pause" -ge 1 ] || pause=1
          note=" (rate limit spent; it lifts at $(date -u -d "@$reset" +%T) UTC)"
        fi ;;
      000|5??) pause=$((try * try * 2)) ;;
    esac
    if [ "$pause" -ge 0 ] && [ "$pause" -le "$cap" ] && [ "$try" -lt "$tries" ]; then
      echo "::notice::$method ${url%%\?*} -> HTTP $code: ${msg:-no message}$note; asking again in ${pause}s (attempt $try of $tries)" >&2
      sleep "$pause"; try=$((try + 1)); continue
    fi
    if [ "$pause" -gt "$cap" ]; then note="$note; GitHub asks for ${pause}s, more than the ${cap}s this waits"
    elif [ "$pause" -ge 0 ]; then note="$note; gave up after $try attempts"; fi
    echo "::error::$method ${url%%\?*} -> HTTP $code: ${msg:-no message}$note" >&2
    rm -f "$hdr" "$body" "$body.err"
    return 1
  done
}

# ghapi_budget NEED: says what is left of the hourly budget before any build is
# paid for. With less than NEED it waits for the reset when that is within
# GHAPI_WAIT, and otherwise fails. The endpoint itself is not counted.
ghapi_budget() {
  local need=$1 left limit reset pause
  read -r left limit reset < <(ghapi GET https://api.github.com/rate_limit \
    | jq -r '.resources.core | "\(.remaining) \(.limit) \(.reset)"') || return 1
  echo "GitHub budget: $left of $limit calls left, resetting at $(date -u -d "@$reset" +%T) UTC; this release needs about $need"
  [ "$left" -ge "$need" ] && return 0
  pause=$((reset - $(date +%s) + 1))
  if [ "$pause" -le "${GHAPI_WAIT:-600}" ]; then
    echo "::notice::waiting ${pause}s for the budget to reset"; sleep "$pause"; return 0
  fi
  echo "::error::GitHub budget is $left of $limit calls and resets in ${pause}s, and this release needs about $need" >&2
  return 1
}
