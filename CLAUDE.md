# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Issue Tracking

Uses beads (`bd`) for issue tracking. Issues prefixed `HN-`.

## Build Commands

```bash
# Enter nix development shell (provides cabal, HLS, ormolu)
nix develop

# Build
cabal build

# Run
cabal run hacker-news

# Format
ormolu --mode inplace app/**/*.hs
```

## Architecture

TUI application for browsing Hacker News "Who's Hiring" threads. Fetches thread data from Algolia's HN API, parses job posts, and highlights salient information (salary, location, remote status, etc.) using regex matching.

**Core modules:**

- `HNI.Fetch` - HTTP fetching from `hn.algolia.com/api/v1/items/<id>` with file-based caching in `/tmp/hn-cache/`
- `HNI.Post` - Aeson-derived `Post body` type representing HN comments (recursive via `children`)
- `HNI.Decoded` - HTML entity decoding and tag stripping for raw post text
- `HNI.Salience` - Regex-based extraction of job-relevant spans (salary, location, remote, URLs, emails)
- `HNI.App` - Brick TUI: navigation (n/p), copy text (w), open in browser (o)
- `Brick.Span` - Text span operations for highlighting regions
- `Brick.Highlight` - Word-wrapping widget that preserves highlight spans across line breaks
