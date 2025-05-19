# Live Stream Extractor

A minimal command-line tool to check if a YouTube channel is currently live and extract its streaming link.

## Overview

This tool scrapes the YouTube channel page and attempts to extract the live stream link (if available) using the embedded `ytInitialData` JavaScript object.

- Written in **Haskell**.
- Uses **Wreq** for HTTP requests, **HandsomeSoup** for HTML parsing, and **Aeson** for JSON decoding.

## ⚠️ Disclaimer

This tool relies on scraping ytInitialData from YouTube pages. If Google sneezes and reorders a `<div>`, this tool might break.


If it stops working, feel free to:
- Fork it 🪓
- Fix it 🔧
- Pretend you learned something 📚


That last step is optional, but highly encouraged.

## Installation

Clone the repository and build with Stack or Cabal:

```bash
git clone https://github.com/h-alice/hsYtLiveStat
cd hsYtLiveStat
stack build
````

## Usage

```bash
stack run <channel-handle>
```

If the channel is currently live, it will print the live streaming URL.

## License

AGPL-3.0
