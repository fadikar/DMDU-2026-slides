# Slides for DMDU annual meeting 2026

The slides are visible [here](https://fadikar.com/DMDU-2026-slides/#/title-slide)

## Export PDF

Export the current rendered slides with Chrome's CSS page size preserved:

```bash
uv run --with websockets python render_pdf.py
```

Rebuild the Quarto deck first and then export:

```bash
uv run --with websockets python render_pdf.py --rebuild
```

Output: `docs/DMDU-2026-slides.pdf`. The script uses Quarto's headless
Chrome when available, falling back to installed Google Chrome/Chromium.
Pass `--chrome /path/to/chrome` to select an executable explicitly.
