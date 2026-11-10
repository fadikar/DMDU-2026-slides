#!/usr/bin/env python3
"""Export the Reveal.js deck using Chrome DevTools and its CSS print layout.

Usage: uv run --with websockets python render_pdf.py [--rebuild]
Optional: --chrome /path/to/chrome --output /path/to/slides.pdf
"""
import argparse
import asyncio
import base64
import json
import shutil
import socket
import subprocess
import tempfile
import time
import urllib.request
from pathlib import Path

HERE = Path(__file__).resolve().parent


def find_chrome(explicit):
    if explicit:
        candidates = [Path(explicit).expanduser()]
    else:
        candidates = list((Path.home() / 'Library/Application Support/quarto/chrome-headless-shell').glob('**/chrome-headless-shell'))
        candidates += [Path('/Applications/Google Chrome.app/Contents/MacOS/Google Chrome')]
        candidates += [Path(p) for name in ('google-chrome', 'chromium', 'chromium-browser') if (p := shutil.which(name))]
    for path in candidates:
        if path.is_file():
            return path
    raise RuntimeError('Chrome not found. Pass --chrome /path/to/chrome.')


async def export(args):
    try:
        import websockets
    except ImportError as exc:
        raise RuntimeError('Run: uv run --with websockets python render_pdf.py') from exc
    chrome = find_chrome(args.chrome)
    if args.rebuild:
        subprocess.run(['quarto', 'render', 'index.qmd'], cwd=HERE, check=True)
    source = HERE / 'docs/index.html'
    if not source.is_file():
        raise RuntimeError('Run quarto render index.qmd first, or pass --rebuild.')
    output = Path(args.output).resolve()
    # Select a free port and isolate this export from any interactive browser.
    with socket.socket() as sock:
        sock.bind(('127.0.0.1', 0))
        port = sock.getsockname()[1]
    with tempfile.TemporaryDirectory(prefix='dmdu-pdf-') as profile:
        proc = subprocess.Popen([str(chrome), '--headless', '--disable-gpu', '--no-first-run',
                                 f'--remote-debugging-port={port}', f'--user-data-dir={profile}',
                                 'about:blank'], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        try:
            deadline = time.monotonic() + 20
            while True:
                try:
                    with urllib.request.urlopen(f'http://127.0.0.1:{port}/json', timeout=1) as response:
                        targets = json.load(response)
                    target = next(t for t in targets if t.get('type') == 'page')
                    break
                except (OSError, ValueError, StopIteration):
                    if proc.poll() is not None or time.monotonic() > deadline:
                        raise RuntimeError('Chrome did not start its debugging endpoint.')
                    await asyncio.sleep(.2)
            async with websockets.connect(target['webSocketDebuggerUrl'], max_size=128 * 1024 * 1024) as ws:
                msg_id = 0

                async def send(method, params=None):
                    nonlocal msg_id
                    msg_id += 1
                    await ws.send(json.dumps({'id': msg_id, 'method': method, 'params': params or {}}))
                    while True:
                        msg = json.loads(await asyncio.wait_for(ws.recv(), timeout=60))
                        if msg.get('id') == msg_id:
                            if 'error' in msg:
                                raise RuntimeError(f'{method}: {msg["error"]}')
                            return msg.get('result', {})

                async def evaluate(expression):
                    result = await send('Runtime.evaluate', {'expression': expression, 'awaitPromise': True, 'returnByValue': True})
                    if 'exceptionDetails' in result:
                        raise RuntimeError(str(result['exceptionDetails']))
                    return result.get('result', {}).get('value')

                await send('Page.enable')
                await send('Emulation.setDeviceMetricsOverride', {'width': 1440, 'height': 810, 'deviceScaleFactor': 1, 'mobile': False})
                await send('Page.navigate', {'url': source.as_uri() + '?print-pdf'})
                deadline = time.monotonic() + 60
                while not await evaluate("document.readyState === 'complete' && typeof Reveal !== 'undefined' && Reveal.isReady() && document.querySelectorAll('.pdf-page').length > 0"):
                    if time.monotonic() > deadline:
                        raise RuntimeError('Timed out waiting for Reveal.js print layout.')
                    await asyncio.sleep(.25)
                await evaluate("document.fonts.ready.then(() => Promise.all([...document.images].map(i => i.complete ? Promise.resolve() : new Promise(r => {i.onload = i.onerror = r}))))")
                await evaluate("new Promise(resolve => {if(window.MathJax?.Hub) MathJax.Hub.Queue(resolve); else if(window.MathJax?.startup?.promise) MathJax.startup.promise.then(resolve); else resolve()})")
                await asyncio.sleep(1)
                missing = await evaluate("[...document.images].filter(i => !i.naturalWidth).map(i => i.getAttribute('src'))")
                if missing:
                    raise RuntimeError(f'Images failed to load: {missing}')
                result = await send('Page.printToPDF', {'printBackground': True, 'preferCSSPageSize': True, 'scale': 1,
                                                       'marginTop': 0, 'marginBottom': 0, 'marginLeft': 0, 'marginRight': 0})
                if not result.get('data'):
                    raise RuntimeError('Chrome returned no PDF data.')
                output.parent.mkdir(parents=True, exist_ok=True)
                output.write_bytes(base64.b64decode(result['data']))
                print(f'Wrote {output}')
        finally:
            proc.terminate()
            try:
                proc.wait(timeout=10)
            except subprocess.TimeoutExpired:
                proc.kill()
                proc.wait()


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--rebuild', action='store_true', help='Render index.qmd before exporting')
    parser.add_argument('--chrome', help='Chrome or chrome-headless-shell executable')
    parser.add_argument('--output', default=str(HERE / 'docs/DMDU-2026-slides.pdf'))
    asyncio.run(export(parser.parse_args()))
