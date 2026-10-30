#!/usr/bin/env python3
"""Run from the project folder (needs err.bib and about.qmd next to it).
1. repairs err.bib (one corrupted entry, one name-splitting "and")
2. wires the bibliography into _quarto.yml, the country template and index.qmd
Safe to run more than once."""
import os, sys

def sub(path, old, new):
    s = open(path).read()
    assert s.count(old) == 1, f"{path}: expected text not found: {old[:60]!r}"
    open(path, 'w').write(s.replace(old, new))

for f in ['err.bib', 'about.qmd', '_quarto.yml', 'index.qmd', 'templates/country.qmd.tpl']:
    if not os.path.exists(f):
        sys.exit(f"missing {f} - run this from the project folder and copy err.bib and about.qmd there first")

# ---- 1. repair err.bib ------------------------------------------------------
s = open('err.bib').read()
# reinhart2000mirage had a different paper (Tovar & Mohd Nor 2018) pasted into it
if '@techreport{tovar2018reserve' not in s and '@article{reinhart2000mirage,' in s:
    start = s.index('@article{reinhart2000mirage,')
    mid   = s.index('  title = {Reserve Currency Blocs', start)
    tail  = s.index('  pages={65--70},', mid)
    end   = s.index('\n}\n', tail) + 3
    rein = ("@article{reinhart2000mirage,\n"
            "  title={The mirage of floating exchange rates},\n"
            "  author={Reinhart, Carmen M},\n"
            "  journal={American Economic Review},\n"
            "  volume={90},\n  number={2},\n  pages={65--70},\n  year={2000},\n"
            "  publisher={American Economic Association}\n}\n\n")
    tovar = "@techreport{tovar2018reserve,\n" + s[mid:tail].rstrip() + "\n}\n"
    s = s[:start] + rein + tovar + s[end:]
    print("err.bib: fixed reinhart2000mirage, recovered tovar2018reserve")
# a bare "and" splits a name, so protect the institution name with double braces
name = 'National Institute of Public Finance and Policy'
s = s.replace('{{' + name + '}}', '{' + name + '}').replace('{' + name + '}', '{{' + name + '}}')
open('err.bib', 'w').write(s)

# ---- 2. wire the bibliography into the site ---------------------------------
if 'bibliography:' not in open('_quarto.yml').read():
    sub('_quarto.yml', "metadata-files:\n  - _sidebar.yml\n",
        "metadata-files:\n  - _sidebar.yml\n\nbibliography: err.bib\n")
    sub('_quarto.yml', "    css: styles.css",
        "    css: styles.css\n    citations-hover: true\n    link-citations: true")
if '@zeileis2010testing' not in open('templates/country.qmd.tpl').read():
    sub('templates/country.qmd.tpl',
        "estimated with structural-break regressions (`fxregime`)",
        "estimated with the structural-break regression framework of @zeileis2010testing (R package `fxregime`)")
if 'about.qmd' not in open('index.qmd').read():
    sub('index.qmd', "Use the sidebar to open a country page.",
        "Use the sidebar to open a country page, or see [About](about.qmd) for the methodology and references.")
print("site files updated")
