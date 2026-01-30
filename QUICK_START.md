# Quick Start Guide

Get your dashboard live in 5 minutes!

## Prerequisites Check

Do you have Quarto installed?

```bash
quarto --version
```

If not: https://quarto.org/docs/get-started/

## Step 1: Render Locally (2 minutes)

```bash
cd quarto_dashboard
quarto render index.qmd
```

Preview in browser:
```bash
open index.html  # Mac
```

## Step 2: Deploy to GitHub Pages (3 minutes)

```bash
# Initialize git (if not already done)
git init
git add index.qmd index.html public_data/
git commit -m "Add Quarto dashboard"

# Push to GitHub
git remote add origin https://github.com/alexandergofton/YOUR-REPO-NAME.git
git branch -M main
git push -u origin main
```

Then on GitHub.com:
1. Go to Settings → Pages
2. Select: Source = "Deploy from branch"
3. Branch = "main", Folder = "/ (root)"
4. Click Save

Wait 2-3 minutes, then visit:
```
https://alexandergofton.github.io/YOUR-REPO-NAME/
```

Done! 🎉

## Alternative: One-Command Deploy

Using Quarto Pub:

```bash
quarto publish quarto-pub index.qmd
```

Follow prompts. Your dashboard goes live instantly!

## What You Get

✅ Lightning-fast loading (1-2 seconds vs 15-30 seconds)
✅ Unlimited users (no concurrent user limits)
✅ Free forever (no hour limits)
✅ 100% reliable (no sleeping/waking)
✅ Works offline (after first load)

## Customization

Want to change the theme? Edit `index.qmd`:

```yaml
format:
  html:
    theme: flatly  # Try: cosmo, flatly, minty, pulse
```

Then re-render:
```bash
quarto render index.qmd
```

## Need Help?

See [README.md](README.md) for detailed docs.

**Questions?** alexander.gofton@csiro.au
