#!/usr/bin/env python3
"""
Build script for personal-wiki
Converts markdown notes to HTML pages
"""

import os
import re
import sys
from datetime import datetime
from pathlib import Path

try:
    import markdown
    from markdown.extensions import fenced_code, tables, nl2br
except ImportError:
    print("Error: markdown library not found. Install with: pip install markdown")
    sys.exit(1)


def extract_title(md_content):
    """Extract title from markdown file (first h1 heading)"""
    match = re.search(r'^#\s+`?([^`\n]+)`?', md_content, re.MULTILINE)
    if match:
        return match.group(1).strip()
    return "Untitled"


def convert_markdown_to_html(md_file, template_path, output_dir):
    """Convert a single markdown file to HTML"""
    # Read markdown file
    with open(md_file, 'r', encoding='utf-8') as f:
        md_content = f.read()

    # Extract title
    title = extract_title(md_content)

    # Convert markdown to HTML
    md = markdown.Markdown(extensions=[
        'fenced_code',
        'tables',
        'nl2br',
        'codehilite',
    ])
    html_content = md.convert(md_content)

    # Read template
    with open(template_path, 'r', encoding='utf-8') as f:
        template = f.read()

    # Replace placeholders in template (except file size and LOC which we'll calculate after)
    html_output = template.replace('[NOTE_TITLE]', title)
    html_output = html_output.replace('[CONTENT]', html_content)

    # Generate output filename
    output_filename = Path(md_file).stem.lower() + '.html'
    output_path = os.path.join(output_dir, output_filename)

    # Write HTML file temporarily to calculate its size and LOC
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(html_output)

    # Calculate HTML file size and lines of code
    html_size_bytes = os.path.getsize(output_path)
    html_size_kb = html_size_bytes / 1024
    if html_size_kb < 1:
        html_size = f"{html_size_bytes}B"
    else:
        html_size = f"{html_size_kb:.1f}KB"

    with open(output_path, 'r', encoding='utf-8') as f:
        html_lines = len(f.readlines())

    # Replace file size and LOC placeholders
    html_output = html_output.replace('[FILE_SIZE]', html_size)
    html_output = html_output.replace('[LOC]', str(html_lines))

    # Write final HTML file with all placeholders replaced
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(html_output)

    return {
        'title': title,
        'filename': output_filename,
        'size': html_size
    }


def generate_index(notes_info, output_path):
    """Generate index.html with list of all notes"""
    # Sort notes by title
    notes_info.sort(key=lambda x: x['title'])

    # Build HTML
    html = """<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="description" content="Personal Wiki - Gabriel Ong">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <link preload="stylesheet" href="style.css" as="style">
  <link rel="stylesheet" href="style.css">
  <link rel="preload" href="https://res.hajimehoshi.com/fonts/SuisseIntl-Regular-WebXL.woff2" as="font" crossorigin="anonymous">
  <style>.thin-space:after{content:"\\2006"}</style>
  <script src="script.js" defer></script>
  <title>GABRIEL ONG</title>
  <link rel="shortcut icon" href="asset/blob.ico" type="image/x-icon">
</head>
<body>
  <div id="click-container"></div>
  <input type="button" id="dark-mode">
  <label for="dark-mode" id="">
    <img id="infinityButton" src="asset/roller.png" height="24" viewbox="0 -960 960 960" width="24"/>
  </label>

  <main>
    <article class="overallArticleTags">

      <section class="introductionPortion">
        <h2>Gabriel's Personal Wiki</h2>
        <p><i>My notes from the things I learn. Available as a <a href="https://github.com/gongahkia/personal-wiki/releases"><b>ZIP file</b></a>.</i></p>
      </section>

      <section class="booksAndBlog">
"""

    for note in notes_info:
        html += f"""        <dl>
          <dt>{note['size']}</dt>
          <dd>
            <a href="pages/{note['filename']}">{note['title']}</a>
          </dd>
        </dl>
"""

    html += """      </section>

    </article>

    <footer>
      <p>© 2023-<span id="current-year"></span> Gabriel Ong. All rights reserved.</p>
    </footer>
  </main>

  <div class="wrapper"></div>
</body>
</html>
"""

    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(html)


def main():
    """Main build function"""
    # Paths
    base_dir = Path(__file__).parent
    notes_dir = base_dir / 'notes'
    template_path = base_dir / 'template' / 'note.html'
    pages_dir = base_dir / 'pages'

    # Create pages directory if it doesn't exist
    pages_dir.mkdir(exist_ok=True)

    # Check if template exists
    if not template_path.exists():
        print(f"Error: Template not found at {template_path}")
        sys.exit(1)

    # Check if notes directory exists
    if not notes_dir.exists():
        print(f"Error: Notes directory not found at {notes_dir}")
        sys.exit(1)

    # Get all markdown files
    md_files = list(notes_dir.glob('*.md'))

    if not md_files:
        print(f"No markdown files found in {notes_dir}")
        sys.exit(0)

    print(f"Found {len(md_files)} markdown files")

    # Convert each markdown file
    notes_info = []
    for md_file in md_files:
        print(f"Converting {md_file.name}...")
        note_info = convert_markdown_to_html(md_file, template_path, pages_dir)
        notes_info.append(note_info)
        print(f"  → Created {note_info['filename']}")

    # Generate index.html
    print("\nGenerating index.html...")
    index_path = base_dir / 'index.html'
    generate_index(notes_info, index_path)
    print(f"  → Created index.html")

    print(f"\n✓ Build complete! Generated {len(notes_info)} pages")


if __name__ == '__main__':
    main()
