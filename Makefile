.PHONY: new build clean up history

all: build

# Build all HTML pages from markdown files
build:
	@echo "Building wiki pages..."
	@python3 build.py

# Create a new markdown note
new:
	@read -p "subject: " title; \
	read -p "language extension: " language; \
	echo "# \`$$title\`" > notes/"$$title".md; \
	echo "" >> notes/"$$title".md; \
	echo "## Comments" >> notes/"$$title".md; \
	echo "" >> notes/"$$title".md; \
	echo "\`\`\`$$language" >> notes/"$$title".md; \
	echo "" >> notes/"$$title".md; \
	echo "\`\`\`" >> notes/"$$title".md; \
	echo "" >> notes/"$$title".md; \
	echo "## Printing" >> notes/"$$title".md; \
	echo "" >> notes/"$$title".md; \
	echo "\`\`\`$$language" >> notes/"$$title".md; \
	echo "" >> notes/"$$title".md; \
	echo "\`\`\`" >> notes/"$$title".md; \
	echo "" >> notes/"$$title".md; \
	echo "## Quickstart" >> notes/"$$title".md; \
	echo "" >> notes/"$$title".md; \
	echo "\`\`\`$$language" >> notes/"$$title".md; \
	echo "" >> notes/"$$title".md; \
	echo "\`\`\`" >> notes/"$$title".md; \
	echo "" >> notes/"$$title".md; \
	echo "## Types" >> notes/"$$title".md; \
	echo "" >> notes/"$$title".md; \
	echo "\`\`\`$$language" >> notes/"$$title".md; \
	echo "" >> notes/"$$title".md; \
	echo "\`\`\`" >> notes/"$$title".md; \
	echo "" >> notes/"$$title".md; \
	echo "## Operators" >> notes/"$$title".md; \
	echo "" >> notes/"$$title".md; \
	echo "\`\`\`$$language" >> notes/"$$title".md; \
	echo "" >> notes/"$$title".md; \
	echo "\`\`\`" >> notes/"$$title".md; \
	echo "" >> notes/"$$title".md; \
	echo "## Control structures" >> notes/"$$title".md; \
	echo "" >> notes/"$$title".md; \
	echo "\`\`\`$$language" >> notes/"$$title".md; \
	echo "" >> notes/"$$title".md; \
	echo "\`\`\`" >> notes/"$$title".md; \
	echo "" >> notes/"$$title".md; \
	echo "## Data structures" >> notes/"$$title".md; \
	echo "" >> notes/"$$title".md; \
	echo "\`\`\`$$language" >> notes/"$$title".md; \
	echo "" >> notes/"$$title".md; \
	echo "\`\`\`" >> notes/"$$title".md; \
	echo "" >> notes/"$$title".md; \
	echo "## Functions" >> notes/"$$title".md; \
	echo "" >> notes/"$$title".md; \
	echo "\`\`\`$$language" >> notes/"$$title".md; \
	echo "" >> notes/"$$title".md; \
	echo "\`\`\`" >> notes/"$$title".md; \
	echo "" >> notes/"$$title".md; \
	echo "## More on" >> notes/"$$title".md; \
	echo "" >> notes/"$$title".md; \
	echo "*" >> notes/"$$title".md; \
	echo "" >> notes/"$$title".md; \
	echo "Markdown file created: notes/$$title.md"

# Clean generated HTML files
clean:
	@echo "Cleaning generated files..."
	@rm -rf pages/
	@rm -f index.html
	@echo "Clean complete!"

up:
	@git pull
	@git status

history:
	@git log
