.PHONY: new

all: new

new:
	@read -p "subject: " title; \
	read -p "language extension: " language; \
	echo "# \`$$title\`" > "$$title".md; \
	echo "" >> "$$title".md; \
	echo "## Comments" >> "$$title".md; \
	echo "" >> "$$title".md; \
	echo "\`\`\`$$language" >> "$$title".md; \
	echo "" >> "$$title".md; \
	echo "\`\`\`" >> "$$title".md; \
	echo "" >> "$$title".md; \
	echo "## Printing" >> "$$title".md; \
	echo "" >> "$$title".md; \
	echo "\`\`\`$$language" >> "$$title".md; \
	echo "" >> "$$title".md; \
	echo "\`\`\`" >> "$$title".md; \
	echo "" >> "$$title".md; \
	echo "## Quickstart" >> "$$title".md; \
	echo "" >> "$$title".md; \
	echo "\`\`\`$$language" >> "$$title".md; \
	echo "" >> "$$title".md; \
	echo "\`\`\`" >> "$$title".md; \
	echo "" >> "$$title".md; \
	echo "## Types" >> "$$title".md; \
	echo "" >> "$$title".md; \
	echo "\`\`\`$$language" >> "$$title".md; \
	echo "" >> "$$title".md; \
	echo "\`\`\`" >> "$$title".md; \
	echo "" >> "$$title".md; \
	echo "## Operators" >> "$$title".md; \
	echo "" >> "$$title".md; \
	echo "\`\`\`$$language" >> "$$title".md; \
	echo "" >> "$$title".md; \
	echo "\`\`\`" >> "$$title".md; \
	echo "" >> "$$title".md; \
	echo "## Control structures" >> "$$title".md; \
	echo "" >> "$$title".md; \
	echo "\`\`\`$$language" >> "$$title".md; \
	echo "" >> "$$title".md; \
	echo "\`\`\`" >> "$$title".md; \
	echo "" >> "$$title".md; \
	echo "## Data structures" >> "$$title".md; \
	echo "" >> "$$title".md; \
	echo "\`\`\`$$language" >> "$$title".md; \
	echo "" >> "$$title".md; \
	echo "\`\`\`" >> "$$title".md; \
	echo "" >> "$$title".md; \
	echo "## Functions" >> "$$title".md; \
	echo "" >> "$$title".md; \
	echo "\`\`\`$$language" >> "$$title".md; \
	echo "" >> "$$title".md; \
	echo "\`\`\`" >> "$$title".md; \
	echo "" >> "$$title".md; \
	echo "## More on" >> "$$title".md; \
	echo "" >> "$$title".md; \
	echo "*" >> "$$title".md; \
	echo "" >> "$$title".md; \
	echo "Markdown file created: $$title.md"

up:
	git pull