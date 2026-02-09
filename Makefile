.PHONY: blog book wiki build-wiki clean-wiki help up history sitemap

# OS detection for sed compatibility
UNAME := $(shell uname)
ifeq ($(UNAME), Darwin)
    SED_CMD := sed -i ''
else
    SED_CMD := sed -i
endif

# Default target
help:
	@echo "Available commands:"
	@echo "  make blog        - Create a new blog post (interactive)"
	@echo "  make book        - Create a new book review (interactive)"
	@echo "  make wiki        - Create a new wiki note (interactive)"
	@echo "  make build-wiki  - Build all wiki HTML from markdown"
	@echo "  make clean-wiki  - Remove generated wiki HTML files"
	@echo "  make sitemap     - Generate sitemap.xml"
	@echo "  make up          - Pull latest changes and show status"
	@echo "  make history     - Show git log"

# Create a new blog post
blog:
	@echo "Creating new blog post..."
	@current_date=$$(date +%Y-%m-%d); \
	printf "Enter date (default: $$current_date): "; \
	read date; \
	date=$${date:-$$current_date}; \
	printf "Enter blog post title (required): "; \
	read title; \
	while [ -z "$$title" ]; do \
		echo "Blog post title is required."; \
		printf "Enter blog post title (required): "; \
		read title; \
	done; \
	filename=$$(echo $$title | tr '[:upper:]' '[:lower:]' | sed 's/[^a-z0-9]/_/g' | sed 's/__*/_/g').html; \
	cp blog/template/blog-post.html blog/posts/$$filename; \
	$(SED_CMD) 's/\[BLOG POST TITLE\]/'"$$title"'/g' blog/posts/$$filename; \
	$(SED_CMD) 's/<h2>Blog Post Title<\/h2>/<h2>'"$$title"'<\/h2>/g' blog/posts/$$filename; \
	$(SED_CMD) 's/<dd>Date Written<\/dd>/<dd>'"$$date"'<\/dd>/g' blog/posts/$$filename; \
	blog_entry="        <dl>\n          <dt>$$date</dt>\n          <dd>\n            <a href=\"posts/$$filename\">$$title</a>\n          </dd>\n        </dl>"; \
	$(SED_CMD) "/<section class=\"booksAndBlog\">/a\\$$blog_entry" blog/index.html; \
	echo "Created blog post: blog/posts/$$filename"; \
	echo "Updated blog/index.html with new blog entry"

# Create a new book review
book:
	@echo "Creating new book review..."
	@current_date=$$(date +%Y-%m-%d); \
	printf "Enter date (default: $$current_date): "; \
	read date; \
	date=$${date:-$$current_date}; \
	printf "Enter book name (required): "; \
	read book_name; \
	while [ -z "$$book_name" ]; do \
		echo "Book name is required."; \
		printf "Enter book name (required): "; \
		read book_name; \
	done; \
	printf "Enter author name (required): "; \
	read author_name; \
	while [ -z "$$author_name" ]; do \
		echo "Author name is required."; \
		printf "Enter author name (required): "; \
		read author_name; \
	done; \
	printf "Enter ISBN number (required): "; \
	read isbn; \
	while [ -z "$$isbn" ]; do \
		echo "ISBN number is required."; \
		printf "Enter ISBN number (required): "; \
		read isbn; \
	done; \
	printf "Enter category (F for Fiction, N for Non-Fiction): "; \
	read category; \
	while [ "$$category" != "F" ] && [ "$$category" != "N" ]; do \
		echo "Category must be F or N."; \
		printf "Enter category (F for Fiction, N for Non-Fiction): "; \
		read category; \
	done; \
	if [ "$$category" = "F" ]; then \
		category_text="Fiction"; \
	else \
		category_text="Non-Fiction"; \
	fi; \
	printf "Enter rating (0-5, can be decimal): "; \
	read rating; \
	while [ -z "$$rating" ] || ! echo "$$rating" | grep -q "^[0-5]\(\.[0-9]\+\)\?$$"; do \
		echo "Rating must be a number between 0 and 5 (can be decimal)."; \
		printf "Enter rating (0-5, can be decimal): "; \
		read rating; \
	done; \
	filename=$$(echo $$book_name | tr '[:upper:]' '[:lower:]' | sed 's/[^a-z0-9]/_/g' | sed 's/__*/_/g').html; \
	cp blog/template/book-post.html blog/posts/$$filename; \
	$(SED_CMD) 's/\[BOOK TITLE\]/'"$$book_name"'/g' blog/posts/$$filename; \
	$(SED_CMD) 's/\[AUTHOR\]/'"$$author_name"'/g' blog/posts/$$filename; \
	$(SED_CMD) 's/<h2>Book Title<\/h2>/<h2>'"$$book_name"'<\/h2>/g' blog/posts/$$filename; \
	$(SED_CMD) 's/<dd>Author Name<\/dd>/<dd>'"$$author_name"'<\/dd>/g' blog/posts/$$filename; \
	$(SED_CMD) 's/<dd>ISBN Number<\/dd>/<dd>'"$$isbn"'<\/dd>/g' blog/posts/$$filename; \
	$(SED_CMD) 's/<dd>Fiction\/Non-Fiction<\/dd>/<dd>'"$$category_text"'<\/dd>/g' blog/posts/$$filename; \
	$(SED_CMD) 's/<dd>\*\/5<\/dd>/<dd>'"$$rating"'\/5<\/dd>/g' blog/posts/$$filename; \
	$(SED_CMD) 's/<dd>Day Month Year<\/dd>/<dd>'"$$date"'<\/dd>/g' blog/posts/$$filename; \
	book_entry="        <dl>\n          <dt>$$date</dt>\n          <dd>\n            <a href=\"posts/$$filename\">$$book_name</a>\n            <br>\n            <span class=\"author\">$$author_name</span>\n            <br>\n            <span class=\"isbn\">ISBN: $$isbn</span>\n            <br>\n            <span class=\"rating\">$$rating/5</span>, $$category_text\n          </dd>\n        </dl>"; \
	$(SED_CMD) "/<section class=\"booksAndBlog\">/a\\$$book_entry" blog/index.html; \
	echo "Created book review: blog/posts/$$filename"; \
	echo "Updated blog/index.html with new book entry"

# Create a new wiki note
wiki:
	@echo "Creating new wiki note..."
	@read -p "Subject: " title; \
	read -p "Language extension: " language; \
	echo "# \`$$title\`" > personal-wiki/notes/"$$title".md; \
	echo "" >> personal-wiki/notes/"$$title".md; \
	echo "## Comments" >> personal-wiki/notes/"$$title".md; \
	echo "" >> personal-wiki/notes/"$$title".md; \
	echo "\`\`\`$$language" >> personal-wiki/notes/"$$title".md; \
	echo "" >> personal-wiki/notes/"$$title".md; \
	echo "\`\`\`" >> personal-wiki/notes/"$$title".md; \
	echo "" >> personal-wiki/notes/"$$title".md; \
	echo "## Printing" >> personal-wiki/notes/"$$title".md; \
	echo "" >> personal-wiki/notes/"$$title".md; \
	echo "\`\`\`$$language" >> personal-wiki/notes/"$$title".md; \
	echo "" >> personal-wiki/notes/"$$title".md; \
	echo "\`\`\`" >> personal-wiki/notes/"$$title".md; \
	echo "" >> personal-wiki/notes/"$$title".md; \
	echo "## Quickstart" >> personal-wiki/notes/"$$title".md; \
	echo "" >> personal-wiki/notes/"$$title".md; \
	echo "\`\`\`$$language" >> personal-wiki/notes/"$$title".md; \
	echo "" >> personal-wiki/notes/"$$title".md; \
	echo "\`\`\`" >> personal-wiki/notes/"$$title".md; \
	echo "" >> personal-wiki/notes/"$$title".md; \
	echo "## Types" >> personal-wiki/notes/"$$title".md; \
	echo "" >> personal-wiki/notes/"$$title".md; \
	echo "\`\`\`$$language" >> personal-wiki/notes/"$$title".md; \
	echo "" >> personal-wiki/notes/"$$title".md; \
	echo "\`\`\`" >> personal-wiki/notes/"$$title".md; \
	echo "" >> personal-wiki/notes/"$$title".md; \
	echo "## Operators" >> personal-wiki/notes/"$$title".md; \
	echo "" >> personal-wiki/notes/"$$title".md; \
	echo "\`\`\`$$language" >> personal-wiki/notes/"$$title".md; \
	echo "" >> personal-wiki/notes/"$$title".md; \
	echo "\`\`\`" >> personal-wiki/notes/"$$title".md; \
	echo "" >> personal-wiki/notes/"$$title".md; \
	echo "## Control structures" >> personal-wiki/notes/"$$title".md; \
	echo "" >> personal-wiki/notes/"$$title".md; \
	echo "\`\`\`$$language" >> personal-wiki/notes/"$$title".md; \
	echo "" >> personal-wiki/notes/"$$title".md; \
	echo "\`\`\`" >> personal-wiki/notes/"$$title".md; \
	echo "" >> personal-wiki/notes/"$$title".md; \
	echo "## Data structures" >> personal-wiki/notes/"$$title".md; \
	echo "" >> personal-wiki/notes/"$$title".md; \
	echo "\`\`\`$$language" >> personal-wiki/notes/"$$title".md; \
	echo "" >> personal-wiki/notes/"$$title".md; \
	echo "\`\`\`" >> personal-wiki/notes/"$$title".md; \
	echo "" >> personal-wiki/notes/"$$title".md; \
	echo "## Functions" >> personal-wiki/notes/"$$title".md; \
	echo "" >> personal-wiki/notes/"$$title".md; \
	echo "\`\`\`$$language" >> personal-wiki/notes/"$$title".md; \
	echo "" >> personal-wiki/notes/"$$title".md; \
	echo "\`\`\`" >> personal-wiki/notes/"$$title".md; \
	echo "" >> personal-wiki/notes/"$$title".md; \
	echo "## More on" >> personal-wiki/notes/"$$title".md; \
	echo "" >> personal-wiki/notes/"$$title".md; \
	echo "*" >> personal-wiki/notes/"$$title".md; \
	echo "" >> personal-wiki/notes/"$$title".md; \
	echo "Created wiki note: personal-wiki/notes/$$title.md"

# Build wiki HTML from markdown
build-wiki:
	@echo "Building wiki pages..."
	@cd personal-wiki && python3 build.py
	@echo "Wiki build complete!"

# Clean generated wiki HTML files
clean-wiki:
	@echo "Cleaning generated wiki files..."
	@rm -f personal-wiki/*.html
	@echo "Clean complete!"

# Generate sitemap.xml
sitemap:
	@echo "Generating sitemap.xml..."
	@python3 scripts/generate_sitemap.py
	@echo "Sitemap generation complete!"

# Git helpers (retained from original)
up:
	@git pull
	@git status

history:
	@git log
