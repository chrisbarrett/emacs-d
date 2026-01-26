# org-capture

Org capture templates and litnote workflow with AI-assisted metadata extraction.

## Files

| File                                         | Purpose                                  |
| :------------------------------------------- | :--------------------------------------- |
| `config/mod-org-capture.el`                  | Capture template definitions             |
| `lisp/+capture.el`                           | Litnote workflow and metadata extraction |
| `capture-templates/litnote.org`              | Org-roam litnote template                |
| `capture-templates/postmortem.org`           | Postmortem review template               |
| `capture-templates/journal.org`              | Daily journal reflection template        |
| `capture-templates/language-learning-review.org` | Weekly language study review         |

## External Packages

| Package      | Purpose                       |
| :----------- | :---------------------------- |
| org-capture  | Built-in capture system       |
| org-cliplink | Clipboard URL insertion       |
| s            | String manipulation           |
| dash         | List/sequence operations      |

## Behaviors

### Capture Templates

All datetree-based templates use `(month day)` tree-type for hierarchical date organization.

| Key | Name              | Type      | Target                  | Tags                      |
| :-- | :---------------- | :-------- | :---------------------- | :------------------------ |
| t   | Todo              | entry     | notes datetree          | none                      |
| n   | Note              | entry     | notes datetree          | none                      |
| N   | Note (set time)   | entry     | notes datetree          | none                      |
| w   | work prefix       | —         | —                       | —                         |
| wt  | Work Todo         | entry     | notes datetree          | `timekeep-work-tag`, work |
| wn  | Work Note         | entry     | notes datetree          | `timekeep-work-tag`, work |
| wN  | Work Note (time)  | entry     | notes datetree          | `timekeep-work-tag`, work |
| l   | Link              | entry     | notes datetree          | none                      |
| L   | Litnote           | plain     | org-roam litnotes dir   | none                      |
| p   | Postmortem        | entry     | notes datetree          | pm                        |
| j   | Journal           | entry     | notes datetree          | journal                   |
| r   | Language Review   | entry     | notes datetree          | learning                  |

### Litnote Workflow

```text
GIVEN a URL in the kill ring or an eww buffer visible
WHEN  org-capture with "L" is invoked
THEN  1. URL content is fetched and rendered
      2. Claude CLI extracts metadata (:title, :author, :year, :url)
      3. User is prompted for missing title/author
      4. New file created in org-roam-directory/litnotes/
      5. File named as snake_case of title
      6. Capture jumps to new file immediately
```

### AI Metadata Extraction

The `+capture--metadata-for-web-document` function:

1. Detects YouTube URLs and uses title-only prompt
2. Generic pages use title + first 1500 chars of rendered content
3. Calls Claude CLI with haiku model (`--model haiku --max-turns 1`)
4. Parses plist from Claude response
5. Returns merged plist with `:url` prepended

### Template Files

**litnote.org:**
- Creates org-roam node with ID property
- Title format: `Author - Title`
- META drawer with URL, AUTHORS, YEAR
- Uses `+capture-context` plist for values

**postmortem.org:**
- Sections: Description, Background & Context, Timeline, Effects, Mitigations
- Tagged with `:pm:`
- Includes comment prompts for each section

**journal.org:**
- Reflective prompts for daily journaling
- Sections for energy, authenticity, challenges, self-description
- Tagged with `:journal:`

**language-learning-review.org:**
- Weekly review with clocktable for French study time
- Prompts for consistency, progress, improvements
- Tagged with `:learning:`

### Global Settings

| Setting                | Value | Effect                              |
| :--------------------- | :---- | :---------------------------------- |
| `:kill-buffer`         | t     | Kill capture buffer after finalize  |

## API

### Variables

| Variable                      | Default | Purpose                              |
| :---------------------------- | :------ | :----------------------------------- |
| `+capture-context`            | nil     | Plist for current capture metadata   |
| `+capture-excerpt-length-chars` | 1500  | Max chars sent to Claude for parsing |

### Functions

| Function                               | Purpose                                          |
| :------------------------------------- | :----------------------------------------------- |
| `+capture-litnote-function`            | Target function for litnote capture              |
| `+capture-read-url`                    | Prompt for URL with kill-ring default            |
| `+litnote-meta-try-from-eww`           | Extract metadata from visible eww buffer         |
| `+litnote-meta-from-url`               | Fetch URL and extract metadata                   |
| `+capture--metadata-for-web-document`  | Call Claude CLI for metadata extraction          |
| `+capture--prompt-for-youtube-video`   | Generate prompt for YouTube metadata             |
| `+capture--prompt-for-generic-web-page`| Generate prompt for generic page metadata        |

## Testable Properties

1. Capture template "t" creates TODO entry in datetree
2. Work templates include both `timekeep-work-tag` and `work` tags
3. Link template uses `org-cliplink-capture` for URL insertion
4. Litnote creates file in `org-roam-directory/litnotes/`
5. Litnote filename is snake_case of title
6. `+capture-read-url` defaults to first URL-like string in kill ring
7. YouTube URLs use title-only prompt (no content excerpt)
8. Generic URLs include up to 1500 chars of rendered content
9. Capture buffers are killed after finalize (`:kill-buffer t`)
