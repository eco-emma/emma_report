# GitHub Copilot Customisation

This directory contains always-on instructions and optional agents for GitHub
Copilot Chat / coding agents working in `emma_report`.

---

## Always-on instructions

`instructions/r-style.instructions.md` — applied to R / Quarto sources.
Requires the [tidyverse style guide](https://style.tidyverse.org) and
[`styler`](https://styler.r-lib.org) formatting. Also see repo-root `AGENTS.md`,
`CLAUDE.md`, `.cursor/rules/r-tidyverse-style.mdc`, and `.clinerules/r-style.md`.

---

## Custom agents (`agents/`)

| Agent | Philosophy |
|---|---|
| `@coder_alpha` | Correctness-first: readable, conventional, tidyverse-styled R |
