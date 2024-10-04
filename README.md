# leyline.el

The leyline package provides utilities to apply complex LLM-suggested code changes to Emacs buffers.
Unlike existing tools such as [avante.nvim](https://github.com/yetone/avante.nvim) and [elysium](https://github.com/lanceberge/elysium/) it does not rely on complex prompt engineering to obtain good results, but instead makes use of a novel diff-based method.
In our experience, this results in a more predictable and semantically correct changes than competing methods.

We also use a specialized form of the [Needleman–Wunsch algorithm](https://en.wikipedia.org/wiki/Needleman–Wunsch_algorithm) to apply the smallest possible set of changes which ensures the point and mark remain stable, even if they are located within the modified region.
This approach was inspired by Radon Rosborough's [apheleia](https://github.com/radian-software/apheleia) package.

## Installation

Leyline is not currently available on any package archive.
We suggest installing it using the builtin `package-vc-install` utility, but any third-party package manager that allows installs from source should work fine.

`M-x package-vc-install RET https://github.com/leotaku/leyline.git RET`

## Setup

Leyline uses [llm](https://github.com/ahyatt/llm) providers to configure which LLMs should be queried for changes.
While any LLM will work is supported in theory, increased model quality and size will of course have an impact on the quality of responses.
The creator of this package currently uses Anthropic's leading `claude-3-5-sonnet-20240620` model.

The following minimal configuration should work if you store your Anthropic API keys using `auth-source`:

``` emacs-lisp
(require 'llm-claude)
(require 'leyline)

(setq llm-warn-on-nonfree . nil)
(setq leyline-provider (make-llm-claude :key (auth-source-pick-first-password :host "api.anthropic.com")))
```

## Usage

Leyline currently provides a single entry point `M-x leyline-buffer RET`, which will do the following:

1. Prompt the user for a TASK.
2. Send this TASK, together with the contents of the current buffer, to the LLM.
3. Apply the returned changes to the current buffer.

Additional commands that can use e.g. the full project context or apply changes to multiple files are planned.

## License

This Emacs package is distributed under the terms of the [GPL-3.0-or-later](LICENSE) license, meaning the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

Copyright notices are included with all individual files of source code.
