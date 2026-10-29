# Notes for Claude

How this repo works and how Luke likes changes made. The README covers setup
and the day-to-day commands; this file is the working conventions.

## Layout
- **chezmoi** manages configs. `.chezmoiroot` is `home/`, so `home/dot_config/x`
  → `~/.config/x`. `home/.chezmoiignore` decides what each OS gets.
- **Symlink mode** (`mode = "symlink"` in `home/.chezmoi.toml.tmpl`): plain files
  are deployed as symlinks into this repo. Luke edits settings inside each app,
  and those edits land here; he saves them with plain `git commit` and `git push`.
  So prefer plain files over `*.tmpl` templates, which are rendered copies and
  break that. Use a template only when a file truly must differ per OS.
- **Ansible** (`main.yml`, `tasks/`) installs packages and OS settings on
  Linux/macOS, then runs chezmoi. Prefer the distro package (dnf, Homebrew) over
  downloading release binaries; check `dnf info` before assuming something isn't
  packaged.
- **Windows** bootstraps with `install.ps1` (winget for chezmoi, then
  `chezmoi apply`). Windows Terminal's `LocalState` is a directory symlink to
  `Windows/TerminalLocalState`.
- **Emacs:** `init.el` and `early-init.el` are vendored from minimal-emacs.d
  (version and commit in the README). Only change them deliberately; Luke's own
  settings go in `pre-*.el` and `post-*.el`. Same `~/.config/emacs` on every OS.
- **Prompt:** oh-my-posh with one shared config
  (`home/dot_config/oh-my-posh/config.omp.json`) in pwsh and bash.
- **Shells:** only bash (Linux/macOS) and pwsh (Windows). Fish and zsh were
  dropped on purpose; don't add them back. Machine-specific bash settings go in
  `~/.bashrc.d/`, not the repo.
- **SSH:** `home/private_dot_ssh/config` has general defaults only and includes
  `~/.ssh/config.d/*`. The repo is public: host names, addresses and anything
  host-specific go in `~/.ssh/config.d/hosts` on each machine, never the repo.
  Windows Terminal SSH profiles use those host names (`ssh nas1`), not IPs.
- **Fonts** are installed by hand (listed in the README), not by any tool.

## Machines, in priority order
1. Windows 11 PC: about 80% of use.
2. FedoraLinux-44 under WSL: about 20%; the Linux machine to verify on.
   Luke doesn't use Emacs there, so don't test or fix Emacs on WSL.
3. FedoraLinux-42 under WSL: old Ansible-era setup.
4. macOS: unused for 2.5 years. Keep its tasks, lowest priority.

## Git
- Commit straight to `main` and push. Use a branch and PR only for something
  major (on the scale of adopting chezmoi).
- The remote is called `Github`, not `origin`.
- Never force-push or rewrite pushed history; fix with a follow-up commit.
- `.gitattributes` normalises to LF (`*.ps1` is CRLF); expect line-ending warnings.

## Checking a change
There are no automated tests. Before pushing, verify the change by hand:
- **Ansible:** `ansible-lint` must pass (config in `.ansible-lint`; it skips
  `home/` and `Windows/`). Run the playbook on Fedora 44 twice: the second run
  must report `changed=0`.
- **Configs on Windows:** `chezmoi apply`, then `chezmoi status` should be
  empty and links should point into the repo. Load what changed (for example
  pwsh with the profile, or Emacs with `emacs --batch` loading `early-init.el`
  and `init.el`).
- **Shell configs:** start the shell interactively (`bash -ic '...'`, or `pwsh`
  with the profile) and check the setting took effect.
- To try unpushed commits on Fedora 44, fetch straight from the Windows repo:
  `git fetch /mnt/c/Users/Luke/Repos/dotfiles main && git merge --ff-only FETCH_HEAD`.
- Say plainly what was and wasn't checked.

## WSL gotchas
- `wsl.exe -- <cmd>` passes the command through the login shell, which expands
  `$` first. Write the commands to a script file
  and run `wsl.exe -d FedoraLinux-44 --cd ~ -- bash /mnt/c/.../script.sh`.
- The Fedora WSL image is minimal: tools like `awk` may be missing, so don't
  rely on them in scripts. Missing tools Luke wants go into `fedora_apps`.

## Safety
- Back up a live config before chezmoi replaces it.
- On Windows, delete via the Recycle Bin rather than permanently.
- Remove an old Windows junction with `[IO.Directory]::Delete(path, $false)`
  (removes only the link), never a recursive delete.
