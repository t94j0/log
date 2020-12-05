# log

Log is an extremely simple tool for writing up things that happen during an operation because taking the time to write up what you're doing in the middle of hacking sucks. Hack when you're in the zone. Write the report later.

Use your command line to easily log notes for yourself and take screenshots.

## Usage

### 1. Make a log

`log "Rooted HalCorp AD with MS-17-10"`

### 2. View log

`log cat`

**Output**
```
2025-01-31 20:20:20.69696969 UTC => Rooted HalCorp AD with MS-17-10
```

### 3. Take a screenshot

`log screenshot`

You can also use

`log sc`

**Output with `log cat`**
```
2025-01-31 20:20:20.69696969 UTC => Screenshot: otkyxhiveh.png
```

Inside the `/var/seclog/<workspace>` directory, you'll find the screenshot

### 4. Change workspaces

```
log switch 192.168.0.2
log testing
```

**/var/seclog/192.168.0.2/record.md**
```
2025-01-31 20:20:20.69696969 UTC => testing
```

### 5. View workspaces

`log ls`

***Output***
```
default    192.168.0.2    192.168.0.3
```

Then, you can switch back into any of these workspaces to keep appending to the log


## Installation

Honestly, I'm too lazy to package this as a .deb, but I might get around to it later. Right now, install it from source.

```bash
sudo apt install scrot
curl -sSL https://get.haskellstack.org/ | sh # curl to bash is awesome
git clone https://github.com/t94j0/log
cd log
stack setup
stack install

sudo log setup # Creates /var/seclog/workspace and parent directory
```

Not too bad, but I didn't test the install code, so something may break. Please don't make an issue for Stack issues. I'll just close it and not respond. If it's an issue with the code, I'll help.
