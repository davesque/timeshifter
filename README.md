# timeshifter

Shifts time entries in subrip files by a specified time interval.  For information about the subrip file format:

<http://en.wikipedia.org/wiki/SubRip>

## Usage

    Usage:
    timeshifter interval filename

`interval` &mdash; the time interval to apply to all entry timestamps in seconds.  Example:

```bash
timeshifter 2.5 never_cry_wolf.srt
```

`filename` &mdash; the name of the subrip file to modify.
