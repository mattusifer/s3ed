> ⚠️ Archived — no longer maintained
>
> This project is no longer actively developed. Issues and PRs are not being monitored.

# s3ed ![travis-ci](https://travis-ci.org/mattusifer/s3ed.svg?branch=master)

S3ed provides an interface to [Amazon S3](https://aws.amazon.com/s3/) from within Emacs.

## Installation

S3ed is available on [MELPA](https://melpa.org/)

```
M-x package-install s3ed
```

## Usage

Initialize s3ed
```elisp
(require 's3ed)
(s3ed-mode)
```

There are two main entry-points to s3ed. The first is the `s3ed-find-file` function - a replacement for `find-file`, but for s3. The second is the `s3ed-save-file` function - a replacement for `save-file` (or `write-file`), but for s3. I have these functions bound as follows:

```elisp
(global-set-key (kbd "C-c s f") 's3ed-find-file)
(global-set-key (kbd "C-c s s") 's3ed-save-file)
```

Note that you can leave `s3ed-mode` off by default - both of the above commands will provide an option to enable it for your current session.

### Opening files with `s3ed-find-file`

If you select a file with `s3ed-find-file`, the file will be downloaded from s3 to your local machine and opened. Saving that file will apply changes directly to s3, and reverting that file will pull data down from s3 before revert.

### Opening directories with `s3ed-find-file`
If you select a directory with `s3ed-find-file`, the directory will open in dired. Before it opens, s3ed will create empty directories and empty files to represent the directories and files at that location in s3, so that when the directory opens in dired, it will show all of the file and directory names at that location on s3. At this point, the following native dired operations on this local directory will be linked to the analagous files on s3. Some of these operations are asynchronous - progress can be tracked in the `*s3ed*` buffer that is opened in the background.

- Renaming files and directories (async)
- Copying files and directories
  - You can copy files within s3 or outside of s3. The former is asynchronous. The latter supports TRAMP, but it is a synchronous operation.
- Deleting files and directories, marking files and directories for deletion (async)
- Refreshing the directory
- Opening files and directories (including opening and editing compressed files, like in normal dired mode)

#### Shell Commands
Shell commands in s3ed dired will be applied by streaming (`aws s3 cp <file> -`) the file on s3 through the given command. Since not all commands support this functionality, only the following are supported: `head`, `cat`, `grep`, `tail`, `sed`, `awk`, `cut`, `wc`, `sort`, `tr`, `uniq`, `lbzcat`, `gzcat`.

### Saving files with `s3ed-save-file`

If you save a file with `s3ed-save-file`, the file is first saved in the s3ed temp dir and then that file is copied up to the requested s3 path.

### Setting the aws profile

The profile that will be used can be set using the interactive
`s3ed-set-profile` function. You can customize the default profile by
setting the `s3ed-profile-name` variable.


## Dependencies

- The `aws` command line utility should be on your `PATH`

## Notes

- S3ed will use /tmp/s3ed/ as scratch space
- S3ed tests will use /tmp/s3ed-aws-testing/ as scratch space

## AWS Authentication

S3ed uses the `aws` command line utility to access s3, which has several options for authentication. One easy method is to run `aws configure` to create a credentials file, which will get picked up by subsequent `aws` calls.
