# tramps3 ![travis-ci](https://travis-ci.org/mattusifer/tramps3.svg?branch=master)

Tramps3 provides an interface to [Amazon S3](https://aws.amazon.com/s3/) from within Emacs. Tramps3 is inspired by [TRAMP](https://www.emacswiki.org/emacs/TrampMode), and strives to provide near-seamless access to S3 from standard Emacs functions.

## Usage

Initialize tramps3
```elisp
(require 'tramps3)
(tramps3-mode)
```

There are two main entry-points to tramps3. The first is the `tramps3-find-file` function - a replacement for `find-file`, but for s3. The second is the `tramps3-save-file` function - a replacement for `save-file` (or `write-file`), but for s3. I have these functions bound as follows:

```elisp
(global-set-key (kbd "C-c s f") 'tramps3-find-file)
(global-set-key (kbd "C-c s s") 'tramps3-save-file)
```

Note that you can leave `tramps3-mode` off by default - both of the above commands will provide an option to enable it for your current session.

### Opening files with `tramps3-find-file`

If you select a file with `tramps3-find-file`, the file will be downloaded from s3 to your local machine and opened. Saving that file will apply changes directly to s3, and reverting that file will pull data down from s3 before revert.

### Opening directories with `tramps3-find-file`
If you select a directory with `tramps3-find-file`, the directory will open in dired. Before it opens, tramps3 will create empty directories and empty files to represent the directories and files at that location in s3, so that when the directory opens in dired, it will show all of the file and directory names at that location on s3. At this point, the following native dired operations on this local directory will be linked to the analagous files on s3:

- Renaming or copying files and directories
- Deleting files and directories, marking files and directories for deletion
- Refreshing the directory
- Opening files and directories (including opening and editing compressed files, like in normal dired mode)
- Common shell commands

**Keep in mind** - Other normal dired commands will work, and the resulting changes to the local dir will not be propogated to s3.

### Saving files with `tramps3-save-file`

If you save a file with `tramps3-save-file`, the file is first saved in the tramps3 temp dir (`/tmp/tramps3` by default) and then that file is copied up to the requested s3 path.

## Dependencies

- The `aws` command line utility should be on your `PATH`

## TrampS3 Configuration

Tramps3 will use `/tmp/tramps3` to pull down data from s3 by default. This location can be modified by changing `TRAMPS3_TMP_S3_DIR`.

## AWS Authentication

Tramps3 uses the `aws` command line utility to access s3, which has several options for authentication. One easy method is to run `aws configure` to create a credentials file, which will get picked up by subsequent `aws` calls.
