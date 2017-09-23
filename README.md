# tramps3

Tramps3 provides an interface to [Amazon S3](https://aws.amazon.com/s3/) from within Emacs. Tramps3 is inspired by [TRAMP](https://www.emacswiki.org/emacs/TrampMode), and strives to treat your data on S3 as if it were local, abstracting away as much of s3 as possible.

## Usage
The main entry-point to tramps3 is from the `tramps3-find-file` function, which is a replacement for `find-file`, but for s3.

Bind this function to a key that will allow quick access to s3

```elisp
(global-set-key (kbd "C-c s f") 'tramps3-find-file)
```

### Opening files with `tramps3-find-file`

If you select a file with `tramps3-find-file`, the file will be downloaded from s3 to your local machine and opened. Saving that file will apply changes directly to s3, and reverting that file will pull data down from s3 before revert. 

### Opening directories with `tramps3-find-file`
If you select a directory with `tramps3-find-file`, the directory will open in dired. Before it opens, tramps3 will create empty directories and empty files to represent the directories and files at that location in s3, so that when the directory opens in dired, it will show all of the file and directory names at that location on s3. At this point there are several shortcuts available for accessing and modifying that data on s3:

- **C-S-g**: Refresh the directory from s3
- **C-S-c**: Copy the thing-at-point, prompting for a destination first
- **C-S-r**: Move (rename) the thing-at-point, prompting for a destination first
- **C-S-d**: Remove the thing-at-point, with an option for recursive removal
- **C-\<return\>**: Open the thing at point. If it is a directory, a new dired buffer will open. If it is a file, the file will be downloaded and opened. Save and revert actions on the resulting file will apply or pull changes from s3.

Note that the above shortcuts mimick traditional dired shortcuts, with "C-S-" appended to the front.

**Keep in mind** - a tramps3 dired buffer is just a normal dired buffer pointed at the tramps3 tmp directory. Normal dired commands will work, and the changes will not be propogated to s3. The above commands must be used in order to propogate changes to s3.

## Dependencies

- The `aws` command line utility should be on your `PATH`

## TrampS3 Configuration

Tramps3 will use `/tmp/tramps3` to pull down data from s3 by default. This location can be modified by changing `TRAMPS3_TMP_S3_DIR`.

## Authorization

Tramps3 uses the `aws` command line utility to access s3, which has several options for authentication. One easy method is to run `aws configure` to create a credentials file, which will get picked up by subsequent `aws` calls.
