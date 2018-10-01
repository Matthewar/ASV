# Filing Error
Errors related to the host's file operations.
The errors are contained in the [filing module](../../src/Manager/Filing.hs) file.
They are held in the type `FilingError`.

## Invalid Library Directory
When one or more library directories cannot be found.

### Constructor
`FilingError_InvalidLibDir`

### Data Contained
- `[String]`: List of directories in string form that cannot be found in the file system

### Error Message
"Invalid library directory: \<Directories\>"

## No File Found
Cannot find the file specified in the host file system.

### Constructor
`FilingError_NoFoundFile`

### Data Contained
- `String`: Relative path of file that cannot be found

### Error Message
"Unable to find file: \<file name\>"
