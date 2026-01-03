# Security Documentation

This document outlines the security measures implemented in this project.

## Overview

This project has been reviewed for security vulnerabilities and implements multiple layers of defense to protect against common attack vectors.

## Security Features

### 1. Input Validation

All user inputs are validated before processing:

#### HTTP API Endpoints (`dot.emacs.d/ide-server/server.py`)
- **Message Length Limits**: Messages are limited to 10,000 characters to prevent memory exhaustion
- **Path Length Limits**: File paths are limited to 1,000 characters
- **Query Length Limits**: Search queries are limited to 1,000 characters
- **Type Validation**: All inputs are checked for correct types (string, int, etc.)
- **Content-Length Validation**: HTTP request bodies are limited to 1MB to prevent large payload attacks

### 2. Path Traversal Prevention

The system implements multiple safeguards against path traversal attacks:

#### Safe Path Validation (`ContextManager._is_safe_path()`)
- **Absolute Path Resolution**: All paths are resolved to absolute paths
- **Dangerous Directory Blocking**: Access to system directories is blocked:
  - `/etc` - System configuration
  - `/sys` - System files
  - `/proc` - Process information
  - `/dev` - Device files
  - `/root` - Root user home
  - `/boot` - Boot files
- **Symlink Protection**: Symbolic links are resolved and validated

### 3. Resource Management

Multiple limits prevent resource exhaustion:

#### File Operations
- **Maximum File Size**: Files larger than 1MB are skipped during search
- **Maximum Search Results**: Search results limited to 20 items
- **Maximum Results Per File**: Each file contributes at most 3 results
- **Maximum Directory Depth**: Directory traversal limited to 5 levels deep
- **Maximum Files Processed**: Search processes at most 100 files
- **Maximum Context Directories**: Users can add at most 10 context directories

#### Line Processing
- **Line Length Limits**: Lines longer than 1,000 characters are skipped
- **Display Truncation**: Long result lines are truncated to 200 characters for display

### 4. File Type Filtering

Binary and potentially dangerous files are automatically excluded:

#### Excluded File Extensions
- **Compiled Files**: `.pyc`, `.pyo`, `.so`, `.o`, `.class`, `.jar`, `.war`, `.ear`
- **Executables**: `.exe`, `.dll`, `.bin`
- **Data Files**: `.dat`, `.db`, `.sqlite`, `.log`
- **Media Files**: `.jpg`, `.jpeg`, `.png`, `.gif`, `.bmp`, `.ico`, `.svg`
- **Archives**: `.zip`, `.tar`, `.gz`, `.bz2`, `.xz`, `.rar`, `.7z`
- **Documents**: `.pdf`, `.doc`, `.docx`, `.xls`, `.xlsx`, `.ppt`, `.pptx`
- **Hidden Files**: All files starting with `.` are skipped

### 5. Secure Subprocess Usage

All Python scripts use secure subprocess practices:

#### `docker/health-check.py`
- Uses `subprocess.run()` with explicit argument lists (not `shell=True`)
- Includes timeout protection (5 seconds) to prevent hanging
- Uses `capture_output=True` to safely handle output

### 6. Logging and Monitoring

The system uses Python's logging framework for security monitoring:

#### Logging Features
- **Structured Logging**: All security events are logged with context
- **Security Warnings**: Path validation failures are logged as warnings
- **Error Tracking**: All exceptions are logged with details
- **Audit Trail**: Context directory additions/removals are logged

### 7. API Key Security

#### Environment Variable Usage
- API keys are read from environment variables (never hardcoded)
- API key absence is handled gracefully with clear user feedback
- API keys are not logged or exposed in error messages

### 8. Thread Safety

#### Concurrent Access Protection
- Global state protected by threading locks
- Double-checked locking pattern for singleton initialization
- Thread-safe HTTP request handling

## Security Best Practices

### For Users

1. **API Keys**: Store API keys in environment variables, not in code
2. **Context Directories**: Only add trusted directories to context search
3. **Network Access**: Run the IDE server on localhost (127.0.0.1) only
4. **File Permissions**: Ensure proper file permissions on configuration files

### For Developers

1. **Input Validation**: Always validate user inputs before processing
2. **Path Operations**: Use the `_is_safe_path()` method for all file operations
3. **Resource Limits**: Respect defined constants for resource limits
4. **Logging**: Log security-relevant events at appropriate levels
5. **Error Handling**: Catch specific exceptions, avoid bare except clauses

## Known Limitations

1. **Single-user Design**: The server is designed for single-user local access
2. **No Authentication**: The HTTP API has no built-in authentication (use firewall/network isolation)
3. **No Encryption**: Communication is unencrypted (use SSH tunneling if needed)
4. **Rate Limiting**: No built-in rate limiting (intended for local use only)

## Reporting Security Issues

If you discover a security vulnerability, please report it by opening a GitHub issue with the label `security`. Do not disclose security vulnerabilities publicly until they have been addressed.

## Security Review History

### 2025-12-29 - Amazon Q Code Review Implementation
- Implemented comprehensive input validation
- Added path traversal protection
- Added resource exhaustion protection
- Added secure logging
- Improved error handling
- Added file type filtering
- Fixed shellcheck warnings in scripts

## References

- [OWASP Top 10](https://owasp.org/www-project-top-ten/)
- [Python Security Best Practices](https://python.readthedocs.io/en/latest/library/security_warnings.html)
- [CWE-22: Path Traversal](https://cwe.mitre.org/data/definitions/22.html)
- [CWE-400: Resource Exhaustion](https://cwe.mitre.org/data/definitions/400.html)
